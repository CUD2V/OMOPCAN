# Helpers for direct age standardisation of incidence and survival.
# Requires EpiStandard >= 0.2.0 and CohortSurvival >= 1.1.2.

readStandardPopulations <- function(
    path = here::here(
      "extras",
      "Standardisation",
      "StandardPopulations"
    )) {
  files <- c(
    ESP2011_2030 = "ESP2011_2030.csv",
    SEGI = "SEGI.csv",
    WSP_2000_2025 = "WSP_2000_2025.csv"
  )

  populations <- lapply(files, function(file) {
    x <- utils::read.csv(
      file.path(path, file),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    names(x)[1:2] <- c("age_group", "population")
    x$age_group <- gsub("\\s+", "", trimws(as.character(x$age_group)))
    x$population <- as.numeric(gsub(",", "", as.character(x$population)))
    x <- x[!grepl("^all", x$age_group, ignore.case = TRUE), , drop = FALSE]

    lower <- as.numeric(sub("^([0-9]+).*", "\\1", x$age_group))
    upper <- lower
    is_range <- grepl("-", x$age_group, fixed = TRUE)
    upper[is_range] <- as.numeric(sub(".*-", "", x$age_group[is_range]))
    upper[grepl("+", x$age_group, fixed = TRUE)] <- 150

    tibble(
      age_group = x$age_group,
      age_lower = lower,
      age_upper = upper,
      population = x$population
    )
  })

  populations
}

parseStudyAgeGroups <- function(ageGroups) {
  ageGroups <- unique(as.character(ageGroups))
  ageGroups <- ageGroups[
    !is.na(ageGroups) &
      !grepl("overall|all ages|0 to 150", ageGroups, ignore.case = TRUE)
  ]

  lower <- as.numeric(stringr::str_extract(ageGroups, "[0-9]+"))
  all_numbers <- stringr::str_extract_all(ageGroups, "[0-9]+")
  upper <- vapply(seq_along(ageGroups), function(i) {
    values <- as.numeric(all_numbers[[i]])
    if (grepl("\\+|over", ageGroups[[i]], ignore.case = TRUE)) {
      150
    } else if (length(values) >= 2) {
      values[[2]]
    } else {
      values[[1]]
    }
  }, numeric(1))

  tibble(
    denominator_age_group = ageGroups,
    age_lower = lower,
    age_upper = upper
  ) |>
    filter(
      !is.na(.data$age_lower),
      !is.na(.data$age_upper),
      .data$age_lower <= .data$age_upper
    )
}

commonAgeGroups <- function(studyBands, reference) {
  study_boundaries <- unique(c(
    studyBands$age_lower,
    studyBands$age_upper + 1
  ))
  reference_boundaries <- unique(c(
    reference$age_lower,
    reference$age_upper + 1
  ))
  boundaries <- sort(intersect(study_boundaries, reference_boundaries))
  boundaries <- sort(unique(c(0, boundaries, 151)))

  tibble(
    common_lower = head(boundaries, -1),
    common_upper = tail(boundaries, -1) - 1
  ) |>
    filter(.data$common_lower <= .data$common_upper) |>
    mutate(
      standard_age_group = if_else(
        .data$common_upper == 150,
        paste0(.data$common_lower, "+"),
        paste0(.data$common_lower, "-", .data$common_upper)
      )
    )
}

standardiseIncidence <- function(
    incidenceResult,
    standardPopulations,
    minimumCounts = 10,
    standardisationAgeGroups = NULL) {
  if (!requireNamespace("EpiStandard", quietly = TRUE)) {
    stop("EpiStandard >= 0.2.0 is required for incidence standardisation.")
  }

  incidence <- IncidencePrevalence::asIncidenceResult(incidenceResult)
  if (!is.null(standardisationAgeGroups)) {
    incidence <- incidence |>
      filter(
        .data$denominator_age_group %in% standardisationAgeGroups
      )
  }
  if (!"person_years" %in% names(incidence)) {
    if (!"person_days" %in% names(incidence)) {
      stop("The incidence result has neither person_years nor person_days.")
    }
    incidence <- incidence |>
      mutate(person_years = .data$person_days / 365.25)
  }

  required <- c(
    "denominator_age_group",
    "outcome_count",
    "person_years"
  )
  missing_required <- setdiff(required, names(incidence))
  if (length(missing_required) > 0) {
    stop(
      "Missing columns in incidence result: ",
      paste(missing_required, collapse = ", ")
    )
  }

  studyBands <- parseStudyAgeGroups(incidence$denominator_age_group)
  if (nrow(studyBands) == 0) {
    stop("No age-specific incidence results were found.")
  }

  measure_columns <- c(
    "denominator_count", "outcome_count", "person_days", "person_years",
    "incidence_100000_pys", "incidence_100000_pys_95CI_lower",
    "incidence_100000_pys_95CI_upper", "incidence_proportion",
    "incidence_proportion_95CI_lower", "incidence_proportion_95CI_upper"
  )
  identity_columns <- c(
    "result_id", "denominator_cohort_id", "denominator_cohort_name",
    "denominator_age_group"
  )
  strata_columns <- setdiff(
    names(incidence),
    c(measure_columns, identity_columns)
  )

  results <- lapply(names(standardPopulations), function(standardName) {
    reference <- standardPopulations[[standardName]]
    common <- commonAgeGroups(studyBands, reference)

    study_map <- studyBands |>
      cross_join(common) |>
      filter(
        .data$age_lower >= .data$common_lower,
        .data$age_upper <= .data$common_upper
      ) |>
      select(
        .data$denominator_age_group,
        .data$standard_age_group
      )

    reference_aggregated <- reference |>
      cross_join(common) |>
      filter(
        .data$age_lower >= .data$common_lower,
        .data$age_upper <= .data$common_upper
      ) |>
      group_by(.data$standard_age_group) |>
      summarise(
        population = sum(.data$population),
        .groups = "drop"
      )

    data_aggregated <- incidence |>
      inner_join(study_map, by = "denominator_age_group") |>
      group_by(
        across(all_of(
          c(strata_columns, "standard_age_group")
        ))
      ) |>
      summarise(
        outcome_count = sum(.data$outcome_count, na.rm = TRUE),
        person_years = sum(.data$person_years, na.rm = TRUE),
        .groups = "drop"
      )

    complete_data <- data_aggregated |>
      group_by(
        across(all_of(strata_columns))
      ) |>
      filter(
        n() == nrow(reference_aggregated),
        all(.data$person_years > 0)
      ) |>
      ungroup()

    if (nrow(complete_data) == 0) {
      return(NULL)
    }

    standardised <- EpiStandard::directlyStandardiseRates(
      data = complete_data,
      event = "outcome_count",
      denominator = "person_years",
      age = "standard_age_group",
      pop = "population",
      strata = strata_columns,
      addMissingGroups = FALSE,
      refdata = reference_aggregated
    ) |>
      mutate(
        standard_population = standardName,
        suppressed = .data$outcome_count < minimumCounts
      )

    rate_columns <- names(standardised)[
      grepl("rate", names(standardised), ignore.case = TRUE)
    ]
    standardised |>
      mutate(
        outcome_count = if_else(
          .data$suppressed,
          NA_real_,
          as.numeric(.data$outcome_count)
        ),
        across(
          all_of(rate_columns),
          ~if_else(.data$suppressed, NA_real_, as.numeric(.x))
        )
      )
  })

  bind_rows(results)
}

exportStandardisedIncidence <- function(
    incidenceResult,
    standardPopulations,
    minimumCounts,
    standardisationAgeGroups = NULL,
    fileName,
    path) {
  standardised <- standardiseIncidence(
    incidenceResult = incidenceResult,
    standardPopulations = standardPopulations,
    minimumCounts = minimumCounts,
    standardisationAgeGroups = standardisationAgeGroups
  )
  utils::write.csv(
    standardised,
    file = file.path(path, fileName),
    row.names = FALSE,
    na = ""
  )
  invisible(standardised)
}

addScopeWeight <- function(
    table,
    ageVariable,
    populationVariable,
    scopeVariables,
    weightVariable,
    tablePrefix) {
  groupVariables <- c("cohort_definition_id", scopeVariables)
  nAgeVariable <- paste0(tablePrefix, "_n_age")
  nTotalVariable <- paste0(tablePrefix, "_n_total")
  observedPopulationVariable <- paste0(
    tablePrefix,
    "_observed_population"
  )
  totalPopulationVariable <- paste0(  tablePrefix,  "_total_population" )

  ageCounts <- table |>
    filter(!is.na(.data[[ageVariable]])) |>
    group_by(
      across(all_of(c(groupVariables, ageVariable)))
    ) |>
    summarise(
      !!nAgeVariable := n(),
      !!observedPopulationVariable :=
        max(.data[[populationVariable]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    compute(name = paste0(tablePrefix, "_age"),,temporary = FALSE, overwrite = TRUE)
   
  totals <- ageCounts |>
    group_by(across(all_of(groupVariables))) |>
    summarise(
      !!nTotalVariable := sum(.data[[nAgeVariable]]),
      !!totalPopulationVariable :=
        sum(.data[[observedPopulationVariable]]),
      .groups = "drop"
    ) |>
    compute(name = paste0(tablePrefix, "_total"),temporary = FALSE, overwrite = TRUE)
 

  table |>
    left_join(ageCounts, by = c(groupVariables, ageVariable)) |>
    left_join(totals,  by = groupVariables) |>
    mutate(
      !!weightVariable := if_else(
        !is.na(.data[[nAgeVariable]]) &
          .data[[totalPopulationVariable]] > 0,
        .data[[populationVariable]] /
          .data[[totalPopulationVariable]] *
          .data[[nTotalVariable]] / .data[[nAgeVariable]],
        NA_real_
      )
    ) |>
    select(-all_of(c( nAgeVariable,  nTotalVariable, observedPopulationVariable, totalPopulationVariable))) 
}

addSurvivalStandardWeights <- function( cdm,
                                        tableName = "outcome_surv",
                                        outputName = "outcome_surv_standardised",
                                        standardPopulations) {
  table <- cdm[[tableName]]
  cohortSetRef <- attr(table, "cohort_set")
  cohortAttritionRef <- attr(table, "cohort_attrition")
  cohortCodelistRef <- attr(table, "cohort_codelist")

  for (standardName in names(standardPopulations)) {
    reference <- standardPopulations[[standardName]]
    prefix <- tolower(gsub("[^A-Za-z0-9]", "", standardName))
    ageVariable <- paste0("std_age_", prefix)
    populationVariable <- paste0("std_pop_", prefix)

    ageCases <- lapply(seq_len(nrow(reference)), function(i) {
      rlang::expr(
        .data$age >= !!reference$age_lower[[i]] &
          .data$age <= !!reference$age_upper[[i]] ~
          !!reference$age_group[[i]]
      )
    })
    ageSymbol <- rlang::sym(ageVariable)
    populationCases <- lapply(seq_len(nrow(reference)), function(i) {
      rlang::expr(
        !!ageSymbol == !!reference$age_group[[i]] ~
          !!reference$population[[i]]
      )
    })

    table <- table |>
      mutate(
        !!ageVariable := case_when(
          !!!ageCases,
          TRUE ~ NA_character_
        ),
        !!populationVariable := case_when(
          !!!populationCases,
          TRUE ~ NA_real_
        )
      )

    scopes <- list( overall = character(), sex = "sex", study_period = "study_period"  )
    
    for (scopeName in names(scopes)) {
      weightVariable <- paste0("std_weight_",prefix,"_",scopeName)
      table <- addScopeWeight(
        table = table,
        ageVariable = ageVariable,
        populationVariable = populationVariable,
        scopeVariables = scopes[[scopeName]],
        weightVariable = weightVariable,
        tablePrefix = paste0("sw_", prefix, "_", scopeName)
      )
    }

    table <- table |>
      compute( name = paste0("surv_weights_", prefix), temporary = FALSE, overwrite = TRUE)
  }

  cdm[[outputName]] <- table |>
    compute(
      name = outputName,
      temporary = FALSE,
      overwrite = TRUE
    )
  cdm[[outputName]] <- omopgenerics::newCohortTable(
    table = cdm[[outputName]],
    cohortSetRef = cohortSetRef,
    cohortAttritionRef = cohortAttritionRef,
    cohortCodelistRef = cohortCodelistRef,
    .softValidation = TRUE
  )
  cdm
}

estimateStandardisedSurvival <- function(
    cdm,
    # targetCohortId,
    standardPopulations,
    tableName = "outcome_surv_standardised") {
  
  scopes <- list(
    overall = NULL,
    sex = list("sex"),
    study_period = list("study_period")
  )
  results <- list()

  for (standardName in names(standardPopulations)) {
    prefix <- tolower(gsub("[^A-Za-z0-9]", "", standardName))

    for (scopeName in names(scopes)) {
      
      weightVariable <- paste0( "std_weight_",  prefix,  "_",  scopeName )
      targetName <- substr(paste0("surv_std_", prefix, "_", scopeName),1, 60 )
      
      message("Computing age-standarisation: ", targetName)
      cohortSetRef <- attr(cdm[[tableName]], "cohort_set")
      cohortAttritionRef <- attr(cdm[[tableName]], "cohort_attrition")
      cohortCodelistRef <- attr(cdm[[tableName]], "cohort_codelist")
      cdm[[targetName]] <- cdm[[tableName]] |>
        filter(!is.na(.data[[weightVariable]])) |>
        compute( name = targetName, temporary = FALSE, overwrite = TRUE)
      cdm[[targetName]] <- omopgenerics::newCohortTable(
        table = cdm[[targetName]],
        cohortSetRef = cohortSetRef,
        cohortAttritionRef = cohortAttritionRef,
        cohortCodelistRef = cohortCodelistRef,
        .softValidation = TRUE
      )
      
      # availableIds <- cdm[[targetName]] |>
      #   distinct(.data$cohort_definition_id) |>
      #   pull("cohort_definition_id")
      # ids <- intersect(targetCohortId, availableIds)
      # if (length(ids) == 0) {
      #   next
      # }

      # for(targetId in ids){
      result <- CohortSurvival::estimateSingleEventSurvival(
        cdm = cdm,
        targetCohortTable = targetName,
        # targetCohortId = targetId,
        outcomeCohortTable = "death_cohort",
        censorOnCohortExit = FALSE,
        weight = weightVariable,
        minimumSurvivalDays = 1,
        estimateGap = 365,
        eventGap = 365,
        strata = scopes[[scopeName]]
      )

      if (scopeName != "overall") {
        result <- result |>
          filter(.data$strata_name == scopeName)
      }

      resultName <- paste( standardName,scopeName,sep = "_")
      # resultName <- paste( standardName,scopeName,targetId,sep = "_")
      
      results[[resultName]] <- result
      # }
    }
  }

  results
}

exportStandardisedSurvival <- function(
    results,
    minimumCounts,
    fileName,
    path) {
  
  surv_all <- NULL
  for(rname in names(results)){
    surv_i <- results[[rname]] |> 
      CohortSurvival::asSurvivalResult() |> 
      mutate(std_name = rname)
    if(is.null(surv_all)){
      surv_all <- surv_i
    }else{
      surv_all <- bind_rows(surv_all, surv_i) 
    }
  }
  
  utils::write.csv(
    surv_all,
    file = file.path(path, fileName),
    row.names = FALSE,
    na = ""
  )
}
