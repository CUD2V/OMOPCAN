# Estimate Incidence 
log4r::info(logger, "Estimate incidence") 

if(length(strat_var)==0){
  inc <- IncidencePrevalence::estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    # censorTable = "exclusion",
    interval = c("years", "overall"),
    outcomeWashout = Inf,
    repeatedEvents = FALSE,
    strata = list(),
    completeDatabaseIntervals = TRUE
  ) 
  
}else{
  
  # Add "overall" to denominator cohort settings
  attr(cdm$denominator, "cohort_set") <- attr(cdm$denominator, "cohort_set") |> 
    mutate(year = "overall")
  
  #incidence for the overall period
  inc_overall <- IncidencePrevalence::estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = c("overall"),
    outcomeWashout = Inf,
    repeatedEvents = FALSE,
    strata = as.list(strat_var),
    completeDatabaseIntervals = TRUE
  ) 
  
  #Get cohort ids for stratified/unstratified analyses
  ids_denom <- omopgenerics::settings(cdm$denominator) |> 
    pull(cohort_definition_id)
  ids_denom_str <- omopgenerics::settings(cdm$denominator) |> 
    filter(age_group %in% c("0 to 150","0 to 19", "20 to 49", "50 to 150")) |> 
    pull(cohort_definition_id)
  ids_denom_nostr <- setdiff(ids_denom, ids_denom_str)
  
  #Unstratified yearly incidence 
  inc_year_nostr <- IncidencePrevalence::estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    denominatorCohortId = ids_denom_nostr,
    outcomeTable = "outcome",
    interval = c("years"),
    outcomeWashout = Inf,
    repeatedEvents = FALSE,
    strata = list(),
    completeDatabaseIntervals = TRUE
  ) 
  
  # Stratified yearly incidence  -----------------------------------------------
  # Generate denominator for strata variables: split by years and add strata
  cdm <- omopgenerics::emptyCohortTable(cdm, name="denominator_all_years")
  yyears <- tryCatch({
    seq(clock::get_year(startdate), clock::get_year(enddate))
  },
  error = function(e) {
    message("clock::get_year() failed, using lubridate::year(). Error: ", conditionMessage(e))
    seq(lubridate::year(startdate), lubridate::year(enddate))
  })
  
  for(yi in yyears){
    
    # start and end dates for each year
    ssdate <- paste0(yi, "-01-01")
    eedate <- paste0(yi, "-12-31")
    
    # trim dates for each year
    cdm$denominator_years <- cdm$denominator |> 
      select(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date) |> 
      CohortConstructor::subsetCohorts(
        cohortId = ids_denom_str,
        name = "denominator_years"
      ) |> 
      CohortConstructor::trimToDateRange(
        dateRange = as.Date(c(ssdate, eedate)),
        name = "denominator_years"
      ) 
    
    # Add stratifications to denominator
    cdm$denominator_years <- cdm$denominator_years %>%
      select(c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date")) %>% 
      PatientProfiles::addCohortIntersectFlag(
        targetCohortTable = "conditions_st",
        window = list(c(-Inf, -1)),
        nameStyle = "{cohort_name}"
      ) %>% 
      compute(name="denominator_years", overwrite = TRUE, temporary = FALSE)
    
    if (any(grepl("smoker", colnames(cdm$denominator_years)))){
      cdm$denominator_years <- cdm$denominator_years |>
        mutate(
          smoking_status = case_when(
            former_current_smoker == 1 ~ "former_current_sometimes",
            non_smoker == 1 ~ "never",
            TRUE ~ "missing")
        ) |>  
        select(-former_current_smoker, -non_smoker) |>  
        compute(name="denominator_years", overwrite = TRUE, temporary = FALSE)
    }
    # Add settings
    attr(cdm$denominator_years, "cohort_set") <- attr(cdm$denominator_years, "cohort_set") |> 
      mutate(
        year = paste0(yi),
        cohort_name = paste0(cohort_name, "_", yi)
        )
    #Bind year denominators in the same object
    cdm <- cdm$denominator_all_years |> 
      omopgenerics::bind(
        cdm$denominator_years,
        name = "denominator_all_years"
      )
  }
  # Get stratification variables for incidence
  strat_var_inc = setdiff(
    colnames(cdm$denominator_all_years), 
    c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date")
  )
  # Compute incidence
  inc_years <- IncidencePrevalence::estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator_all_years",
    outcomeTable = "outcome",
    interval = c("overall"), #this translate to years since we've denominator_year
    outcomeWashout = Inf,
    repeatedEvents = FALSE,
    strata = as.list(strat_var_inc),
    completeDatabaseIntervals = TRUE
  ) 
  
  attr(inc_overall, "settings") <- attr(inc_overall, "settings") |>
    mutate(analysis_interval =  "overall")
  
  attr(inc_year_nostr, "settings") <- attr(inc_year_nostr, "settings") |>
    mutate(analysis_interval =  "years")
  
  attr(inc_years, "settings") <- attr(inc_years, "settings") |>
    mutate(analysis_interval =  "years")
  
  # Bind results
  inc <- omopgenerics::bind(inc_overall, inc_year_nostr, inc_years)

  # attr(inc, "settings") <- attr(inc, "settings") |>
  #   mutate(analysis_interval = ifelse(denominator_year=="overall", "overall", "years"))
}

inc %>% 
  omopgenerics::exportSummarisedResult(
    minCellCount = minimum_counts, 
    fileName =  paste0(db_name,"_incidence.csv"),
    path = output_folder
  )


# Standardise before exporting the age-specific cells
if (exists("executeAgeStandardisation") && executeAgeStandardisation) {
  log4r::info(logger, "Age-standardise incidence")
  exportStandardisedIncidence(
    incidenceResult = inc,
    standardPopulations = standardPopulations,
    minimumCounts = minimum_counts,
    standardisationAgeGroups = names(ageGroupList),
    fileName = paste0(db_name, "_incidence_age_standardised.csv"),
    path = output_folder
  )
}

  
