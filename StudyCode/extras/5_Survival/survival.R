if(cdm$death %>% head(5) %>% count() %>% pull("n") > 0) {
  
  survival_strat_var <- c()
  
  if(!omopgenerics::isTableEmpty(cdm$conditions_st) & !isRegistry){
    
    log4r::info(logger, "Add stratification variables to outcome cohort") 
    cdm$outcome_surv <- cdm$outcome_surv |> 
      PatientProfiles::addCohortIntersectFlag(
        targetCohortTable = "conditions_st",
        window = list(c(-Inf, -1)),
        nameStyle = "{cohort_name}"
      ) %>% 
      compute(name="outcome_surv", overwrite = TRUE, temporary = FALSE)
    
    if (any(grepl("smoker", colnames(cdm$outcome_surv)))){
      cdm$outcome_surv <- cdm$outcome_surv |>
        mutate(
          smoking_status = case_when(
            former_current_smoker == 1 ~ "former_current_sometimes",
            non_smoker == 1 ~ "never",
            TRUE ~ "missing")
        ) |>  
        select(-any_of(c("former_current_smoker", "non_smoker"))) |>  
        compute(name="outcome_surv", overwrite = TRUE, temporary = FALSE)
    }
    
    for(stratif in strat_var){
      nstrat <- cdm$outcome_surv %>%
        dplyr::filter(!is.na(.data[[stratif]])) %>%
        dplyr::count() %>%
        dplyr::pull(n) 
      if(nstrat > minimum_counts){ survival_strat_var <- c(survival_strat_var, stratif)}
    }
  }
  
  #Add year
  cdm$outcome_surv <- tryCatch({
    cdm$outcome_surv |>
      mutate(year = as.character(clock::get_year(cohort_start_date))) |>
      compute(name = "outcome_surv", overwrite = TRUE, temporary = FALSE)
  },
  error = function(e) {
    message("clock::get_year() failed, using lubridate::year(). Error: ", conditionMessage(e))
    cdm$outcome_surv |>
      mutate(year = as.character(lubridate::year(cohort_start_date))) |>
      compute(name = "outcome_surv", overwrite = TRUE, temporary = FALSE)
  })
  
  survival_strata <- list(
    "study_period",
    "age_gr",
    "age_gr_broad",
    "sex",
    "year",
    c("age_gr", "sex"),
    c("age_gr_broad", "sex"),
    c("study_period", "sex"),
    c("study_period", "age_gr"),
    c("study_period", "age_gr_broad"),
    c("study_period", "age_gr", "sex"),
    c("study_period", "age_gr_broad", "sex")
  )
  
  if (length(survival_strat_var) > 0) {
    survival_strata <- c(
      survival_strata,
      as.list(survival_strat_var),
      lapply(
        survival_strat_var,
        function(x) c("study_period", x)
      ),
      lapply(
        survival_strat_var,
        function(x) c("age_gr", x)
      ),
      lapply(
        survival_strat_var,
        function(x) c("sex", x)
      ),
      lapply(
        survival_strat_var,
        function(x) c("age_gr", "sex", x)
      ),
      lapply(
        survival_strat_var,
        function(x) c("age_gr", "sex", "study_period", x)
      )
    )
  }

  # Create death cohort with all relevant columns
  log4r::info(logger, "Generate death cohort") 
  cdm$death_cohort <- CohortConstructor::deathCohort(cdm,"death_cohort")
  
  log4r::info(logger, "Estimate crude single-event survival") 
  CohortSurvival::estimateSingleEventSurvival(
    cdm,
    targetCohortTable = "outcome_surv",
    outcomeCohortTable = "death_cohort",
    censorOnCohortExit = TRUE,
    minimumSurvivalDays = 1,
    estimateGap = 365,
    eventGap = 365,
    strata =survival_strata
  )%>%
  omopgenerics::exportSummarisedResult(
    minCellCount = minimum_counts,
    fileName =  paste0(db_name,"_survival.csv"),
    path = output_folder
  )
  
  if (exists("executeAgeStandardisation") && executeAgeStandardisation) {
    log4r::info(logger, paste0("Age-standardise survival: overall, sex and study period"))
    cdm <- addSurvivalStandardWeights(
      cdm = cdm,
      tableName = "outcome_surv",
      outputName = "outcome_surv_standardised",
      standardPopulations = standardPopulations
    )
    survivalAgeStandardised <- estimateStandardisedSurvival(
      cdm = cdm,
      # targetCohortId = ids_surv,
      standardPopulations = standardPopulations,
      tableName = "outcome_surv_standardised"
    )

    if(length(survivalAgeStandardised)>0){
      exportStandardisedSurvival(
        results = survivalAgeStandardised,
        minimumCounts = minimum_counts,
        fileName = paste0(db_name, "_survival_age_standardised.csv"),
        path = output_folder
      )
    }else{
      log4r::info(logger,"No age-standardised survival results available")
    }
  }

}
  


