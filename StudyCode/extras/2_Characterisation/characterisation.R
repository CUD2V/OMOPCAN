log4r::info(logger, "Summarise charactertistics: demographics") 
cdm$outcome %>%
  CohortCharacteristics::summariseCharacteristics(
    # strata = list("sex", "age_gr", c("age_gr", "sex")),
    strata = list("sex", "age_gr", "age_gr_broad", c("age_gr", "sex"), c("age_gr_broad", "sex")),
    ageGroup = ageGroupList,
    otherVariables = c("age_gr_broad")
  ) %>%
  omopgenerics::exportSummarisedResult(
    minCellCount = minimum_counts,
    path = output_folder,
    fileName = paste0(omopgenerics::cdmName(cdm), "_summarise_characteristics_1demographics.csv")
  )



if(!isRegistry){
  #Add variables for characterisation
  otherVar <-  c("study_period")
  if("smoking_status" %in% strat_var){
    otherVar <- c(otherVar, "smoking_status")
    sm_id <- cdm$conditions_st |> 
      omopgenerics::settings() |> 
      filter(grepl("smoker", cohort_name)) |> 
      pull(cohort_definition_id)
    cdm$outcome <- cdm$outcome |> 
      PatientProfiles::addCohortIntersectFlag(
        targetCohortTable = "conditions_st",
        targetCohortId = sm_id,
        window = list(c(-Inf, -1)),
        nameStyle = "{cohort_name}"
      ) |> 
      mutate(
        smoking_status = case_when(
          former_current_smoker == 1 ~ "former_current_sometimes",
          non_smoker == 1 ~ "never",
          TRUE ~ "missing")
      ) |> 
      select(-any_of(c("former_current_smoker","non_smoker"))) |>
      # left_join(
      #   cdm$denominator|> filter(cohort_definition_id==1) |>  select(subject_id, smoking_status),
      #   by="subject_id"
      # ) %>% 
      compute(name="outcome", overwrite = TRUE, temporary = FALSE)
  }
  
  #Summarise characteristics
  if(!omopgenerics::isTableEmpty(cdm$conditions_all) & !omopgenerics::isTableEmpty(cdm$medications)){
    log4r::info(logger, "Summarise charactertistics: comorbidities & drugs")
    cdm$outcome |> 
      CohortCharacteristics::summariseCharacteristics(
        demographics = FALSE,
        # strata = list("sex", "age_gr", c("age_gr", "sex")),
        strata = list("sex", "age_gr", "age_gr_broad", c("age_gr", "sex"), c("age_gr_broad", "sex")),
        ageGroup = ageGroupList,
        cohortIntersectFlag = list(
          "Conditions prior and up to 365 days before index date" = list(
            targetCohortTable = "conditions_all",
            targetEndDate = "cohort_start_date",
            window = c(-Inf, -366)
          ),
          "Conditions 365 days prior to index date" = list(
            targetCohortTable = "conditions_all",
            targetEndDate = "cohort_start_date",
            window = c(-365, -1)
          ),
          "Conditions on index date" = list(
            targetCohortTable = "conditions_all",
            targetEndDate = "cohort_start_date",
            window = c(0, 0)
          ),
          "Medications 365 days prior to index date" = list(
            targetCohortTable = "medications",
            targetEndDate = "cohort_start_date",
            window = c(-365, -1)
          ),
          "Medications on index date" = list(
            targetCohortTable = "medications",
            targetEndDate = "cohort_start_date",
            window = c(0, 0)
          ),
          "Medications 1 to 90 days after index date" = list(
            targetCohortTable = "medications",
            targetEndDate = "cohort_start_date",
            window = c(1, 90)
          )
        ),
        otherVariables = otherVar
      )|> 
      omopgenerics::exportSummarisedResult(
        minCellCount = minimum_counts,
        path = output_folder,
        fileName = paste0(omopgenerics::cdmName(cdm),"_summarise_characteristics_2cond_drugs.csv")
      )
    
  }else if(!omopgenerics::isTableEmpty(cdm$conditions_all)){
    log4r::info(logger, "Summarise charactertistics: comorbidities")
    cdm$outcome %>%
      CohortCharacteristics::summariseCharacteristics(
        demographics = FALSE,
        # strata = list("sex", "age_gr", c("age_gr", "sex")),
        strata = list("sex", "age_gr", "age_gr_broad", c("age_gr", "sex"), c("age_gr_broad", "sex")),
        ageGroup = ageGroupList,
        cohortIntersectFlag = list(
          "Conditions prior and up to 365 days before index date" = list(
            targetCohortTable = "conditions_all",
            targetEndDate = "cohort_start_date",
            window = c(-Inf, -366)
          ),
          "Conditions 365 days prior to index date" = list(
            targetCohortTable = "conditions_all",
            targetEndDate = "cohort_start_date",
            window = c(-365, -1)
          ),
          "Conditions on index date" = list(
            targetCohortTable = "conditions_all",
            targetEndDate = "cohort_start_date",
            window = c(0, 0)
          )
        ),
        otherVariables = otherVar
      ) %>% 
      omopgenerics::exportSummarisedResult(
        minCellCount = minimum_counts,
        path = output_folder,
        fileName = paste0(omopgenerics::cdmName(cdm),"_summarise_characteristics_2conditions.csv")
      )
  }else if(!omopgenerics::isTableEmpty(cdm$medications)){
    log4r::info(logger, "Summarise charactertistics: medications") 
    cdm$outcome %>%
      CohortCharacteristics::summariseCharacteristics(
        demographics = FALSE,
        # strata = list("sex", "age_gr", c("age_gr", "sex")),
        strata = list("sex", "age_gr", "age_gr_broad", c("age_gr", "sex"), c("age_gr_broad", "sex")),
        ageGroup = ageGroupList,
        cohortIntersectFlag = list(
          "Medications 365 days prior to index date" = list(
            targetCohortTable = "medications",
            targetEndDate = "cohort_start_date",
            window = c(-365, -1)
          ),
          "Medications on index date" = list(
            targetCohortTable = "medications",
            targetEndDate = "cohort_start_date",
            window = c(0, 0)
          ),
          "Medications 1 to 90 days after index date" = list(
            targetCohortTable = "medications",
            targetEndDate = "cohort_start_date",
            window = c(1, 90)
          )
        )
      ) %>% 
      omopgenerics::exportSummarisedResult(
        minCellCount = minimum_counts,
        path = output_folder,
        fileName = paste0(omopgenerics::cdmName(cdm),"_summarise_characteristics_2drugs.csv")
        )
  }
  
  log4r::info(logger, "Large scale characterisation: conditions")
  cdm$outcome %>% 
    CohortCharacteristics::summariseLargeScaleCharacteristics(
      window = list(
        # Shared windows for both condition_occurrence and drug_era
        c(-Inf, -366),   # >1y before
        c(-365, -1),    # 1y-1d before
        c(0, 0)         # Index date
      ),
      eventInWindow = "condition_occurrence",  # Only conditions
      includeSource = TRUE,
      minimumFrequency = 0.1
    ) %>% 
    omopgenerics::exportSummarisedResult(
      minCellCount = minimum_counts,
      path = output_folder,
      fileName = paste0(omopgenerics::cdmName(cdm),"_summarise_large_scale_characteristics_conditions.csv")
    )
    
  log4r::info(logger, "Large scale characterisation: medications")
  cdm$outcome %>% 
    CohortCharacteristics::summariseLargeScaleCharacteristics(
      window = list(
        # Shared windows for both condition_occurrence and drug_era
        c(-365, -1),    # 1y-1d before
        c(0, 0),        # Index date
        # Additional windows for drug_era only
        c(1, 90)        # 1-90d after
      ),
      episodeInWindow = "drug_era",    # Only drug eras
      includeSource = TRUE,
      minimumFrequency = 0.1
    ) %>% 
    omopgenerics::exportSummarisedResult(
      minCellCount = minimum_counts,
      path = output_folder,
      fileName = paste0(omopgenerics::cdmName(cdm),"_summarise_large_scale_characteristics_drugs.csv")
      )
}else{
  # Registries characterization of medications on index date and 1-90 days after
  if(!omopgenerics::isTableEmpty(cdm$medications)){
    log4r::info(logger,"Summarise registry medications: index date and 1 to 90 days after")
    cdm$outcome %>%
      CohortCharacteristics::summariseCharacteristics(
        demographics = FALSE,
        # strata = list("sex", "age_gr", c("age_gr", "sex")),
        strata = list("sex", "age_gr", "age_gr_broad", c("age_gr", "sex"), c("age_gr_broad", "sex")),
        ageGroup = ageGroupList,
        cohortIntersectFlag = list(
          "Medications on index date" = list(
            targetCohortTable = "medications",
            targetEndDate = "cohort_start_date",
            window = c(0, 0)
          ),
          "Medications 1 to 90 days after index date" = list(
            targetCohortTable = "medications",
            targetEndDate = "cohort_start_date",
            window = c(1, 90)
          )
        )
      ) %>%
      omopgenerics::exportSummarisedResult(
        minCellCount = minimum_counts,
        path = output_folder,
        fileName = paste0(omopgenerics::cdmName(cdm),"_summarise_characteristics_2drugs.csv")
      )
    
    log4r::info(logger,"Large scale registry medication characterisation")
    cdm$outcome %>%
      CohortCharacteristics::summariseLargeScaleCharacteristics(
        window = list(
          c(0, 0),
          c(1, 90)
        ),
        episodeInWindow = "drug_era",
        includeSource = TRUE,
        minimumFrequency = 0.1
      ) %>%
      omopgenerics::exportSummarisedResult(
        minCellCount = minimum_counts,
        path = output_folder,
        fileName = paste0(omopgenerics::cdmName(cdm), "_summarise_large_scale_characteristics_drugs.csv")
      )
  }else{
    log4r::info(logger,"No medication records available for registry characterisation")
  }
}
