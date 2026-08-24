#Initialize variables
strat_var = c()
cdm <- omopgenerics::emptyCohortTable(cdm, name="conditions_st", overwrite = TRUE)
cdm <- omopgenerics::emptyCohortTable(cdm, name="outcome", overwrite = TRUE)

# Instantiate outome subsetting from outcome_all
log4r::info(logger, paste0(paste0("Cancers: "), paste(toupper(cancer_types_gr$cohort_name), collapse = ", ")))
log4r::info(logger, "Create outcome cohort")
cdm$outcome <- cdm$outcome_all |> 
  CohortConstructor::subsetCohorts( 
    cohortId = cancer_types_gr$cohort_definition_id,
    name = "outcome" 
  )

#Filter out exlusions
log4r::info(logger, "Apply exclusion criteria to selected cancer cohorts")
##prior observation/study period/ age/ sex
if(daysPriorObservation != 0 ){
  cdm$outcome <- cdm$outcome |> 
    CohortConstructor::requirePriorObservation(
      minPriorObservation = daysPriorObservation
    )
}
cdm$outcome <- cdm$outcome |> 
  CohortConstructor::requireInDateRange(
    dateRange = as.Date(c(startdate, enddate))
  ) |> 
  CohortConstructor::requireAge(
    ageRange = c(0,150)
  )
# cdm$outcome <- cdm$outcome |> 
#   CohortConstructor::requirePriorObservation(
#     minPriorObservation = daysPriorObservation
#   ) |> 
#   CohortConstructor::requireInDateRange(
#     dateRange = as.Date(c(startdate, enddate))
#   ) |> 
#   CohortConstructor::requireAge(
#     ageRange = c(0,150)
#   )

if (cancer_group == "male") {
  cdm$outcome <- cdm$outcome |>
    CohortConstructor::requireSex(sex = "Male")
}

if (cancer_group == "female") {
  cdm$outcome <- cdm$outcome |>
    CohortConstructor::requireSex(sex = "Female")
}

#Add demographics
log4r::info(logger, "Add demographics")
cdm$outcome <- cdm$outcome |> 
  PatientProfiles::addDemographics(ageGroup = ageGroupList) |> 
  rename(age_gr = age_group) |> 
  PatientProfiles::addAge(ageGroup = ageGroupListBroad) |> 
  rename(age_gr_broad = age_group)
  # mutate(
  #   age_gr_broad = case_when(
  #     age >= 0 & age <= 19 ~ "<20",
  #     age >= 20 & age <= 49 ~ "20 to 49",
  #     age >= 50 & age <= 150 ~ "50 +",
  #     TRUE ~ "None"
  #   )
  # )

# Add study period windows
cdm$outcome <- cdm$outcome |> 
  mutate(study_period = case_when(
      cohort_start_date >= as.Date("2000-01-01") & cohort_start_date < as.Date("2005-01-01") ~ "2000-2004",
      cohort_start_date >= as.Date("2005-01-01") & cohort_start_date < as.Date("2010-01-01") ~ "2005-2009",
      cohort_start_date >= as.Date("2010-01-01") & cohort_start_date < as.Date("2015-01-01") ~ "2010-2014",
      cohort_start_date >= as.Date("2015-01-01") & cohort_start_date < as.Date("2020-01-01") ~ "2015-2019",
      cohort_start_date >= as.Date("2020-01-01") & cohort_start_date < as.Date("2025-01-01") ~ "2020-2024",
      TRUE ~ "Outside Range" # Dates outside the specified range
    )
  ) |> 
  compute(name = "outcome", temporary = FALSE, overwrite = TRUE)

if(nrow(covariable_counts)>0 & !isRegistry){
  # Select stratifications depending on cancer group-----------------------------
  log4r::info(logger, paste0("Group stratifications: ", paste(toupper(cancer_types[[cancer_group]]$stratification), collapse = ", ")))
  stratif_gr <- covariable_counts |> 
    filter(cohort_name %in% cancer_types[[cancer_group]]$stratification) |> 
    filter(number_subjects > minimum_counts)
  
  if ("tobacco" %in% cancer_types[[cancer_group]]$stratification & any(grepl("smoker", covariable_counts$cohort_name))){
    n_smoker <-  covariable_counts |>
      dplyr::filter(grepl("smoker", cohort_name)) |>
      dplyr::summarise(n = sum(number_subjects, na.rm = TRUE)) |>
      dplyr::pull(n)
    
    if(n_smoker>minimum_counts){
      stratif_gr <- stratif_gr |> 
        union_all(covariable_counts |> filter(grepl("smoker",cohort_name)))
    }
  }
  
  if (nrow(stratif_gr)>0){
    log4r::info(logger, paste0("Stratification variables: ", paste(toupper(stratif_gr$cohort_name), collapse = ", ")))
    cdm$conditions_st <- cdm$conditions_all |> 
      CohortConstructor::subsetCohorts( 
        cohortId = stratif_gr$cohort_definition_id,
        name = "conditions_st" 
      )
    
    # Add stratifications to denominator
    log4r::info(logger, "Add stratifications to denominator cohort")
    cdm$denominator <- cdm$denominator |>
      select(c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date")) |> 
      PatientProfiles::addCohortIntersectFlag(
        targetCohortTable = "conditions_st",
        window = list(c(-Inf, -1)),
        nameStyle = "{cohort_name}"
      ) |> 
      compute(name="denominator", overwrite = TRUE, temporary = FALSE)
    
    if (any(grepl("smoker", colnames(cdm$denominator)))){
      cdm$denominator <- cdm$denominator |> 
        mutate(
          smoking_status = case_when(
            former_current_smoker == 1 ~ "former_current_sometimes",
            non_smoker == 1 ~ "never",
            TRUE ~ "missing")
        ) |> 
        select(-former_current_smoker, -non_smoker) |> 
        compute(name="denominator", overwrite = TRUE, temporary = FALSE)
    } 
    
    strat_var = setdiff(
      colnames(cdm$denominator), 
      c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date")
    )
  } 
}

## Survival: ------------------------------------------------------------------
#1) Exclude subjects with anymalignancy prior to index date
cdm$outcome_surv <- cdm$outcome |>
  CohortConstructor::requireCohortIntersect(
    targetCohortTable = "exclusion",
    window = list(c(-Inf, -1)),
    intersections = 0, 
    name = "outcome_surv"
  )

#2) Exclude multiple outcomes same date 
cdm$outcome_surv <- cdm$outcome_surv |> 
  CohortConstructor::requireCohortIntersect(
    targetCohortTable = "exclusion",
    window = list(c(0, 0)),
    intersections = 1,
    name = "outcome_surv"
  )
# ------------------------------------------------------------------------------
# Export cohort counts and attrition
log4r::info(logger, "Export cohort counts and attrition tables")
attritions <- list()
counts <- list()
cohorts <- c( "outcome", "outcome_surv", "exclusion" ) 
if(!isRegistry){ cohorts <- c(cohorts, "denominator", "conditions_all", "conditions_st", "medications") }

for (nm in cohorts) {
  if (omopgenerics::isTableEmpty(cdm[[nm]])){
    log4r::info(logger, paste0(nm, " table is empty"))
  }else{
    counts[[nm]] <- CohortCharacteristics::summariseCohortCount(cdm[[nm]])
    attritions[[nm]] <- CohortCharacteristics::summariseCohortAttrition(cdm[[nm]])
  }
}

counts <- omopgenerics::bind(counts)
counts |> 
  omopgenerics::exportSummarisedResult(
    minCellCount = minimum_counts, 
    fileName =  paste0(db_name,"_summarise_cohort_count.csv"),
    path = output_folder
  )

attritions <- omopgenerics::bind(attritions)
attritions |> 
  omopgenerics::exportSummarisedResult(
    minCellCount = minimum_counts, 
    fileName =  paste0(db_name,"_summarise_cohort_attrition.csv"),
    path = output_folder
  )
