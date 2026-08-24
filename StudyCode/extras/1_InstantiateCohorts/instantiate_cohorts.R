# Instantiate exclusion cohort -------------------------------------------------
log4r::info(logger, "Instantiate exclusion")
all_cancer_codes <- CodelistGenerator::codesFromCohort(
  path = here::here("extras", "1_InstantiateCohorts", "Cohorts", "AllCancers") ,
  cdm = cdm
)
# any prior history of malignancy (more malignancy concepts)
codelistExclusion <- CodelistGenerator::codesFromConceptSet(
  here::here("extras", "1_InstantiateCohorts", "Cohorts", "Exclusion"), 
  cdm
)
#Combine exclusion malignancy codes with cancer codes (except non-melanoma skin)
# codelistExclusion <- list(
#   Reduce(
#     union_all,
#     c(all_cancer_codes[!names(all_cancer_codes) %in%  c("nmsc_broad", "nmsc_narrow")],
#       codelistExclusion
#     )
#   )
# )
# names(codelistExclusion) <- "anymalignancy"
all_codes <- unlist(all_cancer_codes[!names(all_cancer_codes) %in% c("nmsc_broad", "nmsc_narrow")], use.names = FALSE)
all_codes <- unique(c(all_codes, unlist(codelistExclusion,use.names = FALSE)))
codelistExclusion <- list("anymalignancy" = all_codes)

#Create exclusion (anymalignancy) cohort
cdm <- CDMConnector::generateConceptCohortSet(
  cdm = cdm,
  conceptSet = codelistExclusion,
  name = "exclusion",
  limit = "all",
  overwrite = TRUE
)

# Generate cancer cohorts ------------------------------------------------------
if (!grepl("NAJS", db_name, ignore.case=TRUE)){
  #Instantiate all cancers (including colorectal)
  log4r::info(logger, "Instantiate cancer cohorts")
  all_cancer_concepts <- CDMConnector::readCohortSet(
    path = here::here("extras", "1_InstantiateCohorts", "Cohorts", "AllCancers" )
  )
  cdm <- CDMConnector::generateCohortSet(
    cdm,
    cohortSet = all_cancer_concepts,
    name = "outcome_all",
    overwrite = TRUE
  )
}else{
  #NAJS must consider only confirmed diagnoses (from registry)
  log4r::info(logger, "Consider confirmed diagnoses for NAJS")
  #Filter confirmed diagnoses
  cdm$conditions_registry = cdm$condition_occurrence |> 
    filter(condition_start_date >= startdate & condition_start_date <= enddate) |> #Filter study period
    filter(condition_status_concept_id ==32893 & condition_type_concept_id == 32879) |>  #Filter confirmed registry diagnosis
    compute(name = "conditions_registry", overwrite = TRUE)
  #Leave only exclusion records that are confirmed
  cdm$exclusion <- cdm$exclusion |> 
    CohortConstructor::requireTableIntersect(
      tableName = "conditions_registry",
      window = c(0,0),
      intersections = c(1,Inf),,
      targetStartDate = "condition_start_date",
      targetEndDate = "condition_start_date"
    ) |> 
    compute(name= "exclusion", overwrite=TRUE)
  #Instantiate all cancers
  log4r::info(logger, "Instantiate cancer cohorts")
  # #Create colorectal code list
  # all_cancer_codes[["colorectal"]] <- Reduce(
  #   union_all, 
  #   all_cancer_codes[names(all_cancer_codes) %in%  c("colon", "rectum")]
  # )
  #Create concept cohort
  cdm$outcome_all <- CohortConstructor::conceptCohort(
    cdm,
    conceptSet = all_cancer_codes,
    name = "outcome_all",
    exit = "event_start_date",
    overlap = "merge",
    inObservation = TRUE,
    table = "condition_occurrence"
  ) |>
    compute(name= "outcome_all", overwrite=TRUE)
  #Intersect concept cohort with conditions recorded from cancer registry only
  cdm$outcome_all <-cdm$outcome_all |> 
    CohortConstructor::requireTableIntersect(
      tableName = "conditions_registry",
      window = c(0,0),
      intersections = c(1,Inf),,
      targetStartDate = "condition_start_date",
      targetEndDate = "condition_start_date"
    ) |> 
    compute(name= "outcome_all", overwrite=TRUE)
  #Filter first record per cancer and set cohort_end to end of observation period
  cdm$outcome_all <- cdm$outcome_all |> 
    CohortConstructor::exitAtObservationEnd() 
}

# instantiate covariables --------------------------------------------------------
cdm <- omopgenerics::emptyCohortTable(cdm, name="conditions_all", overwrite = TRUE)

if(!isRegistry){
  log4r::info(logger, "Instantiate conditions")
  # codelistConditions <- CodelistGenerator::codesFromConceptSet(
  #   here::here("extras","1_InstantiateCohorts","Cohorts","Conditions"),
  #   cdm
  # )
  # cdm <- CDMConnector::generateConceptCohortSet(
  #   cdm = cdm, 
  #   conceptSet = codelistConditions,
  #   name = "conditions",
  #   overwrite = TRUE
  # )
  
  #Instantiate chronic conditions
  codelistConditions <- CodelistGenerator::codesFromConceptSet(
    here::here("extras","1_InstantiateCohorts","Cohorts","Conditions_chronic"),
    cdm
  )
  cdm <- CDMConnector::generateConceptCohortSet(
    cdm = cdm, 
    conceptSet = codelistConditions,
    name = "conditions_chronic",
    limit = "first",
    end = "observation_period_end_date",
    overwrite = TRUE
  )
  
  #Instantiate acute conditions
  codelistConditions <- CodelistGenerator::codesFromConceptSet(
    here::here("extras","1_InstantiateCohorts","Cohorts","Conditions_acute"),
    cdm
  )
  cdm <- CDMConnector::generateConceptCohortSet(
    cdm = cdm, 
    conceptSet = codelistConditions,
    name = "conditions_acute",
    limit = "all",
    end = "event_end_date",
    overwrite = TRUE
  )
  
  cdm <- cdm$conditions_chronic |>
    omopgenerics::bind(cdm$conditions_acute, name="conditions_all")
  
  # instantiate obesity using diagnosis and measurements and bind to "conditions_all"
  log4r::info(logger, "Instantiate obesity BMI")
  obesity_cohorts <- CDMConnector::readCohortSet(
    here::here("extras","1_InstantiateCohorts","Cohorts","Obesity")
  )
  cdm <- CDMConnector::generateCohortSet(
    cdm = cdm,
    cohortSet = obesity_cohorts,
    name = "obesity",
    computeAttrition = TRUE,
    overwrite = TRUE
  )
  cdm <- cdm$conditions_all |>
    omopgenerics::bind(cdm$obesity, name="conditions_all")
  
  # instantiate menopausal status  and bind to "conditions_all"
  log4r::info(logger, "Instantiate menopausal status")
  cdm[["menopausal_status"]] <- cdm |> 
    CohortConstructor::demographicsCohort(
      name = "menopausal_status",
      ageRange = c(50, 150),
      sex = c("Female"),
      minPriorObservation = daysPriorObservation
    ) |> 
    CohortConstructor::renameCohort(cohortId=1, newCohortName="menopausal_status")
  cdm <- cdm$conditions_all |> 
    omopgenerics::bind(cdm$menopausal_status, name="conditions_all")  
  
  # instantiate tobacco smoking and bind to "conditions_all"
  log4r::info(logger, "Instantiate tobacco smoking")
  tobacco_cohorts <- CDMConnector::readCohortSet(
    here::here("extras","1_InstantiateCohorts","Cohorts","Tobacco")
  )
  cdm <- CDMConnector::generateCohortSet(
    cdm = cdm, 
    cohortSet = tobacco_cohorts, 
    name = "tobacco",
    computeAttrition = TRUE,
    overwrite = TRUE
  )
  cdm <- cdm$conditions_all |> 
    omopgenerics::bind(cdm$tobacco, name="conditions_all")
} 

# instantiate medications -------------------------------------------------------
cdm <- omopgenerics::emptyCohortTable(cdm, name="medications", overwrite = TRUE)
if(!omopgenerics::isTableEmpty(cdm$drug_exposure)){
  
  log4r::info(logger, "Instantiate medications")
  codelistMedications <- CodelistGenerator::codesFromConceptSet(
    here::here("extras","1_InstantiateCohorts", "Cohorts","Medications"), 
    cdm
  )
  cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(
    cdm = cdm, 
    conceptSet = codelistMedications,
    gapEra = 30,
    name = "medications"
  )
  
}
  
  

