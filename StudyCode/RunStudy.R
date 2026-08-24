if(!exists("executeInstantiateCohorts")){executeInstantiateCohorts <- TRUE}
if(!exists("executeCharacterisation")){executeCharacterisation <- TRUE}
if(!exists("executeIncidence")){executeIncidence <- TRUE}
if(!exists("executePrevalence")){executePrevalence <- TRUE}
if(!exists("executeSurvival")){executeSurvival <- TRUE}
if(!exists("executeAgeStandardisation")){executeAgeStandardisation <- TRUE}

if(!exists("minimum_counts")){minimum_counts <- 10}

if(!exists("cancersToRun")){
  cancersToRun = c("respiratory", "digestive","male","female","hematologic", "oral","urinary","skin", "other")
}

if (is.na(startdate) || is.null(startdate) || (is.character(startdate) && length(startdate) == 0)) {
  print("Invalid start date. Setting 2000-01-01 as start date.")
  startdate <- as.Date("2000-01-01")
} else{
  startdate <- as.Date(startdate)
}

if (is.na(enddate) || is.null(enddate) || (is.character(enddate) && length(enddate) == 0)) {
  print("Invalid end date. Setting 2024-12-31 as end date.")
  enddate <- as.Date("2024-12-31")
}else{
  enddate <- as.Date(enddate)
}

# start the clock
start <- Sys.time()

# Set output folder location
final_output_folder <- here::here(paste0("Results_", db_name))
if (!dir.exists(final_output_folder)) {
  dir.create(final_output_folder)
}

# create logger
log_file <- here::here(final_output_folder, "log.txt")
if (file.exists(log_file)) {
  # unlink(log_file)
  logger <- log4r::logger(
    threshold = "INFO",
    appenders = log4r::file_appender(file = log_file, append = TRUE)
  )
} else{
  logger <- log4r::create.logger()
}
log4r::logfile(logger) <- log_file
log4r::level(logger) <- "INFO"
log4r::info(logger, "------------------------------------------------------")
log4r::info(logger, "INITIAL SETTINGS")
log4r::info(logger, "------------------------------------------------------")

# snapshot the cdm
snapshot <- OmopSketch::summariseOmopSnapshot(cdm = cdm)

# Predefined prior obs and age/sex strata
ageGroupList = list(
  "0 to 9" = c(0, 9),
  "10 to 19" = c(10, 19),
  "20 to 29" = c(20, 29),
  "30 to 39" = c(30, 39),
  "40 to 49" = c(40, 49),
  "50 to 59" = c(50, 59),
  "60 to 69" = c(60, 69),
  "70 to 79" = c(70, 79),
  "80 to 89" = c(80, 89),
  "90 to 99" = c(90, 99),
  "100 +" = c(100, 150)
)
ageGroupListBroad = list(
  "0 to 19" = c(0, 19),
  "20 to 49" = c(20, 49),
  "50 +" = c(50, 150)
)
sexGroup = c("Both", "Male", "Female")
daysPriorObservation = 365
date_ranges <- list(
  "2000-2004" = c("2000-01-01", "2004-12-31"),
  "2005-2009" = c("2005-01-01", "2009-12-31"),
  "2010-2014" = c("2010-01-01", "2014-12-31"),
  "2015-2019" = c("2015-01-01", "2019-12-31"),
  "2020-2024" = c("2020-01-01", "2024-12-31")
)

if(isHospital){
  daysPriorObservation <- 0
  executeIncidence <- FALSE
  executePrevalence <- FALSE
}

if(executeAgeStandardisation){
  source(here::here( "extras","Standardisation", "age_standardisation.R"))
  standardPopulations <- readStandardPopulations()
}

if(toupper(db_name) == "SIDIAP"){
  log4r::info(logger,"Filter SIDIAP drug_exposure keeping only prescriptions")
  cdm$drug_exposure <- cdm$drug_exposure |>
    filter(drug_type_concept_id == 32839)
}


# Get groups of cancer "cancerTypes"
log4r::info(logger,"Get cancer groups and stratification variables")
source(here::here("extras/cancerGroups.R"))
cancer_types <- cancer_types[cancersToRun]

# Instantiate cohorts 
if (executeInstantiateCohorts) {
  log4r::info(logger, "STEP 0 INSTANTIATE COHORTS ---------------------------")
  source(here::here("extras/1_InstantiateCohorts/instantiate_cohorts.R"))
  source(here::here("extras/1_InstantiateCohorts/instantiate_denominator.R"))
}

# Compute cohort counts --------------------------------------------------------
log4r::info(logger, "Get cohort counts")

cancer_counts <- cdm$outcome_all |> 
  omopgenerics::settings() |> 
  left_join(cdm$outcome_all |> omopgenerics::cohortCount(), by="cohort_definition_id") |> 
  select(cohort_definition_id, cohort_name, number_subjects)

covariable_counts <- cdm$conditions_all |> 
  omopgenerics::settings() |> 
  left_join(cdm$conditions_all |> omopgenerics::cohortCount(), by="cohort_definition_id") |> 
  select(cohort_definition_id, cohort_name, number_subjects)

# Loop over each group  ---------------------------------------------------------
if (any(cancer_counts$number_subjects > minimum_counts, na.rm = TRUE)) {
  for (cancer_group in names(cancer_types)){
    log4r::info(logger, "------------------------------------------------------")
    log4r::info(logger, paste0("RUNNING ANALYSIS FOR ", toupper(cancer_group)," CANCERS"))
    log4r::info(logger, "------------------------------------------------------")

    #Get cancer types in the current GROUP
    cancer_types_gr <-  cancer_counts |> 
      filter(cohort_name %in% cancer_types[[cancer_group]]$types) |> 
      filter(number_subjects > minimum_counts )
    
    if(nrow(cancer_types_gr)>0) {
      
      output_folder <-  paste0(final_output_folder, "/", cancer_group)
      if (!dir.exists(output_folder)) {
        dir.create(output_folder, recursive = TRUE)
      }
      
      snapshot |> 
        omopgenerics::exportSummarisedResult(
          minCellCount = NULL, 
          fileName =  paste0(db_name,"_summarise_omop_snapshot.csv"),
          path = output_folder
        )
    
      log4r::info(logger, "STEP 1 CREATE OUTCOME COHORT -------------------------")
      source(here::here("extras/1_InstantiateCohorts/create_outcome.R"))

      if (executeCharacterisation) {
        log4r::info(logger, "STEP 2 COHORT CHARACTERISATION -----------------------")
        source(here::here("extras/2_Characterisation/characterisation.R"))
      }
      if (executeIncidence) {
        log4r::info(logger, "STEP 3 INCIDENCE -------------------------------------")
        # source(here::here("extras/3_Incidence/incidence.R"))
        source(here::here("extras/3_Incidence/incidence_opt.R"))
      }
      if (executePrevalence) {
        log4r::info(logger, "STEP 4 PREVALENCE ------------------------------------")
        source(here::here("extras/4_Prevalence/prevalence.R"))
      }
      if (executeSurvival) {
        log4r::info(logger, "STEP 5 SURVIVAL ANALYSIS -----------------------------")
        source(here::here("extras/5_Survival/survival.R"))
      }
    } else{
      log4r::info(logger, "NO MINIMUM SUBJECT COUNT TO CREATE COHORTS IN THIS GROUP")
    }
  }
} else{
  log4r::info(logger, "NO MINIMUM SUBJECT COUNT TO CREATE ANY COHORT")
}

# Zip files
log4r::info(logger, "------------------------------------------------------")
log4r::info(logger, "ZIP RESULTS ")
zip_file <- here::here(paste0("Results_", db_name,".zip"))
zip::zipr(zip_file, files = final_output_folder)
file.copy(from = zip_file, to = here::here("shiny","data"))
# Compute and save execution time 
x <- abs(as.numeric(Sys.time()-start, units="secs"))
log4r::info(logger, 
  paste0("STUDY TOOK: ",
         sprintf("%02d:%02d:%02d:%02d",
                 x %/% 86400,  x %% 86400 %/% 3600, x %% 3600 %/% 60,  x %% 60 %/% 1)))
log4r::info(logger, "------------------------------------------------------")
