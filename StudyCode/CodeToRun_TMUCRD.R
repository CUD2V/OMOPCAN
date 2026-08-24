#Activate and restore renv
renv::activate()
renv::restore()

library(dplyr) 

# EDIT FROM HERE: -------------------------------------------------------------

# Set the short name/acronym for your database (e.g. "SIDIAP", "IPCI", "CPRD", "NAJS", etc..)
# Please do not use "omop" or "cdm" for db_name
db_name <-"..."

# database connection details
server_dbi <- "..."
user       <- "..."
password   <- "..."
port       <- "..."
host       <- "..."

# DBI driver (change based on your dbms)
drv <- RPostgres::Postgres()

# Connection
# See https://darwin-eu.github.io/CDMConnector/articles/a04_DBI_connection_examples.html for examples
db <- DBI::dbConnect(
  drv,
  host = host,
  port = port,
  dbname = server_dbi,
  user = user, 
  password = password
)


# The name of the schema that contains the omop tables
cdm_database_schema <- "..."

# The name of the schema where results tables will be created 
results_database_schema <- "..."

#Vocabulary schema
vocabulary_database_schema <- list()
vocabulary_database_schema[["catalog"]] <- "OMOP_VOCAB"
vocabulary_database_schema[["schema"]] <- "dbo" 

# prefix to be used for tables created in the write schema
stem_table <- "..."

# minimum counts that can be displayed according to data governance
minimum_counts <- 10

# Data availability in your database: start and end dates 
startdate <- as.Date("YYYY-MM-DD") 
enddate <- as.Date("YYYY-MM-DD") 

# If your database is a hospital, please set to TRUE.
isHospital <- FALSE

# If your database is a cancer registry set to TRUE and select corresponding path to population counts
isRegistry <- FALSE
# filepop <- here::here("extras/Registries/data_population/norway_dpop.csv")
# filepop <- here::here("extras/Registries/data_population/geneve_dpop.csv")
# filepop <- here::here("extras/Registries/data_population/netherlands_dpop.csv")
# filepop <- here::here("extras/Registries/data_population/belgium_dpop.csv")

# If the database contains death data 
executeSurvival <- TRUE


# DO NOT EDIT FROM HERE --------------------------------------------------------

# create cdm reference 
executeInstantiateCohorts <- TRUE
if(executeInstantiateCohorts){
  cdm <- CDMConnector::cdmFromCon(
    con = db,
    cdmSchema = cdm_database_schema,
    writeSchema = results_database_schema,
    vocabularySchema = vocabulary_database_schema,  
    writePrefix = stem_table,
    cdmName = db_name,
    .softValidation = TRUE
  )
} else{
  listCohorts <- c("outcome_all", "exclusion")
  if(!isRegistry){listCohorts <- c(listCohorts, "denominator", "conditions_all", "medications")}
  cdm <- CDMConnector::cdmFromCon(
    con = db, 
    cdmSchema = cdm_database_schema,
    writeSchema = results_database_schema, 
    vocabularySchema = vocabulary_database_schema,  
    writePrefix = stem_table,
    cdmName = db_name,
    cohortTables = listCohorts,
    .softValidation = TRUE
  )
}

## Add vocabulary tables to cdm
vocab_tables <- c("concept",  "concept_ancestor",  "concept_class", "concept_relationship", 
                "concept_synonym", "domain", "drug_strength", "relationship", "vocabulary")
for (tbl_name in vocab_tables) {
  cdm[[tbl_name]] <- dplyr::tbl(
    db,
    DBI::Id(
      catalog = vocabulary_database_schema[["catalog"]],  # "OMOP_VOCAB"
      schema  = vocabulary_database_schema[["schema"]],   # "dbo"
      table   = tbl_name
    )
  )
}


if(isRegistry){
  source(here::here("RunStudyRegistry.R"))
}else{
  source(here::here("RunStudy.R"))
}

# Close connection
CDMConnector::cdmDisconnect(cdm)
print("Thanks for running this study!")
