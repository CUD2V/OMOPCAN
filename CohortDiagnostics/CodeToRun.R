# #install.packages("renv") # if not already installed, install renv from CRAN
renv::activate()
renv::restore() # this should prompt you to install the various packages required for the study

# packages -----
# load the below packages 
# you should have them all available, with the required version, after
# having run renv::restore above
library(DatabaseConnector)
library(CohortDiagnostics)
library(CirceR)
library(CohortGenerator)
library(here)
library(stringr)
library(tibble)
library(dplyr)

# --- Optional: source a gitignored site_config.R for credentials/schemas ----
# Copy site_config_template.R to site_config.R and fill in your values.
# When present, site_config.R sets connectionDetails, schemas, etc. directly,
# skipping the inline config block below.
if (file.exists(here("site_config.R"))) source(here("site_config.R"))

# database metadata and connection details -----
# The name/ acronym for the database
if (!exists("db.name")) db.name <-  "..."

# database connection details
if (!exists("connectionDetails")) {
  server <-  "..."
  user <- "..."
  password <- "..."
  port <- "..."
  host <- "..."

  # sql dialect used with the OHDSI SqlRender package
  targetDialect <- "..."

  # driver for DatabaseConnector
  #downloadJdbcDrivers(targetDialect, here()) # if you already have this you can omit and change pathToDriver below
  connectionDetails <- createConnectionDetails(
    dbms = targetDialect,
    server = server,
    user = user,
    password = password,
    port = port,
    pathToDriver = here()
  )
}

# schema that contains the OMOP CDM with patient-level data
if (!exists("cdm_database_schema")) cdm_database_schema <- "..."
# schema that contains the vocabulary
if (!exists("vocabulary_database_schema")) vocabulary_database_schema <- "..."
# schema where a results table will be created
if (!exists("results_database_schema")) results_database_schema <- "..."

# stem for tables to be created in your results schema for this analysis
# You can keep the above names or change them
# Note, any existing tables in your results schema with the same name will be overwritten
if (!exists("cohortTableStem")) cohortTableStem <- "..."

# Run analysis ----
source(here("RunAnalysis.R"), echo=TRUE)


 # Merge results
CohortDiagnostics::createMergedResultsFile(dataFolder = here::here("Results"), overwrite = TRUE)

# Launch Shiny to check results. It may prompt you to install OhdsiShinyModules, if not already installed. We advise you to skip the updates of other packages. 
CohortDiagnostics::launchDiagnosticsExplorer()
