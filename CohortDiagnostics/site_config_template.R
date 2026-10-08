# site_config_template.R — copy to site_config.R and fill in your values
#
# site_config.R is gitignored so your credentials and project-specific
# settings stay out of version control. CodeToRun.R sources it automatically
# when present; values here override the inline defaults.
#
# BigQuery via JDBC + Application Default Credentials (OAuthType=3).
# Prerequisites:
#   1. gcloud auth application-default login   (run once in terminal)
#   2. BQ_DRIVER_PATH env var in ~/.Renviron pointing to Simba JDBC driver dir
#      e.g.  BQ_DRIVER_PATH="/path/to/jdbc/bigquery-driver"
#
# renv setup note:
#   The renv.lock pins the Posit package manager, which may be unreachable on
#   some servers. If renv::restore() times out, run from an R shell (not
#   RStudio) and override the repo before restoring:
#     options(renv.config.repos.override = c(CRAN = "https://cloud.r-project.org"))
#     renv::restore()

# --- Database connection ------------------------------------------------------
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "bigquery",
  connectionString = paste0(
    "jdbc:bigquery://https://www.googleapis.com/bigquery/v2:443;",
    "ProjectId=YOUR_GCP_PROJECT;",
    "OAuthType=3;",
    "EnableSession=1"
  ),
  pathToDriver = Sys.getenv("BQ_DRIVER_PATH"),
  user = "",
  password = ""
)

# --- Site + OMOP CDM schemas --------------------------------------------------
db.name                  <- "YOUR_SITE_ID"
cdm_database_schema      <- "YOUR_GCP_PROJECT.YOUR_CDM_DATASET"
vocabulary_database_schema <- "YOUR_GCP_PROJECT.YOUR_CDM_DATASET"
results_database_schema  <- "YOUR_GCP_PROJECT.YOUR_WRITE_DATASET"
cohortTableStem          <- "omopcan_cd"

# BigQuery has no real temp tables — SqlRender emulates them as permanent
# tables in this dataset. Point to a dataset you have write access to.
options(sqlRenderTempEmulationSchema = "YOUR_GCP_PROJECT.YOUR_WRITE_DATASET")
