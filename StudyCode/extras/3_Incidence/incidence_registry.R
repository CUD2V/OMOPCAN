# Estimate Incidence 
log4r::info(logger, "Estimate registry incidence") 

inc <- estimateRegistryIncidence(
  cdm = cdm,
  denominatorTableName = "denominator_counts",
  outcomeTable = "outcome",
  # censorTable = "exclusion", 
  interval = c("years", "overall"),
  outcomeWashout = Inf,
  repeatedEvents = FALSE
)

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
  