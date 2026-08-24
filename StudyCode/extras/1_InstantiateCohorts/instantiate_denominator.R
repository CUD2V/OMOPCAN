# Generate denominator (NOT CANCER REGISTRIES)-----
log4r::info(logger, "Instantiate denominator")

cdm <- IncidencePrevalence::generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator" ,
  cohortDateRange = c(as.Date(startdate), as.Date(enddate)),
  requirementInteractions = TRUE,
  # ageGroup = append(ageGroupList, list("0 to 150" = c(0,150)), after = 0),
  ageGroup = append(c(ageGroupList, ageGroupListBroad), list("0 to 150" = c(0,150)), after = 0),
  sex = sexGroup,
  daysPriorObservation = daysPriorObservation
)


