# Generate denominator 
log4r::info(logger, "Read registry population counts")
tablePopulation <- readDenominatorCounts(file= filepop)

denominator_counts <- getRegistryDenominator(
  data_input = tablePopulation,
  # region_input = registryRegion,
  start_year_input = clock::get_year(startdate),
  end_year_input = clock::get_year(enddate),
  strata_age = TRUE ,
  strata_sex = TRUE
)

# Add agr_group_broad to denominator 
log4r::info(logger, "Compute denominator age groups broad")
parse_age <- function(age_gr) {
  if (age_gr == "Overall") {
    return(c(NA_real_, NA_real_))
  } else if (stringr::str_detect(age_gr, "and over")) {
    low <- as.numeric(stringr::str_extract(age_gr, "^[0-9]+"))
    return(c(low, 150))
  } else {
    parts <- as.numeric(stringr::str_split(age_gr, " to ")[[1]])
    return(parts)
  }
}

denom_parsed <- denominator_counts |>
  select( year, sex, age_gr, age_gr_n, population) |> 
  rowwise() |>
  mutate(age_low  = parse_age(age_gr)[1],  age_high = parse_age(age_gr)[2]) |>
  ungroup()

max_age_gr_n <- max(denom_parsed$age_gr_n, na.rm = TRUE)

denominator_age_broad <- purrr::map_dfr(names(ageGroupListBroad), function(grp_name) {
  rango <- ageGroupListBroad[[grp_name]]
  denom_parsed |>
    filter(!is.na(age_low), age_low >= rango[1], age_high <= rango[2]) |>
    group_by(year, sex) |>
    summarise(population = sum(population), .groups = "drop") |>
    mutate(
      age_gr = grp_name,
      age_gr_n = max_age_gr_n + match(grp_name, names(ageGroupListBroad))
   )
})

denom_final <- denominator_counts |>
  select( year, sex, age_gr, age_gr_n, population) |>
  bind_rows(denominator_age_broad) |>
  arrange(year,age_gr_n, sex ) |> 
  group_by(age_gr_n, sex ) |> 
  mutate(cohort_definition_id = cur_group_id()) |>
  ungroup() |> 
  select(cohort_definition_id, year, sex, age_gr, age_gr_n, population)

log4r::info(logger, "Insert registry population counts in the cdm")
cdm <- omopgenerics::insertTable(
  cdm, 
  name="denominator_counts", 
  table = denom_final,  
  overwrite = TRUE,  
  temporary = FALSE
  )

log4r::info(logger, "Extract sex and age groups from registry population counts")
sexGroup = sort(unique(denominator_counts$sex))
ageGroup = denominator_counts %>% distinct(age_gr_n, age_gr) %>% arrange(age_gr_n) %>% pull(age_gr)
ageGroup = ageGroup[!grepl("overall", ageGroup, ignore.case=TRUE)]
ageGroupList <- setNames(
  lapply(ageGroup, function(g) {
    if (stringr::str_detect(g, "to") || stringr::str_detect(g, "-")) {
      nums <- as.numeric(stringr::str_split(g, " to ", simplify = TRUE))
      return(nums)
    }
    if (stringr::str_detect(g, "over") || stringr::str_detect(g, "+")) {
      x <- as.numeric(stringr::str_extract(g, "\\d+"))
      return(c(x, 150))
    }
    return(NA)
  }),
  ageGroup
)
