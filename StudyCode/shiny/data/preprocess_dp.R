#Recognize zip files in data folder
zip_files <- list.files(path = here::here("data"),pattern = "\\.zip$",full.names = FALSE)

#Loop over db results (for data partners it would be just their results)
result = omopgenerics::emptySummarisedResult()
for (db in zip_files){
  #DB name
  dbn = gsub("Results_", "", gsub(".zip", "", db))
  message("Processing results from: ", dbn)
  # Unzip results
  unzip_dir = gsub(".zip", "", here::here("data",db))
  if (!dir.exists(unzip_dir)){
    unzip(here::here("data", db), exdir = here::here("data"))
    message("Unzipped: ", db)
  }
  #Import results from each cancer group
  result_db = omopgenerics::emptySummarisedResult()
  subfolds = list.dirs(unzip_dir)[-1]
  for (subfold in subfolds){
    cancer_group <- basename(subfold)
    message("Processing results for cancer group: ", cancer_group)
    if(nrow(result_db) == 0){
      result_load <-  omopgenerics::importSummarisedResult(subfold) 
    }else{
      files <- list.files(subfold, pattern = "\\.csv$", full.names = TRUE)
      files <- files[!grepl("snapshot", files)]
      result_load <-  omopgenerics::importSummarisedResult(files) 
    }
    attr(result_load, "settings") <-  attr(result_load, "settings") |>
      mutate(cancer_gr = cancer_group)
    message("Bind results  with other groups")
    result_db <- omopgenerics::bind(
      result_db, 
      result_load
    )
  }
  result <- omopgenerics::bind(
    result, 
    result_db
  )
}

#Get results ids for each type of result
resultList <- omopgenerics::settings(result) %>% 
  dplyr::mutate(
    result_type_group = dplyr::if_else(
      startsWith(result_type, "survival_"),
      "survival",
      result_type
    )
  ) %>%
  group_by(result_type_group) %>% 
  summarise(
    result_ids = paste(sort(unique(result_id)), collapse = ","),
    .groups = "drop"
  ) %>%
  mutate(result_ids = strsplit(result_ids, ",")) %>%
  mutate(result_ids = lapply(result_ids, function(x) as.integer(trimws(x)))) %>%
  { setNames(.$result_ids, .$result_type_group) }


# Process data and save results in .Rdata file 
data <- prepareResult(result, resultList)
filterValues <- defaultFilterValues(result, resultList)

# Add incidence stratification to filterValues
exclude_patterns <- c("cdm_name", "date", "outcome", "denominator", "variable", "analysis", 
                      "interval", "year", "target", "study", "age", "sex")
inc_strata <- filterValues[
  grepl("incidence_grouping_",names(filterValues))  & 
    !grepl(paste(exclude_patterns, collapse = "|"), names(filterValues))
]
if(length(inc_strata)==0){
  filterValues$incidence_strata <- NULL
}else{
  incidence_strata <- gsub("incidence_grouping_", "", names(inc_strata))
  filterValues$incidence_strata <- incidence_strata
}

# survival results
if("survival" %in% names(data)){
    #correct age_gr and sex values
    strataCols <- visOmopResults::strataColumns(data$survival)
    if("sex" %in% strataCols & "age_gr" %in% strataCols & "age_gr_broad" %in% strataCols){
      data$survival <- data$survival |>
        omopgenerics::splitStrata() |>
        mutate(
          sex     = stringr::str_trim(sex, side = "right"),
          age_gr  = stringr::str_trim(age_gr, side = "right"),
          age_gr_broad  = stringr::str_trim(age_gr_broad, side = "right")
        ) |>
        omopgenerics::uniteStrata(strataCols)

      var_adj <- c("survival_grouping_sex", "survival_grouping_age_gr", "survival_grouping_age_gr_broad")
      filterValues[var_adj] <- lapply(filterValues[var_adj], function(x) unique(stringr::str_trim(x, side = "right")))
    }

    # create list of strata values
    cols <- visOmopResults::strataColumns(data$survival)
    if(length(cols) != 0){
      filterValues$survival_strata <- cols[!grepl(paste(c(exclude_patterns, "reason"), collapse = "|"), cols)]
    }else{
      filterValues$survival_strata <- NULL
    }
  
}


# Group cancers and stratification
source(here::here("cancerGroups.R"))
data$cancer_group_strata <- cancer_types
filterValues$cancer_group_names <- names(cancer_types)


#Save RData file
save(data, filterValues, file = here::here( "data", "shinyData.RData"))
rm(result,resultList)
