server <- function(input, output, session) {
  
  #browser()
  #download raw data 
  output$download_raw <- shiny::downloadHandler(
    filename = "results.csv",
    content = function(file) {
      omopgenerics::exportSummarisedResult(data, fileName = file)
    }
  )
 
  ## CDM SNAPSHOT ############################################
  createOutputCDM <- shiny::reactive({
    result <- data |>
      filterData("summarise_omop_snapshot", input)
    OmopSketch::tableOmopSnapshot(
      result
    )
  })
  output$summarise_omop_snapshot_gt_17 <- gt::render_gt({
    createOutputCDM() %>%
      gt::tab_options(
        table.font.size = "100%",
        data_row.padding = gt::px(2)
      )
  })
  output$summarise_omop_snapshot_gt_17_download <- shiny::downloadHandler(
    filename = paste0("output_gt_summarise_omop_snapshot.", input$summarise_omop_snapshot_gt_17_download_type),
    content = function(file) {
      obj <- createOutputCDM()
      gt::gtsave(data = obj, filename = file)
    }
  )

  ## COHORT COUNT  ###################################################
  getTidyDataSummariseCohortCount <- shiny::reactive({
    vars = c("variable_name", "variable_level", "estimate_name")
    res <- data |>
      filterData("summarise_cohort_count", input) |>
      omopgenerics::addSettings() |>
      omopgenerics::splitAll() |>
      dplyr::select(!"result_id")|>
      visOmopResults::pivotEstimates(pivotEstimatesBy = vars)
    #filter group of cancer
    all_types <- unique(unlist(lapply(
      data$cancer_group_strata[input$summarise_cohort_count_cancer_group], 
      `[[`, "types"
    )))
    all_strata <- unique(unlist(lapply(
      data$cancer_group_strata[input$summarise_cohort_count_cancer_group], 
      `[[`, "stratification"
    )))
    res <- res |>
      dplyr::filter(
        dplyr::case_when(
          table_name %in% c("outcome", "outcome_surv") ~ 
            cohort_name %in% all_types,
          table_name == "conditions_st" ~ 
            cohort_name %in% all_strata,
          TRUE ~ TRUE
        )
      )
  })
  
  output$summarise_cohort_count_tidy <- DT::renderDT({
    palette <- colorRampPalette(c("#ffffff", "#cccccc"))(100)
    #Escala por columna individual
    scaled_colors_column <- function(vec) {
      rng <- range(vec, na.rm = TRUE)
      index <- ceiling((vec - rng[1]) / diff(rng) * 99) + 1
      index[is.na(index)] <- 1
      palette[index]
    }
    dataRes <- getTidyDataSummariseCohortCount()
    if (any(grepl("Number records_count", colnames(dataRes))) & 
        any(grepl("Number subjects_count", colnames(dataRes)))){
      values_1 <- dataRes[["Number records_count"]]
      values_2 <- dataRes[["Number subjects_count"]]
      colors_count_1 <- scaled_colors_column(values_1)
      colors_count_2 <- scaled_colors_column(values_2)
      DT::datatable(
        getTidyDataSummariseCohortCount(),
        options = list(scrollX = TRUE),
        rownames = FALSE,
        class = 'cell-border'
        # class = 'white-background'
      )|>
        DT::formatStyle(
          columns = colnames(getTidyDataSummariseCohortCount()),
          backgroundColor = "white"
        ) |>
        DT::formatStyle(
          columns = "Number records_count",
          backgroundColor = DT::styleEqual(values_1, colors_count_1)
        ) |>
        DT::formatStyle(
          columns = "Number subjects_count",
          backgroundColor = DT::styleEqual(values_2, colors_count_2)
        )
    } else if (any(grepl("Number records_count", colnames(dataRes)))){
      values_1 <- dataRes[["Number records_count"]]
      colors_count_1 <- scaled_colors_column(values_1)
      DT::datatable(
        getTidyDataSummariseCohortCount(),
        options = list(scrollX = TRUE),
        rownames = FALSE,
        class = 'cell-border hover'
        # class = 'white-background'
      )|>
      DT::formatStyle(
        columns = colnames(getTidyDataSummariseCohortCount()),
        backgroundColor = "white",
        color = "black"
      ) |>
        DT::formatStyle(
          columns = "Number records_count",
          backgroundColor = DT::styleEqual(values_1, colors_count_1),
          color = "black"
        ) 
    } else if (any(grepl("Number subjects_count", colnames(dataRes)))){
      values_2 <- dataRes[["Number subjects_count"]]
      colors_count_2 <- scaled_colors_column(values_2)
      DT::datatable(
        getTidyDataSummariseCohortCount(),
        options = list(scrollX = TRUE),
        rownames = FALSE,
        class = 'cell-border hover'
        # class = 'white-background'
      )|>
        DT::formatStyle(
          columns = colnames(getTidyDataSummariseCohortCount()),
          backgroundColor = "white",
          color = "black"
        ) |>
        DT::formatStyle(
          columns = "Number records_count",
          backgroundColor = DT::styleEqual(values_2, colors_count_2),
          color = "black"
        ) 
    } else {
      DT::datatable(
        getTidyDataSummariseCohortCount(),
        options = list(scrollX = TRUE),
        rownames = FALSE,
        class = 'cell-border hover'
        # class = 'white-background'
      ) %>% 
        DT::formatStyle(
          columns = colnames(getTidyDataSummariseCohortCount()),
          backgroundColor = "white",
          color = "black"
        ) 
    }
  })
  
  output$summarise_cohort_count_tidy_download <- shiny::downloadHandler(
    filename = "tidy_summarise_cohort_count.csv",
    content = function(file) {
      getTidyDataSummariseCohortCount() |>
        readr::write_csv(file = file)
    }
  )

  ##output table summarise_cohort_count
  createOutputCount <- shiny::reactive({
    result <- data |>
      filterData("summarise_cohort_count", input)
    #filter group cancer
    all_types <- unique(unlist(lapply(
      data$cancer_group_strata[input$summarise_cohort_count_cancer_group], 
      `[[`, "types"
    )))
    all_strata <- unique(unlist(lapply(
      data$cancer_group_strata[input$summarise_cohort_count_cancer_group], 
      `[[`, "stratification"
    )))
    result <- result |> 
      omopgenerics::filterSettings(
        ! table_name %in% c("outcome", "conditions_st", "outcome_surv")
      ) |> 
      omopgenerics::bind(
        result |> 
          omopgenerics::filterGroup(
            cohort_name %in% c(all_types, all_strata )
            )
      )
    CohortCharacteristics::tableCohortCount(
      result,
      header = input$summarise_cohort_count_gt_9_header,
      groupColumn = input$summarise_cohort_count_gt_9_groupColumn,
      hide = input$summarise_cohort_count_gt_9_hide
    )
  })
  output$summarise_cohort_count_gt_9 <- gt::render_gt({
    createOutputCount()
  })
  output$summarise_cohort_count_gt_9_download <- shiny::downloadHandler(
    filename = paste0("output_gt_summarise_cohort_count.", input$summarise_cohort_count_gt_9_download_type),
    content = function(file) {
      obj <- createOutputCount()
      gt::gtsave(data = obj, filename = file)
    }
  )
  
  #plot cohort counts 
  createOutputCountPlot <- shiny::reactive({
    result <- data |>
      filterData("summarise_cohort_count", input)
    #filter group cancer
    all_types <- unique(unlist(lapply(
      data$cancer_group_strata[input$summarise_cohort_count_cancer_group], 
      `[[`, "types"
    )))
    all_strata <- unique(unlist(lapply(
      data$cancer_group_strata[input$summarise_cohort_count_cancer_group], 
      `[[`, "stratification"
    )))
    #filter cancer group
    result <- result |> 
      omopgenerics::filterSettings(
        ! table_name %in% c("outcome", "conditions_st", "outcome_surv")
      ) |> 
      omopgenerics::bind(
        result |> 
          omopgenerics::filterGroup(
              cohort_name %in% c(all_types, all_strata )
          )
        )
    CohortCharacteristics::plotCohortCount(
      result,
      facet = input$summarise_cohort_count_ggplot2_10_facet,
      colour = input$summarise_cohort_count_ggplot2_10_colour
    ) 
  })
  output$summarise_cohort_count_ggplot2_10 <- shiny::renderPlot({
    createOutputCountPlot()
  })
  output$summarise_cohort_count_ggplot2_10_download <- shiny::downloadHandler(
    filename = paste0("output_ggplot2_summarise_cohort_count.", "png"),
    content = function(file) {
      obj <- createOutputCountPlot()
      ggplot2::ggsave(
        filename = file,
        plot = obj,
        width = as.numeric(input$summarise_cohort_count_ggplot2_10_download_width),
        height = as.numeric(input$summarise_cohort_count_ggplot2_10_download_height),
        units = input$summarise_cohort_count_ggplot2_10_download_units,
        dpi = as.numeric(input$summarise_cohort_count_ggplot2_10_download_dpi)
      )
    }
  )
  
  ## COHORT ATTRITION #################################################
  #tidy summarise_cohort_attrition 
  getTidyDataSummariseCohortAttrition <- shiny::reactive({
    vars <-  c("variable_name", "variable_level", "estimate_name")
    res <- data |>
      filterData("summarise_cohort_attrition", input) |>
      omopgenerics::addSettings() |>
      omopgenerics::splitAll() |>
      dplyr::select(!"result_id")
    #filter group of cancer
    all_types <- unique(unlist(lapply(
      data$cancer_group_strata[input$summarise_cohort_attrition_cancer_group], 
      `[[`, "types"
    )))
    all_strata <- unique(unlist(lapply(
      data$cancer_group_strata[input$summarise_cohort_attrition_cancer_group], 
      `[[`, "stratification"
    )))
    res <- res |>
      dplyr::filter(
        dplyr::case_when(
          table_name %in% c("outcome", "outcome_surv") ~ 
            cohort_name %in% all_types,
          table_name == "conditions_st" ~ 
            cohort_name %in% all_strata,
          TRUE ~ TRUE
        )
      )
    #columns to eliminate
    colsEliminate <- colnames(res)
    colsEliminate <- colsEliminate[!colsEliminate %in% c(
      "cdm_name","cohort_name", "reason", "reason_id" ,"variable_name", "variable_level",
      "estimate_name", "estimate_type", "estimate_value"
    )]
    res |>
      visOmopResults::pivotEstimates(pivotEstimatesBy = vars)|>
      dplyr::select(!dplyr::all_of(colsEliminate)) %>% 
      dplyr::rename(n_subj = number_subjects_count,
                    n_rec = number_records_count,
                    excl_subj = excluded_subjects_count,
                    excl_rec = excluded_records_count)
  })
  output$summarise_cohort_attrition_tidy <- DT::renderDT({
    DT::datatable(
      getTidyDataSummariseCohortAttrition(),
      options = list(scrollX = TRUE),
      rownames = FALSE
    )
  })
  output$summarise_cohort_attrition_tidy_download <- shiny::downloadHandler(
    filename = "tidy_summarise_cohort_attrition.csv",
    content = function(file) {
      getTidyDataSummariseCohortAttrition() |>
        readr::write_csv(file = file)
    }
  )
 #plot cohort attrition
  createOutputCohortAttrition <- shiny::reactive({
    result <- data |>
      filterData("summarise_cohort_attrition", input)
    #filter group of cancer
    all_types <- unique(unlist(lapply(
      data$cancer_group_strata[input$summarise_cohort_attrition_cancer_group], 
      `[[`, "types"
    )))
    all_strata <- unique(unlist(lapply(
      data$cancer_group_strata[input$summarise_cohort_attrition_cancer_group], 
      `[[`, "stratification"
    )))
    result <- result |> 
      omopgenerics::filterSettings(
        ! table_name %in% c("outcome", "conditions_st", "outcome_surv")
      ) |> 
      omopgenerics::bind(
        result |> 
          omopgenerics::filterGroup(
            cohort_name %in% c(all_types, all_strata )
          )
      )
    #Plot
    CohortCharacteristics::plotCohortAttrition(
      result
    )
  })
  output$summarise_cohort_attrition_grViz_4 <- DiagrammeR::renderGrViz({
    createOutputCohortAttrition()
  })
  output$summarise_cohort_attrition_grViz_4_download <- shiny::downloadHandler(
    filename = paste0("output_grViz_summarise_cohort_attrition.", "png"),
    content = function(file) {
      obj <- createOutputCohortAttrition()
      DiagrammeR::export_graph(
        graph = obj,
        file_name = file,
        fily_type = "png",
        width = as.numeric(input$summarise_cohort_attrition_grViz_4_download_width),
        height = as.numeric(input$summarise_cohort_attrition_grViz_4_download_height)
      )
    }
  )

  ## CHARACTERISTICS: DEMOGRAPHCS ########################################
  #tidy summarise_characteristics 
  getTidyDataSummariseCharacteristicsDemographics <- shiny::reactive({
    res <- data |>
      filterData("summarise_characteristics", input, "summarise_characteristics_demographics") |> 
      omopgenerics::filter(!grepl("Condit|Medicat", variable_name)) |> 
      # filterData("summarise_characteristics_demographics", input) |>
      omopgenerics::addSettings() |>
      omopgenerics::splitAll() |>
      dplyr::select(!"result_id")
    #filter group of cancer
    all_types <- unique(unlist(lapply(
      data$cancer_group_strata[input$summarise_characteristics_demographics_cancer_group], 
      `[[`, "types"
    )))
    res <- res |>
      dplyr::filter( cohort_name %in% all_types)
    # columns to eliminate
    colsEliminate <- colnames(res)
    colsEliminate <- colsEliminate[!colsEliminate %in% c(
      input$summarise_characteristics_demographics_tidy_columns, "variable_name", "variable_level",
      "estimate_name", "estimate_type", "estimate_value"
    )]
    # pivot
    pivot <- input$summarise_characteristics_demographics_tidy_pivot
    if (pivot != "none") {
      vars <- switch(pivot,
        "estimates" = "estimate_name",
        "estimates and variables" = c("variable_name", "variable_level", "estimate_name")
      )
      res <- res |>
        visOmopResults::pivotEstimates(pivotEstimatesBy = vars)
    }
    res |>
      dplyr::select(!dplyr::all_of(colsEliminate))
  })
  output$summarise_characteristics_demographics_tidy <- DT::renderDT({
    DT::datatable(
      getTidyDataSummariseCharacteristicsDemographics(),
      options = list(scrollX = TRUE),
      rownames = FALSE
    )
  })
  output$summarise_characteristics_demographics_tidy_download <- shiny::downloadHandler(
    filename = "tidy_summarise_characteristics_demographics.csv",
    content = function(file) {
      getTidyDataSummariseCharacteristicsDemographics() |>
        readr::write_csv(file = file)
    }
  )

  #table characteristics
  createOutputCharactDemo <- shiny::reactive({
    result <- data |>
      # filterData("summarise_characteristics_demographics", input)
      filterData("summarise_characteristics", input, "summarise_characteristics_demographics")|> 
      omopgenerics::filterSettings(additional == "")
      
    #filter group of cancer
    all_types <- unique(unlist(lapply(
      data$cancer_group_strata[input$summarise_characteristics_demographics_cancer_group], 
      `[[`, "types"
    )))
    result <- result |> 
      omopgenerics::filterGroup(cohort_name %in% all_types )
    #table
    CohortCharacteristics::tableCharacteristics(
      result,
      header = input$summarise_characteristics_demographics_gt_7_header,
      groupColumn = input$summarise_characteristics_demographics_gt_7_groupColumn,
      hide = input$summarise_characteristics_demographics_gt_7_hide
    )
  })
  output$summarise_characteristics_demographics_gt_7 <- gt::render_gt({
    createOutputCharactDemo()
  })
  output$summarise_characteristics_demographics_gt_7_download <- shiny::downloadHandler(
    filename = paste0("output_gt_summarise_characteristics_demographics.", input$summarise_characteristics_demographics_gt_7_download_type),
    content = function(file) {
      obj <- createOutputCharactDemo()
      gt::gtsave(data = obj, filename = file)
    }
  )
  ##plot cohort demographcis
  createOutputCharactPlotDemo <- shiny::reactive({
    result <- data |>
      # filterData("summarise_characteristics_demographics", input)
      filterData("summarise_characteristics", input, "summarise_characteristics_demographics")|> 
      omopgenerics::filterSettings(additional == "")
    
    #filter group of cancer
    all_types <- unique(unlist(lapply(
      data$cancer_group_strata[input$summarise_characteristics_demographics_cancer_group], 
      `[[`, "types"
    )))
    result <- result |> 
      omopgenerics::filterGroup(cohort_name %in% all_types )
    #plot
    CohortCharacteristics::plotCharacteristics(
      result,
      plotStyle = input$summarise_characteristics_demographics_ggplot2_8_plotStyle,
      facet = input$summarise_characteristics_demographics_ggplot2_8_facet,
      colour = input$summarise_characteristics_demographics_ggplot2_8_colour
    )
  })
  output$summarise_characteristics_demographics_ggplot2_8 <- shiny::renderPlot({
    createOutputCharactPlotDemo()
  })
  output$summarise_characteristics_demographics_ggplot2_8_download <- shiny::downloadHandler(
    filename = paste0("output_ggplot2_summarise_characteristics_demographics.", "png"),
    content = function(file) {
      obj <- createOutputCharactPlotDemo()
      ggplot2::ggsave(
        filename = file,
        plot = obj,
        width = as.numeric(input$summarise_characteristics_demographics_ggplot2_8_download_width),
        height = as.numeric(input$summarise_characteristics_demographics_ggplot2_8_download_height),
        units = input$summarise_characteristics_demographics_ggplot2_8_download_units,
        dpi = as.numeric(input$summarise_characteristics_demographics_ggplot2_8_download_dpi)
      )
    }
  )

  ## CHARACTERISTICS: conditions & drugs####################################
  #tidy summarise_characteristics 
  getTidyDataSummariseCharacteristicsClinical <- shiny::reactive({
    res <- data |>
      # filterData("summarise_characteristics_clinical", input) |>
      filterData("summarise_characteristics", input, "summarise_characteristics_clinical") |>
      omopgenerics::filter(grepl("Condit|Medicat", variable_name)) |> 
      omopgenerics::addSettings() |>
      omopgenerics::splitAll() |>
      dplyr::select(!"result_id")
    #filter group of cancer
    all_types <- unique(unlist(lapply(
      data$cancer_group_strata[input$summarise_characteristics_clinical_cancer_group], 
      `[[`, "types"
    )))
    res <- res |>
      dplyr::filter( cohort_name %in% all_types)
    # columns to eliminate
    colsEliminate <- colnames(res)
    colsEliminate <- colsEliminate[!colsEliminate %in% c(
      input$summarise_characteristics_clinical_tidy_columns, "variable_name", "variable_level",
      "estimate_name", "estimate_type", "estimate_value"
    )]
    # pivot
    pivot <- input$summarise_characteristics_clinical_tidy_pivot
    if (pivot != "none") {
      vars <- switch(pivot,
                     "estimates" = "estimate_name",
                     "estimates and variables" = c("variable_name", "variable_level", "estimate_name")
      )
      res <- res |>
        visOmopResults::pivotEstimates(pivotEstimatesBy = vars)
    }
    res |>
      dplyr::select(!dplyr::all_of(colsEliminate))
  })
  output$summarise_characteristics_clinical_tidy <- DT::renderDT({
    DT::datatable(
      getTidyDataSummariseCharacteristicsClinical(),
      options = list(scrollX = TRUE),
      rownames = FALSE
    )
  })
  output$summarise_characteristics_clinical_tidy_download <- shiny::downloadHandler(
    filename = "tidy_summarise_characteristics_clinical.csv",
    content = function(file) {
      getTidyDataSummariseCharacteristicsClinical() |>
        readr::write_csv(file = file)
    }
  )
  #Table summarise_characteristics_clinical
  createOutputCharactCli <- shiny::reactive({
    result <- data |>
      # filterData("summarise_characteristics_clinical", input) |> 
      filterData("summarise_characteristics", input, "summarise_characteristics_clinical") |>
      omopgenerics::filter(grepl("Condit|Medicat", variable_name)) 
    #filter group of cancer
    all_types <- unique(unlist(lapply(
      data$cancer_group_strata[input$summarise_characteristics_clinical_cancer_group], 
      `[[`, "types"
    )))
    result <- result |> 
      omopgenerics::filterGroup(cohort_name %in% all_types )
    CohortCharacteristics::tableCharacteristics(
      result,
      header = input$summarise_characteristics_clinical_gt_7_header,
      groupColumn = input$summarise_characteristics_clinical_gt_7_groupColumn,
      hide = input$summarise_characteristics_clinical_gt_7_hide
    )
  })
  output$summarise_characteristics_clinical_gt_7 <- gt::render_gt({
    createOutputCharactCli()
  })
  output$summarise_characteristics_clinical_gt_7_download <- shiny::downloadHandler(
    filename = paste0("output_gt_summarise_characteristics_clinical.", input$summarise_characteristics_clinical_gt_7_download_type),
    content = function(file) {
      obj <- createOutputCharactCli()
      gt::gtsave(data = obj, filename = file)
    }
  )

  ## LSC: conditions ######################################
  #tidy summarise_lsc_conditions 
  getTidyDataSummariseLSCcond<- shiny::reactive({
    res <- data |>
      # filterData("summarise_large_scale_characteristics_conditions", input) |>
      filterData("summarise_large_scale_characteristics", input) |>
      omopgenerics::addSettings() |>
      omopgenerics::splitAll() |>
      dplyr::select(!"result_id")|>
      # dplyr::arrange(dplyr::desc(estimate_value))
      dplyr::mutate(estimate_value = as.numeric(estimate_value)) |>
      dplyr::group_by("variable_level")|>
      dplyr::arrange(dplyr::desc(estimate_value), .by_group = TRUE) |>
      dplyr::ungroup() |> 
      filter(table_name == "condition_occurrence")
    #filter group of cancer
    all_types <- unique(unlist(lapply(
      data$cancer_group_strata[input$summarise_large_scale_characteristics_cancer_group], 
      `[[`, "types"
    )))
    res <- res |>
      dplyr::filter( cohort_name %in% all_types)
    #columns to eliminate
    colsEliminate <- colnames(res)
    colsEliminate <- colsEliminate[!colsEliminate %in% c(
      input$summarise_lsc_conditions_tidy_columns,"cohort_name", "variable_name", "variable_level",
      "estimate_name", "estimate_type", "estimate_value"
    )]
    res |>
      dplyr::select(!dplyr::all_of(colsEliminate))
  })
  output$summarise_lsc_conditions_tidy <- DT::renderDT({
    DT::datatable(
      getTidyDataSummariseLSCcond(),
      options = list(scrollX = TRUE, order = list()),
      rownames = FALSE
    )
  })
  output$summarise_lsc_conditions_tidy_download <- shiny::downloadHandler(
    filename = "tidy_summarise_lsc_conditions.csv",
    content = function(file) {
      getTidyDataSummariseLSCcond() |>
        readr::write_csv(file = file)
    }
  )

  ## LSC: drugs ##########################################
  #tidy summarise_lsc_drugs
  getTidyDataSummariseLSCdrugs<- shiny::reactive({
    res <- data |>
      filterData("summarise_large_scale_characteristics", input) |>
      omopgenerics::addSettings() |>
      omopgenerics::splitAll() |>
      dplyr::select(!"result_id")|>
      # dplyr::arrange(dplyr::desc(estimate_value))
      dplyr::mutate(estimate_value = as.numeric(estimate_value)) |>
      dplyr::group_by("variable_level")|>
      dplyr::arrange(dplyr::desc(estimate_value), .by_group = TRUE) |>
      dplyr::ungroup() |> 
      filter(table_name == "drug_era")
    #filter group of cancer
    all_types <- unique(unlist(lapply(
      data$cancer_group_strata[input$summarise_large_scale_characteristics_cancer_group], 
      `[[`, "types"
    )))
    res <- res |>
      dplyr::filter( cohort_name %in% all_types)
    #columns to eliminate
    colsEliminate <- colnames(res)
    colsEliminate <- colsEliminate[!colsEliminate %in% c(
      input$summarise_lsc_drugs_tidy_columns, "cohort_name", "variable_name", "variable_level",
      "estimate_name", "estimate_type", "estimate_value"
    )]
    res |>
      dplyr::select(!dplyr::all_of(colsEliminate))
  })
  output$summarise_lsc_drugs_tidy <- DT::renderDT({
    DT::datatable(
      getTidyDataSummariseLSCdrugs(),
      options = list(scrollX = TRUE, order = list()),
      rownames = FALSE
    )
  })
  output$summarise_lsc_drugs_tidy_download <- shiny::downloadHandler(
    filename = "tidy_summarise_lsc_drugs.csv",
    content = function(file) {
      getTidyDataSummariseLSCdrugs() |>
        readr::write_csv(file = file)
    }
  )

  # INCIDENCE ##################################################################
  #tidy incidence
  getTidyDataIncidence <- shiny::reactive({
    res <- data |>
      filterData("incidence", input) |>
      omopgenerics::addSettings() |>
      omopgenerics::splitAll()|>
      dplyr::select(!"result_id")
    #filter group of cancer
    all_types <- unique(unlist(lapply(
      data$cancer_group_strata[input$incidence_cancer_group], 
      `[[`, "types"
    )))
    res <- res |>
      dplyr::filter(outcome_cohort_name %in% all_types)
    #filter strata
    if(!is.null(filterValues$incidence_strata)){
      if(is.null(input$incidence_strata_cols) ){
        #Filtra overall en todas las strata y elimina las columnas
        res <- res |>
          dplyr::filter(
            dplyr::if_all(
              dplyr::any_of(filterValues$incidence_strata),
              ~ .x == "overall"
            )
          ) |>
          dplyr::select(-dplyr::any_of(filterValues$incidence_strata)) 
      }else{
        #Filtra overall solo en las NO seleccionadas y elimina esas columnas
        cols_to_filter <- setdiff(filterValues$incidence_strata, input$incidence_strata_cols)
        res <- res |>
          dplyr::filter(
            dplyr::if_all(
              dplyr::any_of(cols_to_filter),
              ~ .x == "overall"
            )
          ) |>
          dplyr::select(-dplyr::any_of(cols_to_filter))
      }
    } 
    #columns to eliminate
    colsEliminate <- colnames(res)
    colsEliminate <- colsEliminate[!colsEliminate %in% c(
      input$incidence_tidy_columns,
      input$incidence_strata_cols %||% character(0),
      "variable_name", "variable_level",
      "estimate_name", "estimate_type", "estimate_value"
    )]
    #pivot
    pivot <- input$incidence_tidy_pivot
    if (pivot != "none") {
      vars <- switch(pivot,
        "estimates" = "estimate_name",
        "estimates and variables" = c("variable_name", "variable_level", "estimate_name")
      )
      res <- res |>
        visOmopResults::pivotEstimates(pivotEstimatesBy = vars)
    }
    #Change large  columns names
    if (any(grepl("Outcome", colnames(res)))) {
      cols_with_outcome <- grepl("Outcome", colnames(res))
      colnames(res)[cols_with_outcome] <- gsub("Outcome_", "", colnames(res)[cols_with_outcome])
      cols_with_denom <- grepl("Denominator", colnames(res))
      colnames(res)[cols_with_denom] <- gsub("Denominator_", "", colnames(res)[cols_with_denom])
      res |>
        dplyr::select(!dplyr::any_of(colsEliminate))
      
    } else {
      res |>
        dplyr::select(!dplyr::any_of(colsEliminate))
    }
  })
  output$incidence_tidy <- DT::renderDT({
    DT::datatable(
      getTidyDataIncidence(),
      options = list(scrollX = TRUE),
      rownames = FALSE
    )
  })
  output$incidence_tidy_download <- shiny::downloadHandler(
    filename = "tidy_incidence.csv",
    content = function(file) {
      getTidyDataIncidence() |>
        readr::write_csv(file = file)
    }
  )
  #Table incidence
  createOutputInc <- shiny::reactive({
      result <- data |>
        filterData("incidence", input) 
      #filter group of cancer
      all_types <- unique(unlist(lapply(
        data$cancer_group_strata[input$incidence_cancer_group], 
        `[[`, "types"
      )))
      result <- result |> 
        omopgenerics::filterGroup(outcome_cohort_name %in% all_types )
      #Filter survival strata
      colsToFilter <- setdiff(filterValues$incidence_strata, input$incidence_strata_cols)
      if (length(colsToFilter) > 0) {
        result <- result |>
          dplyr::filter(
            !strata_name %in% colsToFilter
          )
      }
      IncidencePrevalence::tableIncidence(
        result,
        header = input$incidence_gt_18_header,
        groupColumn = input$incidence_gt_18_groupColumn,
        settingsColumn = c("denominator_age_group", "denominator_sex"),
        # settingsColumn = c("denominator_age_group", "denominator_sex", "denominator_target_cohort_name"),
        hide = input$incidence_gt_18_hide
      )
  })
  output$incidence_gt_18 <- gt::render_gt({
    createOutputInc()
  })
  output$incidence_gt_18_download <- shiny::downloadHandler(
    filename = paste0("output_gt_incidence.", input$incidence_gt_18_download_type),
    content = function(file) {
      obj <- createOutputInc()
      gt::gtsave(data = obj, filename = file)
    }
  )

  ##Plot incidence
  createOutputIncPlot <- shiny::reactive({
    result <- data |>
      filterData("incidence", input) 
    #filter group of cancer
    all_types <- unique(unlist(lapply(
      data$cancer_group_strata[input$incidence_cancer_group], 
      `[[`, "types"
    )))
    result <- result |> 
      omopgenerics::filterGroup(outcome_cohort_name %in% all_types )
    #Filter strata manually
    if(!is.null(input$incidence_strata_cols)){
      result <- result |> 
        filter(strata_name %in% input$incidence_strata_cols)
    }else{
      result <- result |> 
        filter(strata_name =="overall")
    }
    IncidencePrevalence::plotIncidence(
      result,
      x = input$incidence_ggplot2_19_x,
      ribbon = input$incidence_ggplot2_19_ribbon,
      facet = input$incidence_ggplot2_19_facet,
      colour = c(input$incidence_ggplot2_19_colour) 
      # colour = c(input$incidence_ggplot2_19_colour, input$incidence_strata_cols) #force strata in colours
    )
  })
  output$incidence_ggplot2_19 <- shiny::renderPlot({
    createOutputIncPlot()
  })
  output$incidence_ggplot2_19_download <- shiny::downloadHandler(
    filename = paste0("output_ggplot2_incidence.", "png"),
    content = function(file) {
      obj <- createOutputIncPlot()
      ggplot2::ggsave(
        filename = file,
        plot = obj,
        width = as.numeric(input$incidence_ggplot2_19_download_width),
        height = as.numeric(input$incidence_ggplot2_19_download_height),
        units = input$incidence_ggplot2_19_download_units,
        dpi = as.numeric(input$incidence_ggplot2_19_download_dpi)
      )
    }
  )


  ## Incidence attrition  #########################################
  #tidy incidence_attrition 
  getTidyDataIncidenceAttrition <- shiny::reactive({
    res <- data |>
      filterData("incidence_attrition", input) |>
      omopgenerics::addSettings() |>
      omopgenerics::splitAll() |>
      dplyr::select(!"result_id")
    #filter group of cancer
    all_types <- unique(unlist(lapply(
      data$cancer_group_strata[input$incidence_attrition_cancer_group], 
      `[[`, "types"
    )))
    res <- res |>
      dplyr::filter( outcome_cohort_name %in% all_types)
    # columns to eliminate
    colsEliminate <- colnames(res)
    colsEliminate <- colsEliminate[!colsEliminate %in% c(
      input$incidence_attrition_tidy_columns, 
      "denominator_cohort_name", "outcome_cohort_name", 
      "reason","reason_id", "variable_name", "variable_level",
      "estimate_name", "estimate_type", "estimate_value"
    )]
    vars <- c("variable_name", "variable_level", "estimate_name")
    res <- res |>
      visOmopResults::pivotEstimates(pivotEstimatesBy = vars) 
    res |>
      dplyr::select(!dplyr::any_of(colsEliminate)) %>% 
      dplyr::rename(
        denominator = denominator_cohort_name,
        outcome = outcome_cohort_name,
        n_records = number_records_count,
        n_subjects = number_subjects_count,
        excluded_records = excluded_records_count,
        excluded_subjects = excluded_subjects_count)
  })
  output$incidence_attrition_tidy <- DT::renderDT({
    DT::datatable(
      getTidyDataIncidenceAttrition(),
      options = list(scrollX = TRUE),
      rownames = FALSE
    )
  })
  output$incidence_attrition_tidy_download <- shiny::downloadHandler(
    filename = "tidy_incidence_attrition.csv",
    content = function(file) {
      getTidyDataIncidenceAttrition() |>
        readr::write_csv(file = file)
    }
  )
 
  ## PREVALENCE ##############################################################
  #tidy prevalence 
  getTidyDataPrevalence <- shiny::reactive({
    res <- data |>
      filterData("prevalence", input) |>
      omopgenerics::addSettings() |>
      omopgenerics::splitAll() |>
      dplyr::select(!"result_id")
    #filter group of cancer
    all_types <- unique(unlist(lapply(
      data$cancer_group_strata[input$prevalence_cancer_group], 
      `[[`, "types"
    )))
    res <- res |>
      dplyr::filter( outcome_cohort_name %in% all_types)
    # columns to eliminate
    colsEliminate <- colnames(res)
    colsEliminate <- colsEliminate[!colsEliminate %in% c(
      input$prevalence_tidy_columns,  "variable_name", "variable_level",
      "estimate_name", "estimate_type", "estimate_value"
    )]
    # pivot
    pivot <- input$prevalence_tidy_pivot
    if (pivot != "none") {
      vars <- switch(pivot,
                     "estimates" = "estimate_name",
                     "estimates and variables" = c("variable_name", "variable_level", "estimate_name")
      )
      res <- res |>
        visOmopResults::pivotEstimates(pivotEstimatesBy = vars)
    }
    
    if (any(grepl("Outcome", colnames(res)))) {
      cols_with_outcome <- grepl("Outcome", colnames(res))
      colnames(res)[cols_with_outcome] <- gsub("Outcome_", "", colnames(res)[cols_with_outcome])
      cols_with_denom <- grepl("Denominator", colnames(res))
      colnames(res)[cols_with_denom] <- gsub("Denominator_", "", colnames(res)[cols_with_denom])
      res |>
        dplyr::select(!dplyr::all_of(colsEliminate))
    } else {
      res |>
        dplyr::select(!dplyr::all_of(colsEliminate))
    }
  })
  
  output$prevalence_tidy <- DT::renderDT({
    DT::datatable(
      getTidyDataPrevalence(),
      options = list(scrollX = TRUE),
      rownames = FALSE
    )
  })
  output$prevalence_tidy_download <- shiny::downloadHandler(
    filename = "tidy_prevalence.csv",
    content = function(file) {
      getTidyDataPrevalence() |>
        readr::write_csv(file = file)
    }
  )
  
  ##table prevalence
  createOutputPrev <- shiny::reactive({
    result <- data |>
      filterData("prevalence", input)
    #filter group of cancer
    all_types <- unique(unlist(lapply(
      data$cancer_group_strata[input$prevalence_cancer_group], 
      `[[`, "types"
    )))
    result <- result |> 
      omopgenerics::filterGroup(outcome_cohort_name %in% all_types )
    #table
    IncidencePrevalence::tablePrevalence(
      result,
      header = input$prevalence_gt_18_header,
      groupColumn = input$prevalence_gt_18_groupColumn,
      settingsColumn = c("denominator_age_group", "denominator_sex"),
      hide = input$prevalence_gt_18_hide
    )
  })
  output$prevalence_gt_18 <- gt::render_gt({
    createOutputPrev()
  })
  output$prevalence_gt_18_download <- shiny::downloadHandler(
    filename = paste0("output_gt_prevalence.", input$prevalence_gt_18_download_type),
    content = function(file) {
      obj <- createOutputPrev()
      gt::gtsave(data = obj, filename = file)
    }
  )
  
  ##Plot prevalence
  createOutputPrevPlot <- shiny::reactive({
    result <- data |>
      filterData("prevalence", input)
    #filter group of cancer
    all_types <- unique(unlist(lapply(
      data$cancer_group_strata[input$prevalence_cancer_group], 
      `[[`, "types"
    )))
    result <- result |> 
      omopgenerics::filterGroup(outcome_cohort_name %in% all_types )
    #plot
    IncidencePrevalence::plotPrevalence(
      result,
      x = input$prevalence_ggplot2_19_x,
      ribbon = input$prevalence_ggplot2_19_ribbon,
      facet = input$prevalence_ggplot2_19_facet,
      colour = input$prevalence_ggplot2_19_colour
    )+
    labs(y = "5-year partial prevalence")
  })
  output$prevalence_ggplot2_19 <- shiny::renderPlot({
    createOutputPrevPlot()
  })
  output$prevalence_ggplot2_19_download <- shiny::downloadHandler(
    filename = paste0("output_ggplot2_prevalence.", "png"),
    content = function(file) {
      obj <- createOutputPrevPlot()
      ggplot2::ggsave(
        filename = file,
        plot = obj,
        width = as.numeric(input$prevalence_ggplot2_19_download_width),
        height = as.numeric(input$prevalence_ggplot2_19_download_height),
        units = input$prevalence_ggplot2_19_download_units,
        dpi = as.numeric(input$prevalence_ggplot2_19_download_dpi)
      )
    }
  )
  
  ## Prevalence attrition ########################################
  ##tidy prevalence_attrition 
  getTidyDataPrevalenceAttrition <- shiny::reactive({
    res <- data |>
      filterData("prevalence_attrition", input) |>
      omopgenerics::addSettings() |>
      omopgenerics::splitAll() |>
      dplyr::select(!"result_id")
    #filter group of cancer
    all_types <- unique(unlist(lapply(
      data$cancer_group_strata[input$prevalence_attrition_cancer_group], 
      `[[`, "types"
    )))
    res <- res |>
      dplyr::filter(outcome_cohort_name %in% all_types)
    # columns to eliminate
    colsEliminate <- colnames(res)
    colsEliminate <- colsEliminate[!colsEliminate %in% c(
      input$prevalence_attrition_tidy_columns,"denominator_cohort_name", "outcome_cohort_name", 
      "reason","reason_id", "variable_name", "variable_level",
      "estimate_name", "estimate_type", "estimate_value"
    )]
    vars <- c("variable_name", "variable_level", "estimate_name")
    res <- res |>
      visOmopResults::pivotEstimates(pivotEstimatesBy = vars) 
    res |>
      dplyr::select(!dplyr::all_of(colsEliminate)) %>% 
      dplyr::rename(
        denominator = denominator_cohort_name,
        outcome = outcome_cohort_name,
        n_records = number_records_count,
        n_subjects = number_subjects_count,
        excluded_records = excluded_records_count,
        excluded_subjects = excluded_subjects_count)
  })
  output$prevalence_attrition_tidy <- DT::renderDT({
    DT::datatable(
      getTidyDataPrevalenceAttrition(),
      options = list(scrollX = TRUE),
      rownames = FALSE
    )
  })
  output$prevalence_attrition_tidy_download <- shiny::downloadHandler(
    filename = "tidy_prevalence_attrition.csv",
    content = function(file) {
      getTidyDataPrevalenceAttrition() |>
        readr::write_csv(file = file)
    }
  )

  ## SURVIVAL ##################################################################
  ## Survival summary ----
  ##survival summary tidy
  getTidyDataSurvivalSummary <- shiny::reactive({
    res <- data |>
      # filterData("survival_summary", input) |>
      filterData("survival", input, "survival_summary") |>
      omopgenerics::filterSettings(result_type == "survival_summary") |> 
      omopgenerics::addSettings() |>
      omopgenerics::splitAll() |>
      dplyr::select(!"result_id")
    
    #filter group of cancer
    all_types <- unique(unlist(lapply(
      data$cancer_group_strata[input$survival_summary_cancer_group], 
      `[[`, "types"
    )))
    res <- res |>
      dplyr::filter( target_cohort %in% all_types)

    #Filter survival strata
    colsToFilter <- setdiff(filterValues$survival_strata, input$survival_summary_strata)
    if(length(colsToFilter) != 0){
      res <- res |>
        dplyr::filter(
          dplyr::if_all(
            dplyr::all_of(colsToFilter),
            ~ .x == "overall"
          )
        ) |>
        dplyr::select(-dplyr::all_of(colsToFilter))
    }
    #columns to eliminate
    colsEliminate <- colnames(res)
    colsEliminate <- colsEliminate[!colsEliminate %in% c(
      input$survival_summary_tidy_columns, input$survival_summary_strata, "estimate_value"
    )]
    #pivot
    pivot <- input$survival_summary_tidy_pivot
    if (pivot != "none") {
      vars <- switch(pivot,
                     "estimates" = "estimate_name",
                     "estimates and variables" = c("variable_name", "variable_level", "estimate_name")
      )
      res <- res |>
        visOmopResults::pivotEstimates(pivotEstimatesBy = vars)
    }
    res |>
      dplyr::select(!dplyr::any_of(colsEliminate))
  })
  output$survival_tidy <- DT::renderDT({
    DT::datatable(
      getTidyDataSurvivalSummary(),
      options = list(scrollX = TRUE),
      rownames = FALSE
    )
  })
  output$survival_tidy_download <- shiny::downloadHandler(
    filename = "tidy_survival.csv",
    content = function(file) {
      getTidyDataSurvivalSummary() |>
        readr::write_csv(file = file)
    }
  )
  ##survival table 
  createOutputSurv <- shiny::reactive({
    #cancer group
    all_types <- unique(unlist(lapply(
      data$cancer_group_strata[input$survival_summary_cancer_group], 
      `[[`, "types"
    )))
    #results
    result <- data$survival |> 
      dplyr::filter(cdm_name %in% input$survival_summary_grouping_cdm_name) |> 
      omopgenerics::filterGroup(target_cohort %in% all_types) |>
      omopgenerics::filterGroup(
        target_cohort %in% input$survival_summary_grouping_target_cohort
      ) |> 
      omopgenerics::filterStrata(
        sex %in% input$survival_summary_grouping_sex,
        age_gr %in% input$survival_summary_grouping_age_gr,
        age_gr_broad %in% input$survival_summary_grouping_age_gr_broad,
        study_period %in% input$survival_summary_grouping_study_period,
        year %in% input$survival_summary_grouping_year
      ) 
    #Filter survival strata
    colsToFilter <- setdiff(filterValues$survival_strata, input$survival_summary_strata)
    if (length(colsToFilter) > 0) {
      for (col in colsToFilter) {
        result <- result |>
          omopgenerics::filterStrata(
            # !!rlang::sym(col) %in% "overall"
            !grepl(paste(colsToFilter, collapse = "|"), strata_name)
          )
      }
    }
    # CohortSurvival::tableSurvival(
    #   result,
    #   header = input$survival_summary_gt_18_header,
    #   groupColumn = input$survival_summary_gt_18_groupColumn
    # )
    
    tbl <- CohortSurvival::tableSurvival(
      result,
      header = input$survival_summary_gt_18_header,
      groupColumn = input$survival_summary_gt_18_groupColumn
    )
    
    if (length(input$survival_summary_gt_18_hideColumn)!=0) {
      hide_cols_formatted <- input$survival_summary_gt_18_hideColumn |>
        stringr::str_replace_all("_", " ") |>
        stringr::str_to_sentence()
      
      tbl <- tbl |> gt::cols_hide(columns = hide_cols_formatted)
    }
    
    tbl
    
  })
  output$survival_summary_gt_18 <- gt::render_gt({
    createOutputSurv()
  })
  
  ## Survival risk -----
  
  #survival events tidy 
  getTidyDataSurvivalRisk <-  shiny::reactive({
    res <- data |>
      # filterData("survival_events", input) |>
      filterData("survival", input, "survival_events") |>
      omopgenerics::filterSettings(result_type == "survival_events") |>
      omopgenerics::addSettings() |>
      omopgenerics::splitAll() |>
      dplyr::select(!"result_id")
    
    #filter group of cancer
    all_types <- unique(unlist(lapply(
      data$cancer_group_strata[input$survival_events_cancer_group], 
      `[[`, "types"
    )))
    res <- res |>
      dplyr::filter( target_cohort %in% all_types)
    
    #Filter survival strata
    colsToFilter <- setdiff(filterValues$survival_strata, input$survival_events_strata)
    if(length(colsToFilter) != 0){
      res <- res |>
        dplyr::filter(
          dplyr::if_all(
            dplyr::all_of(colsToFilter),
            ~ .x == "overall"
          )
        ) |>
        dplyr::select(-dplyr::all_of(colsToFilter))
    }
    #columns to eliminate
    colsEliminate <- colnames(res)
    colsEliminate <- colsEliminate[!colsEliminate %in% c(
      input$survival_events_tidy_columns, input$survival_events_strata, "estimate_value"
    )]
    #pivot
    pivot <- input$survival_events_tidy_pivot
    if (pivot != "none") {
      vars <- switch(pivot,
                     "estimates" = "estimate_name",
                     "estimates and variables" = c("variable_name", "variable_level", "estimate_name")
      )
      res <- res |>
        visOmopResults::pivotEstimates(pivotEstimatesBy = vars)
    }
    res |>
      dplyr::select(!dplyr::any_of(colsEliminate))
  })
  output$survival_risk_tidy <- DT::renderDT({
    DT::datatable(
      getTidyDataSurvivalRisk(),
      options = list(scrollX = TRUE),
      rownames = FALSE
    )
  })

  #survival risk table 
  createOutputSurvRisk <- shiny::reactive({
    #filter group of cancer
    all_types <- unique(unlist(lapply(
      data$cancer_group_strata[input$survival_events_cancer_group], 
      `[[`, "types"
    )))
    #results
    result <- data$survival |> 
      dplyr::filter(cdm_name %in% input$survival_events_grouping_cdm_name) |> 
      omopgenerics::filterGroup(target_cohort %in% all_types) |>
      omopgenerics::filterGroup(
        target_cohort %in% input$survival_events_grouping_target_cohort
      ) |> 
      omopgenerics::filterStrata(
        sex %in% input$survival_events_grouping_sex,
        age_gr %in% input$survival_events_grouping_age_gr,
        age_gr_broad %in% input$survival_events_grouping_age_gr_broad,
        study_period %in% input$survival_events_grouping_study_period,
        year %in% input$survival_events_grouping_year
      ) 
    #Filter survival strata
    colsToFilter <- setdiff(filterValues$survival_strata, input$survival_events_strata)
    if (length(colsToFilter) > 0) {
      for (col in colsToFilter) {
        result <- result |>
          omopgenerics::filterStrata(
            # !!rlang::sym(col) %in% "overall"
            !grepl(paste(colsToFilter, collapse = "|"), strata_name)
          )
      }
    }
    CohortSurvival::riskTable(
      result,
      # times = ,
      header = input$survival_events_gt_18_header,
      groupColumn = input$survival_events_gt_18_groupColumn
    )
    
  })
  output$survival_events_gt_18 <- gt::render_gt({
    createOutputSurvRisk()
  })
  
  ## survival probability ----
  ##tidy table
  getTidyDataSurvivalProb <-  shiny::reactive({
    res <- data |>
      # filterData("survival_estimates", input) |>
      filterData("survival", input, "survival_estimates") |>
      omopgenerics::filterSettings(result_type == "survival_estimates") |>
      omopgenerics::addSettings() |>
      omopgenerics::splitAll() |>
      dplyr::select(!"result_id")
    
    #filter group of cancer
    all_types <- unique(unlist(lapply(
      data$cancer_group_strata[input$survival_estimates_cancer_group], 
      `[[`, "types"
    )))
    res <- res |>
      dplyr::filter( target_cohort %in% all_types)
    
    #Filter survival strata
    colsToFilter <- setdiff(filterValues$survival_strata, input$survival_estimates_strata)
    if(length(colsToFilter) != 0){
      res <- res |>
        dplyr::filter(
          dplyr::if_all(
            dplyr::all_of(colsToFilter),
            ~ .x == "overall"
          )
        ) |>
        dplyr::select(-dplyr::all_of(colsToFilter))
    }
    #columns to eliminate
    colsEliminate <- colnames(res)
    colsEliminate <- colsEliminate[!colsEliminate %in% c(
      input$survival_estimates_tidy_columns, input$survival_estimates_strata, "estimate_value"
    )]
    #pivot
    pivot <- input$survival_estimates_tidy_pivot
    if (pivot != "none") {
      vars <- switch(pivot,
                     "estimates" = "estimate_name",
                     "estimates and variables" = c("variable_name", "variable_level", "estimate_name")
      )
      res <- res |>
        visOmopResults::pivotEstimates(pivotEstimatesBy = vars)
    }
    res |>
      dplyr::select(!dplyr::any_of(colsEliminate))
  })
  output$survival_estimates_tidy <- DT::renderDT({
    DT::datatable(
      getTidyDataSurvivalProb(),
      options = list(scrollX = TRUE),
      rownames = FALSE
    )
  })

  ##Survival plots 
  createOutputSurvPlot <- shiny::reactive({
    #filter group of cancer
    all_types <- unique(unlist(lapply(
      data$cancer_group_strata[input$survival_estimates_cancer_group], 
      `[[`, "types"
    )))
    #results
    result <- data$survival |> 
      dplyr::filter(cdm_name %in% input$survival_estimates_grouping_cdm_name) |> 
      omopgenerics::filterGroup(target_cohort %in% all_types) |>
      omopgenerics::filterGroup(
        target_cohort %in% input$survival_estimates_grouping_target_cohort
      ) |> 
      omopgenerics::filterStrata(
        sex %in% input$survival_estimates_grouping_sex,
        age_gr %in% input$survival_estimates_grouping_age_gr,
        age_gr_broad %in% input$survival_estimates_grouping_age_gr_broad,
        study_period %in% input$survival_estimates_grouping_study_period,
        year %in% input$survival_estimates_grouping_year
      ) 
    #Filter survival strata
    colsToFilter <- setdiff(filterValues$survival_strata, input$survival_estimates_strata)
    if (length(colsToFilter) > 0) {
      result <- result |>
        omopgenerics::filterStrata(
          # !!rlang::sym(col) %in% "overall"
          !grepl(paste(colsToFilter, collapse = "|"), strata_name)
        )
    }
    # result <- data |>
    #   filterData("survival", input)
    CohortSurvival::plotSurvival(
      result,
      ribbon = input$survival_ggplot2_19_ribbon,
      facet = input$survival_ggplot2_19_facet,
      colour = input$survival_ggplot2_19_colour
      #   ,cumulativeFailure = FALSE,
      )

  })
  output$survival_ggplot2_19 <- shiny::renderPlot({
    createOutputSurvPlot()
  })
  output$survival_ggplot2_19_download <- shiny::downloadHandler(
    filename = paste0("output_ggplot2_survival.", "png"),
    content = function(file) {
      obj <- createOutputSurvPlot()
      ggplot2::ggsave(
        filename = file,
        plot = obj,
        width = as.numeric(input$survival_ggplot2_19_download_width),
        height = as.numeric(input$survival_ggplot2_19_download_height),
        units = input$survival_ggplot2_19_download_units,
        dpi = as.numeric(input$survival_ggplot2_19_download_dpi)
      )
    }
  )
  
  ## Survival attrition #########################################
  ##tidy survival_attrition 
  getTidyDataSurvivalAttrition <- shiny::reactive({
    res <- data |>
      # filterData("survival_attrition", input) |>
      filterData("survival", input, "survival_attrition") |>
      omopgenerics::filterSettings(result_type == "survival_attrition") |>
      omopgenerics::addSettings() |>
      omopgenerics::splitAll() |>
      dplyr::select(!"result_id")
    
    #filter group of cancer
    all_types <- unique(unlist(lapply(
      data$cancer_group_strata[input$survival_attrition_cancer_group], 
      `[[`, "types"
    )))
    res <- res |>
      dplyr::filter(sub("_.*", "", target_cohort) %in% all_types)
    
    # columns to eliminate
    colsEliminate <- colnames(res)
    colsEliminate <- colsEliminate[!colsEliminate %in% c(
      input$survival_attrition_tidy_columns,"target_cohort",  
      "reason","reason_id", "variable_name", "variable_level",
      "estimate_name", "estimate_type", "estimate_value"
    )]
    
    vars <- c("variable_name", "variable_level", "estimate_name")
    
    res <- res |>
      visOmopResults::pivotEstimates(pivotEstimatesBy = vars) 
    
    res |>
      dplyr::select(!dplyr::all_of(colsEliminate)) 
      # %>% 
      # dplyr::rename(denominator = denominator_cohort_name, 
      #               outcome = outcome_cohort_name,
      #               n_records = number_records_count,
      #               n_subjects = number_subjects_count,
      #               excluded_records = excluded_records_count,
      #               excluded_subjects = excluded_subjects_count)
  })
  output$survival_attrition_tidy <- DT::renderDT({
    DT::datatable(
      getTidyDataSurvivalAttrition(),
      options = list(scrollX = TRUE),
      rownames = FALSE
    )%>%
      DT::formatStyle(
        'reason',  
        `min-width` = '400px',
        # `max-width` = '500px',
        `white-space` = 'normal',
        `word-wrap` = 'break-word'
      )
  })
  output$survival_attrition_tidy_download <- shiny::downloadHandler(
    filename = "tidy_survival_attrition.csv",
    content = function(file) {
      getTidyDataSurvivalAttrition() |>
        readr::write_csv(file = file)
    }
  )
 

  
}
