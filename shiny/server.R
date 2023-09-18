#### SERVER ------
server <-	function(input, output, session) {
  
  # prevalence disease
  get_prevalence_estimates<-reactive({
    
    table<-prevalence_estimates %>% 
      # first deselect settings which did not vary for this study
      select(!c(analysis_id, analysis_outcome_lookback_days,
                analysis_interval,
                analysis_time_point,
                analysis_complete_database_intervals,
                denominator_start_date,
                denominator_end_date,
                analysis_type, analysis_full_contribution)) %>% 
      filter(database_name %in% input$prevalence_database_name_selector)  %>% 
      filter(as.character(prevalence_start_date) %in% input$prevalence_start_date_selector)  %>% 
      filter(denominator_age_group %in% input$prevalence_denominator_age_group_selector)     %>% 
      filter(denominator_sex %in% input$prevalence_denominator_sex_selector)     %>% 
      filter(denominator_days_prior_history %in% input$prevalence_denominator_days_prior_history_selector)   %>% 
      filter(outcome_cohort_name %in% input$prevalence_outcome_cohort_name_selector)  
    
    table
  }) 
  
  output$tbl_prevalence_estimates<-  renderDataTable({
    
    table<-get_prevalence_estimates() 
    
    validate(need(ncol(table)>1, 
                  "No results for selected inputs"))
    
    table <- table %>% 
      mutate(prevalence=paste0(nice.num3(prevalence*100), "%")) %>% 
      mutate(prevalence_95CI_lower=paste0(nice.num3(prevalence_95CI_lower*100), "%")) %>% 
      mutate(prevalence_95CI_upper=paste0(nice.num3(prevalence_95CI_upper*100), "%")) %>% 
      mutate(prevalence= ifelse(!is.na(prevalence),
                                paste0(prevalence, " (",
                                       prevalence_95CI_lower," to ", 
                                       prevalence_95CI_upper, " )"))) %>% 
      select(!c("prevalence_95CI_lower", "prevalence_95CI_upper",
                "cohort_obscured", "result_obscured",
                "analysis_min_cell_count",
                "denominator_cohort_id", "outcome_cohort_id")) 
    datatable(table,
              rownames= FALSE,
              # colnames = c('Variable' = 1),
              extensions = 'Buttons',
              # options = list(lengthChange = FALSE,
              #                pageLength = 40,
              #                buttons = c('copy', 'csv', 'excel')))
              options = list(lengthChange = FALSE,
                             pageLength = 94,
                             dom = 'tB',
                             buttons = list(list(extend = "excel", 
                                                 text = "Download table as excel",
                                                 filename = "PatientProfiles.csv"))
              ))
  } )
  
  output$plot_prevalence_estimates<- renderPlotly({ 
    
    table<-get_prevalence_estimates() 
    validate(need(ncol(table)>1, 
                  "No results for selected inputs"))
    
    if(is.null(input$prevalence_plot_group)){
      if(!is.null(input$prevalence_plot_facet)){
        p<-table %>% 
          unite("facet_var", 
                c(all_of(input$prevalence_plot_facet)), remove = FALSE, sep = "; ") %>% 
          ggplot(aes_string(x=input$prevalence_x_axis, y="prevalence",
                            ymin = "prevalence_95CI_lower",
                            ymax = "prevalence_95CI_upper")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0) +
          facet_wrap(vars(facet_var),ncol = 2)+
          scale_y_continuous(
            labels = scales::percent,
            limits = c(0, NA)
          ) +
          theme_bw()
      } else{
        p<-table %>% 
          ggplot(aes_string(x=input$prevalence_x_axis, y="prevalence",
                            ymin = "prevalence_95CI_lower",
                            ymax = "prevalence_95CI_upper")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0) +
          scale_y_continuous(
            labels = scales::percent,
            limits = c(0, NA)
          ) +
          theme_bw()        
      }
    } 
    
    
    if(!is.null(input$prevalence_plot_group) ){ 
      
      if(is.null(input$prevalence_plot_facet) ){ 
        p<-table %>% 
          unite("Group", 
                c(all_of(input$prevalence_plot_group)), remove = FALSE, sep = "; ") %>% 
          ggplot(aes_string(x=input$prevalence_x_axis, y="prevalence",
                            ymin = "prevalence_95CI_lower",
                            ymax = "prevalence_95CI_upper",
                            group="Group",
                            colour="Group")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0, position=position_dodge(width=1)) +
          theme_bw()
      }
      
      if(!is.null(input$prevalence_plot_facet) ){
        if(!is.null(input$prevalence_plot_group) ){ 
          p<-table %>% 
            unite("Group", 
                  c(all_of(input$prevalence_plot_group)), remove = FALSE, sep = "; ") %>% 
            unite("facet_var", 
                  c(all_of(input$prevalence_plot_facet)), remove = FALSE, sep = "; ") %>% 
            ggplot(aes_string(x=input$prevalence_x_axis, y="prevalence",
                              ymin = "prevalence_95CI_lower",
                              ymax = "prevalence_95CI_upper",
                              group="Group",
                              colour="Group")) +
            geom_point(position=position_dodge(width=1))+
            geom_errorbar(width=0, position=position_dodge(width=1)) +
            facet_wrap(vars(facet_var),ncol = 2)+  
            scale_y_continuous(
              labels = scales::percent,
              limits = c(0, NA)
            )  +
            theme_bw()
        }
      }
      
    }
    
    p
    
  })
  
  get_prevalence_attrition<-reactive({
    
    table<-prevalence_attrition %>% 
      # first deselect settings which did not vary for this study
      select(!c(analysis_id, analysis_outcome_lookback_days,
                analysis_interval,
                analysis_time_point,
                analysis_complete_database_intervals,
                denominator_start_date,
                denominator_end_date)) %>% 
      filter(database_name %in% input$prevalence_database_name_selector)    %>% 
      filter(denominator_age_group %in% input$prevalence_denominator_age_group_selector)     %>% 
      filter(denominator_sex %in% input$prevalence_denominator_sex_selector)     %>% 
      filter(denominator_days_prior_history %in% input$prevalence_denominator_days_prior_history_selector)     %>% 
      filter(outcome_cohort_name %in% input$prevalence_outcome_cohort_name_selector)  
    
    table
  }) 
  
  output$tbl_prevalence_attrition<-  renderDataTable({
    
    table<-get_prevalence_attrition() 
    
    validate(need(ncol(table)>1, 
                  "No results for selected inputs"))
    
    table <- table %>% 
      select(!c("analysis_full_contribution","analysis_min_cell_count",
                "denominator_cohort_id", "outcome_cohort_id")) 
    
    datatable(table,
              rownames= FALSE,
              # colnames = c('Variable' = 1),
              extensions = 'Buttons',
              # options = list(lengthChange = FALSE,
              #                pageLength = 40,
              #                buttons = c('copy', 'csv', 'excel')))
              options = list(lengthChange = FALSE,
                             pageLength = 94,
                             dom = 'tB',
                             buttons = list(list(extend = "excel", 
                                                 text = "Download table as excel",
                                                 filename = "PatientProfiles.csv"))
              ))
  })
  
  # prevalence drugs
  get_prevalence_estimates_drugs<-reactive({
    
    table1<-prevalence_estimates_drugs %>%
      # first deselect settings which did not vary for this study
      select(!c(analysis_id, analysis_outcome_lookback_days,
                analysis_interval,
                analysis_time_point,
                analysis_complete_database_intervals,
                denominator_start_date,
                denominator_end_date,
                analysis_type, analysis_full_contribution)) %>%
      filter(database_name %in% input$prevalence_database_name_selector_drugs)  %>%
      filter(as.character(prevalence_start_date) %in% input$prevalence_start_date_selector_drugs)  %>%
      filter(denominator_age_group %in% input$prevalence_denominator_age_group_selector_drugs)     %>%
      filter(denominator_sex %in% input$prevalence_denominator_sex_selector_drugs)     %>%
      filter(denominator_days_prior_history %in% input$prevalence_denominator_days_prior_history_selector_drugs)   %>%
      filter(outcome_cohort_name %in% input$prevalence_outcome_cohort_name_selector_drugs)
    table1
  })
  
  output$tbl_prevalence_estimates_drugs<-  renderDataTable({
    
    table1<-get_prevalence_estimates_drugs()
    
    validate(need(ncol(table1)>1,
                  "No results for selected inputs"))
    
    table1 <- table1 %>%
      mutate(prevalence=paste0(nice.num3(prevalence*100), "%")) %>%
      mutate(prevalence_95CI_lower=paste0(nice.num3(prevalence_95CI_lower*100), "%")) %>%
      mutate(prevalence_95CI_upper=paste0(nice.num3(prevalence_95CI_upper*100), "%")) %>%
      mutate(prevalence= ifelse(!is.na(prevalence),
                                paste0(prevalence, " (",
                                       prevalence_95CI_lower," to ",
                                       prevalence_95CI_upper, " )"))) %>%
      select(!c("prevalence_95CI_lower", "prevalence_95CI_upper",
                "cohort_obscured", "result_obscured",
                "analysis_min_cell_count",
                "denominator_cohort_id", "outcome_cohort_id"))
    datatable(table1,
              rownames= FALSE,
              # colnames = c('Variable' = 1),
              extensions = 'Buttons',
              # options = list(lengthChange = FALSE,
              #                pageLength = 40,
              #                buttons = c('copy', 'csv', 'excel')))
              options = list(lengthChange = FALSE,
                             pageLength = 94,
                             dom = 'tB',
                             buttons = list(list(extend = "excel",
                                                 text = "Download table as excel",
                                                 filename = "PatientProfiles.csv"))
              ))
  } )
  
  output$plot_prevalence_estimates_drugs<- renderPlotly({
    
    table<-get_prevalence_estimates_drugs()
    validate(need(ncol(table)>1,
                  "No results for selected inputs"))
    
    if(is.null(input$prevalence_plot_group_drugs)){
      if(!is.null(input$prevalence_plot_facet_drugs)){
        p<-table %>%
          unite("facet_var",
                c(all_of(input$prevalence_plot_facet_drugs)), remove = FALSE, sep = "; ") %>%
          ggplot(aes_string(x=input$prevalence_x_axis_drugs, y="prevalence",
                            ymin = "prevalence_95CI_lower",
                            ymax = "prevalence_95CI_upper")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0) +
          facet_wrap(vars(facet_var),ncol = 2)+
          scale_y_continuous(
            labels = scales::percent,
            limits = c(0, NA)
          ) +
          theme_bw()
      } else{
        p<-table %>%
          ggplot(aes_string(x=input$prevalence_x_axis_drugs, y="prevalence",
                            ymin = "prevalence_95CI_lower",
                            ymax = "prevalence_95CI_upper")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0) +
          scale_y_continuous(
            labels = scales::percent,
            limits = c(0, NA)
          ) +
          theme_bw()
      }
    }
    
    
    if(!is.null(input$prevalence_plot_group_drugs) ){
      
      if(is.null(input$prevalence_plot_facet_drugs) ){
        p<-table %>%
          unite("Group",
                c(all_of(input$prevalence_plot_group_drugs)), remove = FALSE, sep = "; ") %>%
          ggplot(aes_string(x=input$prevalence_x_axis_drugs, y="prevalence",
                            ymin = "prevalence_95CI_lower",
                            ymax = "prevalence_95CI_upper",
                            group="Group",
                            colour="Group")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0, position=position_dodge(width=1)) +
          theme_bw()
      }
      
      if(!is.null(input$prevalence_plot_facet_drugs) ){
        if(!is.null(input$prevalence_plot_group_drugs) ){
          p<-table %>%
            unite("Group",
                  c(all_of(input$prevalence_plot_group_drugs)), remove = FALSE, sep = "; ") %>%
            unite("facet_var",
                  c(all_of(input$prevalence_plot_facet_drugs)), remove = FALSE, sep = "; ") %>%
            ggplot(aes_string(x=input$prevalence_x_axis_drugs, y="prevalence",
                              ymin = "prevalence_95CI_lower",
                              ymax = "prevalence_95CI_upper",
                              group="Group",
                              colour="Group")) +
            geom_point(position=position_dodge(width=1))+
            geom_errorbar(width=0, position=position_dodge(width=1)) +
            facet_wrap(vars(facet_var),ncol = 2)+
            scale_y_continuous(
              labels = scales::percent,
              limits = c(0, NA)
            )  +
            theme_bw()
        }
      }
      
    }
    
    p
    
  })
  
  get_prevalence_attrition_drugs<-reactive({
    
    table<-prevalence_attrition_drugs %>%
      # first deselect settings which did not vary for this study
      select(!c(analysis_id, analysis_outcome_lookback_days,
                analysis_interval,
                analysis_time_point,
                analysis_complete_database_intervals,
                denominator_start_date,
                denominator_end_date)) %>%
      filter(database_name %in% input$prevalence_database_name_selector_drugs)    %>%
      filter(denominator_age_group %in% input$prevalence_denominator_age_group_selector_drugs)     %>%
      filter(denominator_sex %in% input$prevalence_denominator_sex_selector_drugs)     %>%
      filter(denominator_days_prior_history %in% input$prevalence_denominator_days_prior_history_selector_drugs)     %>%
      filter(outcome_cohort_name %in% input$prevalence_outcome_cohort_name_selector_drugs)
    
    table
  })
  
  output$tbl_prevalence_attrition_drugs<-  renderDataTable({
    
    table<-get_prevalence_attrition_drugs()
    
    validate(need(ncol(table)>1,
                  "No results for selected inputs"))
    
    table <- table %>%
      select(!c("analysis_full_contribution","analysis_min_cell_count",
                "denominator_cohort_id", "outcome_cohort_id"))
    
    datatable(table,
              rownames= FALSE,
              # colnames = c('Variable' = 1),
              extensions = 'Buttons',
              # options = list(lengthChange = FALSE,
              #                pageLength = 40,
              #                buttons = c('copy', 'csv', 'excel')))
              options = list(lengthChange = FALSE,
                             pageLength = 94,
                             dom = 'tB',
                             buttons = list(list(extend = "excel",
                                                 text = "Download table as excel",
                                                 filename = "PatientProfiles.csv"))
              ))
  })
  
  # prevalence PD
  get_prevalence_estimates_PD<-reactive({
    
    table1<-prevalence_estimates_drugPD %>%
      # first deselect settings which did not vary for this study
      select(!c(analysis_id, analysis_outcome_lookback_days,
                analysis_interval,
                analysis_time_point,
                analysis_complete_database_intervals,
                denominator_start_date,
                denominator_end_date,
                analysis_type, analysis_full_contribution)) %>%
      filter(database_name %in% input$prevalence_database_name_selector_PD)  %>%
      filter(as.character(prevalence_start_date) %in% input$prevalence_start_date_selector_PD)  %>%
      filter(denominator_age_group %in% input$prevalence_denominator_age_group_selector_PD)     %>%
      filter(denominator_sex %in% input$prevalence_denominator_sex_selector_PD)     %>%
      filter(denominator_days_prior_history %in% input$prevalence_denominator_days_prior_history_selector_PD)   %>%
      filter(outcome_cohort_name %in% input$prevalence_outcome_cohort_name_selector_PD)
    table1
  })
  
  output$tbl_prevalence_estimates_PD<-  renderDataTable({
    
    table1<-get_prevalence_estimates_PD()
    
    validate(need(ncol(table1)>1,
                  "No results for selected inputs"))
    
    table1 <- table1 %>%
      mutate(prevalence=paste0(nice.num3(prevalence*100), "%")) %>%
      mutate(prevalence_95CI_lower=paste0(nice.num3(prevalence_95CI_lower*100), "%")) %>%
      mutate(prevalence_95CI_upper=paste0(nice.num3(prevalence_95CI_upper*100), "%")) %>%
      mutate(prevalence= ifelse(!is.na(prevalence),
                                paste0(prevalence, " (",
                                       prevalence_95CI_lower," to ",
                                       prevalence_95CI_upper, " )"))) %>%
      select(!c("prevalence_95CI_lower", "prevalence_95CI_upper",
                "cohort_obscured", "result_obscured",
                "analysis_min_cell_count",
                "denominator_cohort_id", "outcome_cohort_id"))
    datatable(table1,
              rownames= FALSE,
              # colnames = c('Variable' = 1),
              extensions = 'Buttons',
              # options = list(lengthChange = FALSE,
              #                pageLength = 40,
              #                buttons = c('copy', 'csv', 'excel')))
              options = list(lengthChange = FALSE,
                             pageLength = 94,
                             dom = 'tB',
                             buttons = list(list(extend = "excel",
                                                 text = "Download table as excel",
                                                 filename = "PatientProfiles.csv"))
              ))
  } )
  
  output$plot_prevalence_estimates_PD<- renderPlotly({
    
    table<-get_prevalence_estimates_PD()
    validate(need(ncol(table)>1,
                  "No results for selected inputs"))
    
    if(is.null(input$prevalence_plot_group_PD)){
      if(!is.null(input$prevalence_plot_facet_PD)){
        p<-table %>%
          unite("facet_var",
                c(all_of(input$prevalence_plot_facet_PD)), remove = FALSE, sep = "; ") %>%
          ggplot(aes_string(x=input$prevalence_x_axis_PD, y="prevalence",
                            ymin = "prevalence_95CI_lower",
                            ymax = "prevalence_95CI_upper")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0) +
          facet_wrap(vars(facet_var),ncol = 2)+
          scale_y_continuous(
            labels = scales::percent,
            limits = c(0, NA)
          ) +
          theme_bw()
      } else{
        p<-table %>%
          ggplot(aes_string(x=input$prevalence_x_axis_PD, y="prevalence",
                            ymin = "prevalence_95CI_lower",
                            ymax = "prevalence_95CI_upper")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0) +
          scale_y_continuous(
            labels = scales::percent,
            limits = c(0, NA)
          ) +
          theme_bw()
      }
    }
    
    
    if(!is.null(input$prevalence_plot_group_PD) ){
      
      if(is.null(input$prevalence_plot_facet_PD) ){
        p<-table %>%
          unite("Group",
                c(all_of(input$prevalence_plot_group_PD)), remove = FALSE, sep = "; ") %>%
          ggplot(aes_string(x=input$prevalence_x_axis_PD, y="prevalence",
                            ymin = "prevalence_95CI_lower",
                            ymax = "prevalence_95CI_upper",
                            group="Group",
                            colour="Group")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0, position=position_dodge(width=1)) +
          theme_bw()
      }
      
      if(!is.null(input$prevalence_plot_facet_PD) ){
        if(!is.null(input$prevalence_plot_group_PD) ){
          p<-table %>%
            unite("Group",
                  c(all_of(input$prevalence_plot_group_PD)), remove = FALSE, sep = "; ") %>%
            unite("facet_var",
                  c(all_of(input$prevalence_plot_facet_PD)), remove = FALSE, sep = "; ") %>%
            ggplot(aes_string(x=input$prevalence_x_axis_PD, y="prevalence",
                              ymin = "prevalence_95CI_lower",
                              ymax = "prevalence_95CI_upper",
                              group="Group",
                              colour="Group")) +
            geom_point(position=position_dodge(width=1))+
            geom_errorbar(width=0, position=position_dodge(width=1)) +
            facet_wrap(vars(facet_var),ncol = 2)+
            scale_y_continuous(
              labels = scales::percent,
              limits = c(0, NA)
            )  +
            theme_bw()
        }
      }
      
    }
    
    p
    
  })
  
  get_prevalence_attrition_PD<-reactive({
    
    table<-prevalence_attrition_drugPD %>%
      # first deselect settings which did not vary for this study
      select(!c(analysis_id, analysis_outcome_lookback_days,
                analysis_interval,
                analysis_time_point,
                analysis_complete_database_intervals,
                denominator_start_date,
                denominator_end_date)) %>%
      filter(database_name %in% input$prevalence_database_name_selector_PD)    %>%
      filter(denominator_age_group %in% input$prevalence_denominator_age_group_selector_PD)     %>%
      filter(denominator_sex %in% input$prevalence_denominator_sex_selector_PD)     %>%
      filter(denominator_days_prior_history %in% input$prevalence_denominator_days_prior_history_selector_PD)     %>%
      filter(outcome_cohort_name %in% input$prevalence_outcome_cohort_name_selector_PD)
    
    table
  })
  
  output$tbl_prevalence_attrition_PD<-  renderDataTable({
    
    table<-get_prevalence_attrition_PD()
    
    validate(need(ncol(table)>1,
                  "No results for selected inputs"))
    
    table <- table %>%
      select(!c("analysis_full_contribution","analysis_min_cell_count",
                "denominator_cohort_id", "outcome_cohort_id"))
    
    datatable(table,
              rownames= FALSE,
              # colnames = c('Variable' = 1),
              extensions = 'Buttons',
              # options = list(lengthChange = FALSE,
              #                pageLength = 40,
              #                buttons = c('copy', 'csv', 'excel')))
              options = list(lengthChange = FALSE,
                             pageLength = 94,
                             dom = 'tB',
                             buttons = list(list(extend = "excel",
                                                 text = "Download table as excel",
                                                 filename = "PatientProfiles.csv"))
              ))
  })
  
  # prevalence Parkinsonism
  get_prevalence_estimates_Parkinsonism<-reactive({
    
    table1<-prevalence_estimates_drugParkinsonism %>%
      # first deselect settings which did not vary for this study
      select(!c(analysis_id, analysis_outcome_lookback_days,
                analysis_interval,
                analysis_time_point,
                analysis_complete_database_intervals,
                denominator_start_date,
                denominator_end_date,
                analysis_type, analysis_full_contribution)) %>%
      filter(database_name %in% input$prevalence_database_name_selector_Parkinsonism)  %>%
      filter(as.character(prevalence_start_date) %in% input$prevalence_start_date_selector_Parkinsonism)  %>%
      filter(denominator_age_group %in% input$prevalence_denominator_age_group_selector_Parkinsonism)     %>%
      filter(denominator_sex %in% input$prevalence_denominator_sex_selector_Parkinsonism)     %>%
      filter(denominator_days_prior_history %in% input$prevalence_denominator_days_prior_history_selector_Parkinsonism)   %>%
      filter(outcome_cohort_name %in% input$prevalence_outcome_cohort_name_selector_Parkinsonism)
    table1
  })
  
  output$tbl_prevalence_estimates_Parkinsonism<-  renderDataTable({
    
    table1<-get_prevalence_estimates_Parkinsonism()
    
    validate(need(ncol(table1)>1,
                  "No results for selected inputs"))
    
    table1 <- table1 %>%
      mutate(prevalence=paste0(nice.num3(prevalence*100), "%")) %>%
      mutate(prevalence_95CI_lower=paste0(nice.num3(prevalence_95CI_lower*100), "%")) %>%
      mutate(prevalence_95CI_upper=paste0(nice.num3(prevalence_95CI_upper*100), "%")) %>%
      mutate(prevalence= ifelse(!is.na(prevalence),
                                paste0(prevalence, " (",
                                       prevalence_95CI_lower," to ",
                                       prevalence_95CI_upper, " )"))) %>%
      select(!c("prevalence_95CI_lower", "prevalence_95CI_upper",
                "cohort_obscured", "result_obscured",
                "analysis_min_cell_count",
                "denominator_cohort_id", "outcome_cohort_id"))
    datatable(table1,
              rownames= FALSE,
              # colnames = c('Variable' = 1),
              extensions = 'Buttons',
              # options = list(lengthChange = FALSE,
              #                pageLength = 40,
              #                buttons = c('copy', 'csv', 'excel')))
              options = list(lengthChange = FALSE,
                             pageLength = 94,
                             dom = 'tB',
                             buttons = list(list(extend = "excel",
                                                 text = "Download table as excel",
                                                 filename = "PatientProfiles.csv"))
              ))
  } )
  
  output$plot_prevalence_estimates_Parkinsonism<- renderPlotly({
    
    table<-get_prevalence_estimates_Parkinsonism()
    validate(need(ncol(table)>1,
                  "No results for selected inputs"))
    
    if(is.null(input$prevalence_plot_group_Parkinsonism)){
      if(!is.null(input$prevalence_plot_facet_Parkinsonism)){
        p<-table %>%
          unite("facet_var",
                c(all_of(input$prevalence_plot_facet_Parkinsonism)), remove = FALSE, sep = "; ") %>%
          ggplot(aes_string(x=input$prevalence_x_axis_Parkinsonism, y="prevalence",
                            ymin = "prevalence_95CI_lower",
                            ymax = "prevalence_95CI_upper")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0) +
          facet_wrap(vars(facet_var),ncol = 2)+
          scale_y_continuous(
            labels = scales::percent,
            limits = c(0, NA)
          ) +
          theme_bw()
      } else{
        p<-table %>%
          ggplot(aes_string(x=input$prevalence_x_axis_Parkinsonism, y="prevalence",
                            ymin = "prevalence_95CI_lower",
                            ymax = "prevalence_95CI_upper")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0) +
          scale_y_continuous(
            labels = scales::percent,
            limits = c(0, NA)
          ) +
          theme_bw()
      }
    }
    
    
    if(!is.null(input$prevalence_plot_group_Parkinsonism) ){
      
      if(is.null(input$prevalence_plot_facet_Parkinsonism) ){
        p<-table %>%
          unite("Group",
                c(all_of(input$prevalence_plot_group_Parkinsonism)), remove = FALSE, sep = "; ") %>%
          ggplot(aes_string(x=input$prevalence_x_axis_Parkinsonism, y="prevalence",
                            ymin = "prevalence_95CI_lower",
                            ymax = "prevalence_95CI_upper",
                            group="Group",
                            colour="Group")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0, position=position_dodge(width=1)) +
          theme_bw()
      }
      
      if(!is.null(input$prevalence_plot_facet_Parkinsonism) ){
        if(!is.null(input$prevalence_plot_group_Parkinsonism) ){
          p<-table %>%
            unite("Group",
                  c(all_of(input$prevalence_plot_group_Parkinsonism)), remove = FALSE, sep = "; ") %>%
            unite("facet_var",
                  c(all_of(input$prevalence_plot_facet_Parkinsonism)), remove = FALSE, sep = "; ") %>%
            ggplot(aes_string(x=input$prevalence_x_axis_Parkinsonism, y="prevalence",
                              ymin = "prevalence_95CI_lower",
                              ymax = "prevalence_95CI_upper",
                              group="Group",
                              colour="Group")) +
            geom_point(position=position_dodge(width=1))+
            geom_errorbar(width=0, position=position_dodge(width=1)) +
            facet_wrap(vars(facet_var),ncol = 2)+
            scale_y_continuous(
              labels = scales::percent,
              limits = c(0, NA)
            )  +
            theme_bw()
        }
      }
      
    }
    
    p
    
  })
  
  get_prevalence_attrition_Parkinsonism<-reactive({
    
    table<-prevalence_attrition_drugParkinsonism %>%
      # first deselect settings which did not vary for this study
      select(!c(analysis_id, analysis_outcome_lookback_days,
                analysis_interval,
                analysis_time_point,
                analysis_complete_database_intervals,
                denominator_start_date,
                denominator_end_date)) %>%
      filter(database_name %in% input$prevalence_database_name_selector_Parkinsonism)    %>%
      filter(denominator_age_group %in% input$prevalence_denominator_age_group_selector_Parkinsonism)     %>%
      filter(denominator_sex %in% input$prevalence_denominator_sex_selector_Parkinsonism)     %>%
      filter(denominator_days_prior_history %in% input$prevalence_denominator_days_prior_history_selector_Parkinsonism)     %>%
      filter(outcome_cohort_name %in% input$prevalence_outcome_cohort_name_selector_Parkinsonism)
    
    table
  })
  
  output$tbl_prevalence_attrition_Parkinsonism<-  renderDataTable({
    
    table<-get_prevalence_attrition_Parkinsonism()
    
    validate(need(ncol(table)>1,
                  "No results for selected inputs"))
    
    table <- table %>%
      select(!c("analysis_full_contribution","analysis_min_cell_count",
                "denominator_cohort_id", "outcome_cohort_id"))
    
    datatable(table,
              rownames= FALSE,
              # colnames = c('Variable' = 1),
              extensions = 'Buttons',
              # options = list(lengthChange = FALSE,
              #                pageLength = 40,
              #                buttons = c('copy', 'csv', 'excel')))
              options = list(lengthChange = FALSE,
                             pageLength = 94,
                             dom = 'tB',
                             buttons = list(list(extend = "excel",
                                                 text = "Download table as excel",
                                                 filename = "PatientProfiles.csv"))
              ))
  })
  
  # prevalence DIP
  get_prevalence_estimates_DIP<-reactive({
    
    table1<-prevalence_estimates_drugDIP %>%
      # first deselect settings which did not vary for this study
      select(!c(analysis_id, analysis_outcome_lookback_days,
                analysis_interval,
                analysis_time_point,
                analysis_complete_database_intervals,
                denominator_start_date,
                denominator_end_date,
                analysis_type, analysis_full_contribution)) %>%
      filter(database_name %in% input$prevalence_database_name_selector_DIP)  %>%
      filter(as.character(prevalence_start_date) %in% input$prevalence_start_date_selector_DIP)  %>%
      filter(denominator_age_group %in% input$prevalence_denominator_age_group_selector_DIP)     %>%
      filter(denominator_sex %in% input$prevalence_denominator_sex_selector_DIP)     %>%
      filter(denominator_days_prior_history %in% input$prevalence_denominator_days_prior_history_selector_DIP)   %>%
      filter(outcome_cohort_name %in% input$prevalence_outcome_cohort_name_selector_DIP)
    table1
  })
  
  output$tbl_prevalence_estimates_DIP<-  renderDataTable({
    
    table1<-get_prevalence_estimates_DIP()
    
    validate(need(ncol(table1)>1,
                  "No results for selected inputs"))
    
    table1 <- table1 %>%
      mutate(prevalence=paste0(nice.num3(prevalence*100), "%")) %>%
      mutate(prevalence_95CI_lower=paste0(nice.num3(prevalence_95CI_lower*100), "%")) %>%
      mutate(prevalence_95CI_upper=paste0(nice.num3(prevalence_95CI_upper*100), "%")) %>%
      mutate(prevalence= ifelse(!is.na(prevalence),
                                paste0(prevalence, " (",
                                       prevalence_95CI_lower," to ",
                                       prevalence_95CI_upper, " )"))) %>%
      select(!c("prevalence_95CI_lower", "prevalence_95CI_upper",
                "cohort_obscured", "result_obscured",
                "analysis_min_cell_count",
                "denominator_cohort_id", "outcome_cohort_id"))
    datatable(table1,
              rownames= FALSE,
              # colnames = c('Variable' = 1),
              extensions = 'Buttons',
              # options = list(lengthChange = FALSE,
              #                pageLength = 40,
              #                buttons = c('copy', 'csv', 'excel')))
              options = list(lengthChange = FALSE,
                             pageLength = 94,
                             dom = 'tB',
                             buttons = list(list(extend = "excel",
                                                 text = "Download table as excel",
                                                 filename = "PatientProfiles.csv"))
              ))
  } )
  
  output$plot_prevalence_estimates_DIP<- renderPlotly({
    
    table<-get_prevalence_estimates_DIP()
    validate(need(ncol(table)>1,
                  "No results for selected inputs"))
    
    if(is.null(input$prevalence_plot_group_DIP)){
      if(!is.null(input$prevalence_plot_facet_DIP)){
        p<-table %>%
          unite("facet_var",
                c(all_of(input$prevalence_plot_facet_DIP)), remove = FALSE, sep = "; ") %>%
          ggplot(aes_string(x=input$prevalence_x_axis_DIP, y="prevalence",
                            ymin = "prevalence_95CI_lower",
                            ymax = "prevalence_95CI_upper")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0) +
          facet_wrap(vars(facet_var),ncol = 2)+
          scale_y_continuous(
            labels = scales::percent,
            limits = c(0, NA)
          ) +
          theme_bw()
      } else{
        p<-table %>%
          ggplot(aes_string(x=input$prevalence_x_axis_DIP, y="prevalence",
                            ymin = "prevalence_95CI_lower",
                            ymax = "prevalence_95CI_upper")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0) +
          scale_y_continuous(
            labels = scales::percent,
            limits = c(0, NA)
          ) +
          theme_bw()
      }
    }
    
    
    if(!is.null(input$prevalence_plot_group_DIP) ){
      
      if(is.null(input$prevalence_plot_facet_DIP) ){
        p<-table %>%
          unite("Group",
                c(all_of(input$prevalence_plot_group_DIP)), remove = FALSE, sep = "; ") %>%
          ggplot(aes_string(x=input$prevalence_x_axis_DIP, y="prevalence",
                            ymin = "prevalence_95CI_lower",
                            ymax = "prevalence_95CI_upper",
                            group="Group",
                            colour="Group")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0, position=position_dodge(width=1)) +
          theme_bw()
      }
      
      if(!is.null(input$prevalence_plot_facet_DIP) ){
        if(!is.null(input$prevalence_plot_group_DIP) ){
          p<-table %>%
            unite("Group",
                  c(all_of(input$prevalence_plot_group_DIP)), remove = FALSE, sep = "; ") %>%
            unite("facet_var",
                  c(all_of(input$prevalence_plot_facet_DIP)), remove = FALSE, sep = "; ") %>%
            ggplot(aes_string(x=input$prevalence_x_axis_DIP, y="prevalence",
                              ymin = "prevalence_95CI_lower",
                              ymax = "prevalence_95CI_upper",
                              group="Group",
                              colour="Group")) +
            geom_point(position=position_dodge(width=1))+
            geom_errorbar(width=0, position=position_dodge(width=1)) +
            facet_wrap(vars(facet_var),ncol = 2)+
            scale_y_continuous(
              labels = scales::percent,
              limits = c(0, NA)
            )  +
            theme_bw()
        }
      }
      
    }
    
    p
    
  })
  
  get_prevalence_attrition_DIP<-reactive({
    
    table<-prevalence_attrition_drugDIP %>%
      # first deselect settings which did not vary for this study
      select(!c(analysis_id, analysis_outcome_lookback_days,
                analysis_interval,
                analysis_time_point,
                analysis_complete_database_intervals,
                denominator_start_date,
                denominator_end_date)) %>%
      filter(database_name %in% input$prevalence_database_name_selector_DIP)    %>%
      filter(denominator_age_group %in% input$prevalence_denominator_age_group_selector_DIP)     %>%
      filter(denominator_sex %in% input$prevalence_denominator_sex_selector_DIP)     %>%
      filter(denominator_days_prior_history %in% input$prevalence_denominator_days_prior_history_selector_DIP)     %>%
      filter(outcome_cohort_name %in% input$prevalence_outcome_cohort_name_selector_DIP)
    
    table
  })
  
  output$tbl_prevalence_attrition_DIP<-  renderDataTable({
    
    table<-get_prevalence_attrition_DIP()
    
    validate(need(ncol(table)>1,
                  "No results for selected inputs"))
    
    table <- table %>%
      select(!c("analysis_full_contribution","analysis_min_cell_count",
                "denominator_cohort_id", "outcome_cohort_id"))
    
    datatable(table,
              rownames= FALSE,
              # colnames = c('Variable' = 1),
              extensions = 'Buttons',
              # options = list(lengthChange = FALSE,
              #                pageLength = 40,
              #                buttons = c('copy', 'csv', 'excel')))
              options = list(lengthChange = FALSE,
                             pageLength = 94,
                             dom = 'tB',
                             buttons = list(list(extend = "excel",
                                                 text = "Download table as excel",
                                                 filename = "PatientProfiles.csv"))
              ))
  })

  # prevalence VP
  get_prevalence_estimates_VP<-reactive({
    
    table1<-prevalence_estimates_drugVP %>%
      # first deselect settings which did not vary for this study
      select(!c(analysis_id, analysis_outcome_lookback_days,
                analysis_interval,
                analysis_time_point,
                analysis_complete_database_intervals,
                denominator_start_date,
                denominator_end_date,
                analysis_type, analysis_full_contribution)) %>%
      filter(database_name %in% input$prevalence_database_name_selector_VP)  %>%
      filter(as.character(prevalence_start_date) %in% input$prevalence_start_date_selector_VP)  %>%
      filter(denominator_age_group %in% input$prevalence_denominator_age_group_selector_VP)     %>%
      filter(denominator_sex %in% input$prevalence_denominator_sex_selector_VP)     %>%
      filter(denominator_days_prior_history %in% input$prevalence_denominator_days_prior_history_selector_VP)   %>%
      filter(outcome_cohort_name %in% input$prevalence_outcome_cohort_name_selector_VP)
    table1
  })
  
  output$tbl_prevalence_estimates_VP<-  renderDataTable({
    
    table1<-get_prevalence_estimates_VP()
    
    validate(need(ncol(table1)>1,
                  "No results for selected inputs"))
    
    table1 <- table1 %>%
      mutate(prevalence=paste0(nice.num3(prevalence*100), "%")) %>%
      mutate(prevalence_95CI_lower=paste0(nice.num3(prevalence_95CI_lower*100), "%")) %>%
      mutate(prevalence_95CI_upper=paste0(nice.num3(prevalence_95CI_upper*100), "%")) %>%
      mutate(prevalence= ifelse(!is.na(prevalence),
                                paste0(prevalence, " (",
                                       prevalence_95CI_lower," to ",
                                       prevalence_95CI_upper, " )"))) %>%
      select(!c("prevalence_95CI_lower", "prevalence_95CI_upper",
                "cohort_obscured", "result_obscured",
                "analysis_min_cell_count",
                "denominator_cohort_id", "outcome_cohort_id"))
    datatable(table1,
              rownames= FALSE,
              # colnames = c('Variable' = 1),
              extensions = 'Buttons',
              # options = list(lengthChange = FALSE,
              #                pageLength = 40,
              #                buttons = c('copy', 'csv', 'excel')))
              options = list(lengthChange = FALSE,
                             pageLength = 94,
                             dom = 'tB',
                             buttons = list(list(extend = "excel",
                                                 text = "Download table as excel",
                                                 filename = "PatientProfiles.csv"))
              ))
  } )
  
  output$plot_prevalence_estimates_VP<- renderPlotly({
    
    table<-get_prevalence_estimates_VP()
    validate(need(ncol(table)>1,
                  "No results for selected inputs"))
    
    if(is.null(input$prevalence_plot_group_VP)){
      if(!is.null(input$prevalence_plot_facet_VP)){
        p<-table %>%
          unite("facet_var",
                c(all_of(input$prevalence_plot_facet_VP)), remove = FALSE, sep = "; ") %>%
          ggplot(aes_string(x=input$prevalence_x_axis_VP, y="prevalence",
                            ymin = "prevalence_95CI_lower",
                            ymax = "prevalence_95CI_upper")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0) +
          facet_wrap(vars(facet_var),ncol = 2)+
          scale_y_continuous(
            labels = scales::percent,
            limits = c(0, NA)
          ) +
          theme_bw()
      } else{
        p<-table %>%
          ggplot(aes_string(x=input$prevalence_x_axis_VP, y="prevalence",
                            ymin = "prevalence_95CI_lower",
                            ymax = "prevalence_95CI_upper")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0) +
          scale_y_continuous(
            labels = scales::percent,
            limits = c(0, NA)
          ) +
          theme_bw()
      }
    }
    
    
    if(!is.null(input$prevalence_plot_group_VP) ){
      
      if(is.null(input$prevalence_plot_facet_VP) ){
        p<-table %>%
          unite("Group",
                c(all_of(input$prevalence_plot_group_VP)), remove = FALSE, sep = "; ") %>%
          ggplot(aes_string(x=input$prevalence_x_axis_VP, y="prevalence",
                            ymin = "prevalence_95CI_lower",
                            ymax = "prevalence_95CI_upper",
                            group="Group",
                            colour="Group")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0, position=position_dodge(width=1)) +
          theme_bw()
      }
      
      if(!is.null(input$prevalence_plot_facet_VP) ){
        if(!is.null(input$prevalence_plot_group_VP) ){
          p<-table %>%
            unite("Group",
                  c(all_of(input$prevalence_plot_group_VP)), remove = FALSE, sep = "; ") %>%
            unite("facet_var",
                  c(all_of(input$prevalence_plot_facet_VP)), remove = FALSE, sep = "; ") %>%
            ggplot(aes_string(x=input$prevalence_x_axis_VP, y="prevalence",
                              ymin = "prevalence_95CI_lower",
                              ymax = "prevalence_95CI_upper",
                              group="Group",
                              colour="Group")) +
            geom_point(position=position_dodge(width=1))+
            geom_errorbar(width=0, position=position_dodge(width=1)) +
            facet_wrap(vars(facet_var),ncol = 2)+
            scale_y_continuous(
              labels = scales::percent,
              limits = c(0, NA)
            )  +
            theme_bw()
        }
      }
      
    }
    
    p
    
  })
  
  get_prevalence_attrition_VP<-reactive({
    
    table<-prevalence_attrition_drugVP %>%
      # first deselect settings which did not vary for this study
      select(!c(analysis_id, analysis_outcome_lookback_days,
                analysis_interval,
                analysis_time_point,
                analysis_complete_database_intervals,
                denominator_start_date,
                denominator_end_date)) %>%
      filter(database_name %in% input$prevalence_database_name_selector_VP)    %>%
      filter(denominator_age_group %in% input$prevalence_denominator_age_group_selector_VP)     %>%
      filter(denominator_sex %in% input$prevalence_denominator_sex_selector_VP)     %>%
      filter(denominator_days_prior_history %in% input$prevalence_denominator_days_prior_history_selector_VP)     %>%
      filter(outcome_cohort_name %in% input$prevalence_outcome_cohort_name_selector_VP)
    
    table
  })
  
  output$tbl_prevalence_attrition_VP<-  renderDataTable({
    
    table<-get_prevalence_attrition_VP()
    
    validate(need(ncol(table)>1,
                  "No results for selected inputs"))
    
    table <- table %>%
      select(!c("analysis_full_contribution","analysis_min_cell_count",
                "denominator_cohort_id", "outcome_cohort_id"))
    
    datatable(table,
              rownames= FALSE,
              # colnames = c('Variable' = 1),
              extensions = 'Buttons',
              # options = list(lengthChange = FALSE,
              #                pageLength = 40,
              #                buttons = c('copy', 'csv', 'excel')))
              options = list(lengthChange = FALSE,
                             pageLength = 94,
                             dom = 'tB',
                             buttons = list(list(extend = "excel",
                                                 text = "Download table as excel",
                                                 filename = "PatientProfiles.csv"))
              ))
  })
  
  # incidence
  get_incidence_estimates<-reactive({
    
    table<-incidence_estimates %>% 
      # first deselect settings which did not vary for this study
      select(!c(analysis_id, 
                analysis_interval,
                analysis_complete_database_intervals,
                denominator_start_date,
                denominator_end_date)) %>% 
      filter(database_name %in% input$incidence_database_name_selector)  %>% 
      filter(as.character(incidence_start_date) %in% input$incidence_start_date_selector)  %>% 
      filter(denominator_age_group %in% input$incidence_denominator_age_group_selector)     %>% 
      filter(denominator_sex %in% input$incidence_denominator_sex_selector)     %>% 
      filter(denominator_days_prior_history %in% input$incidence_denominator_days_prior_history_selector)   %>% 
      filter(outcome_cohort_name %in% input$incidence_outcome_cohort_name_selector)  
    
    table
  }) 
  
  output$tbl_incidence_estimates<-  renderDataTable({
    
    table<-get_incidence_estimates() 
    
    validate(need(ncol(table)>1, 
                  "No results for selected inputs"))
    
    table <- table %>% 
      mutate(incidence_100000_pys=nice.num.count(incidence_100000_pys)) %>% 
      mutate(incidence_100000_pys_95CI_lower=nice.num.count(incidence_100000_pys_95CI_lower)) %>% 
      mutate(incidence_100000_pys_95CI_upper=nice.num.count(incidence_100000_pys_95CI_upper)) %>% 
      mutate(incidence_100000_pys= ifelse(!is.na(incidence_100000_pys),
                                          paste0(incidence_100000_pys, " (",
                                                 incidence_100000_pys_95CI_lower," to ", 
                                                 incidence_100000_pys_95CI_upper, " )"))) %>% 
      select(!c("incidence_100000_pys_95CI_lower", "incidence_100000_pys_95CI_upper",
                "cohort_obscured", "result_obscured","person_days",
                "analysis_min_cell_count","analysis_repeated_events",
                "analysis_outcome_washout",
                "denominator_cohort_id", "outcome_cohort_id")) %>% 
      mutate(n_persons=nice.num.count(n_persons)) %>% 
      mutate(n_events=nice.num.count(n_events)) %>% 
      mutate(person_years=nice.num.count(person_years)) %>% 
      relocate(incidence_start_date) %>% 
      relocate(incidence_end_date, .after = incidence_start_date) %>% 
      relocate(person_years, .after = n_persons)
    datatable(table,
              rownames= FALSE,
              # colnames = c('Variable' = 1),
              extensions = 'Buttons',
              # options = list(lengthChange = FALSE,
              #                pageLength = 40,
              #                buttons = c('copy', 'csv', 'excel')))
              options = list(lengthChange = FALSE,
                             pageLength = 94,
                             dom = 'tB',
                             buttons = list(list(extend = "excel", 
                                                 text = "Download table as excel",
                                                 filename = "PatientProfiles.csv"))
              ))
  } )
  
  output$plot_incidence_estimates<- renderPlotly({ 
    
    table<-get_incidence_estimates() 
    validate(need(ncol(table)>1, 
                  "No results for selected inputs"))
    
    if(is.null(input$incidence_plot_group)){
      if(!is.null(input$incidence_plot_facet)){
        p<-table %>% 
          unite("facet_var", 
                c(all_of(input$incidence_plot_facet)), remove = FALSE, sep = "; ") %>% 
          ggplot(aes_string(x=input$incidence_x_axis, y="incidence_100000_pys",
                            ymin = "incidence_100000_pys_95CI_lower",
                            ymax = "incidence_100000_pys_95CI_upper")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0) +
          facet_wrap(vars(facet_var),ncol = 2)+
          scale_y_continuous(
            limits = c(0, NA)
          ) +
          theme_bw()
      } else{
        p<-table %>% 
          ggplot(aes_string(x=input$incidence_x_axis, y="incidence_100000_pys",
                            ymin = "incidence_100000_pys_95CI_lower",
                            ymax = "incidence_100000_pys_95CI_upper")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0) +
          scale_y_continuous(
            limits = c(0, NA)
          ) +
          theme_bw()        
      }
    } 
    
    
    if(!is.null(input$incidence_plot_group) ){ 
      
      if(is.null(input$incidence_plot_facet) ){ 
        p<-table %>% 
          unite("Group", 
                c(all_of(input$incidence_plot_group)), remove = FALSE, sep = "; ") %>% 
          ggplot(aes_string(x=input$incidence_x_axis, y="incidence_100000_pys",
                            ymin = "incidence_100000_pys_95CI_lower",
                            ymax = "incidence_100000_pys_95CI_upper",
                            group="Group",
                            colour="Group")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0, position=position_dodge(width=1)) +
          theme_bw()
      }
      
      if(!is.null(input$incidence_plot_facet) ){
        if(!is.null(input$incidence_plot_group) ){ 
          p<-table %>% 
            unite("Group", 
                  c(all_of(input$incidence_plot_group)), remove = FALSE, sep = "; ") %>% 
            unite("facet_var", 
                  c(all_of(input$incidence_plot_facet)), remove = FALSE, sep = "; ") %>% 
            ggplot(aes_string(x=input$incidence_x_axis, y="incidence_100000_pys",
                              ymin = "incidence_100000_pys_95CI_lower",
                              ymax = "incidence_100000_pys_95CI_upper",
                              group="Group",
                              colour="Group")) +
            geom_point(position=position_dodge(width=1))+
            geom_errorbar(width=0, position=position_dodge(width=1)) +
            facet_wrap(vars(facet_var),ncol = 2)+  
            scale_y_continuous(
              limits = c(0, NA)
            )  +
            theme_bw()
        }
      }
      
    }
    
    p
    
  })
  
  get_incidence_attrition<-reactive({
    
    table<-incidence_attrition %>% 
      # first deselect settings which did not vary for this study
      select(!c(analysis_id, 
                analysis_interval,
                analysis_repeated_events,analysis_outcome_washout,
                analysis_complete_database_intervals,
                denominator_start_date,
                denominator_end_date)) %>% 
      filter(database_name %in% input$incidence_database_name_selector)    %>% 
      filter(denominator_age_group %in% input$incidence_denominator_age_group_selector)     %>% 
      filter(denominator_sex %in% input$incidence_denominator_sex_selector)     %>% 
      filter(denominator_days_prior_history %in% input$incidence_denominator_days_prior_history_selector)     %>% 
      filter(outcome_cohort_name %in% input$incidence_outcome_cohort_name_selector)  
    
    table
  }) 
  
  output$tbl_incidence_attrition<-  renderDataTable({
    
    table<-get_incidence_attrition() 
    
    validate(need(ncol(table)>1, 
                  "No results for selected inputs"))
    
    table <- table %>% 
      select(!c("analysis_min_cell_count",
                "denominator_cohort_id", "outcome_cohort_id")) 
    
    datatable(table,
              rownames= FALSE,
              # colnames = c('Variable' = 1),
              extensions = 'Buttons',
              # options = list(lengthChange = FALSE,
              #                pageLength = 40,
              #                buttons = c('copy', 'csv', 'excel')))
              options = list(lengthChange = FALSE,
                             pageLength = 94,
                             dom = 'tB',
                             buttons = list(list(extend = "excel", 
                                                 text = "Download table as excel",
                                                 filename = "PatientProfiles.csv"))
              ))
  })  
  
  # incidence drugs
  get_incidence_estimates_drugs<-reactive({
    
    table<-incidence_estimates_drugs %>% 
      # first deselect settings which did not vary for this study
      select(!c(analysis_id, 
                analysis_interval,
                analysis_complete_database_intervals,
                denominator_start_date,
                denominator_end_date)) %>% 
      filter(database_name %in% input$incidence_database_name_selector_drugs)  %>% 
      filter(as.character(incidence_start_date) %in% input$incidence_start_date_selector_drugs)  %>% 
      filter(denominator_age_group %in% input$incidence_denominator_age_group_selector_drugs)     %>% 
      filter(denominator_sex %in% input$incidence_denominator_sex_selector_drugs)     %>% 
      filter(denominator_days_prior_history %in% input$incidence_denominator_days_prior_history_selector_drugs)   %>% 
      filter(outcome_cohort_name %in% input$incidence_outcome_cohort_name_selector_drugs)  
    
    table
  }) 
  
  output$tbl_incidence_estimates_drugs<-  renderDataTable({
    
    table<-get_incidence_estimates_drugs() 
    
    validate(need(ncol(table)>1, 
                  "No results for selected inputs"))
    
    table <- table %>% 
      mutate(incidence_100000_pys=nice.num.count(incidence_100000_pys)) %>% 
      mutate(incidence_100000_pys_95CI_lower=nice.num.count(incidence_100000_pys_95CI_lower)) %>% 
      mutate(incidence_100000_pys_95CI_upper=nice.num.count(incidence_100000_pys_95CI_upper)) %>% 
      mutate(incidence_100000_pys= ifelse(!is.na(incidence_100000_pys),
                                          paste0(incidence_100000_pys, " (",
                                                 incidence_100000_pys_95CI_lower," to ", 
                                                 incidence_100000_pys_95CI_upper, " )"))) %>% 
      select(!c("incidence_100000_pys_95CI_lower", "incidence_100000_pys_95CI_upper",
                "cohort_obscured", "result_obscured","person_days",
                "analysis_min_cell_count","analysis_repeated_events",
                "analysis_outcome_washout",
                "denominator_cohort_id", "outcome_cohort_id")) %>% 
      mutate(n_persons=nice.num.count(n_persons)) %>% 
      mutate(n_events=nice.num.count(n_events)) %>% 
      mutate(person_years=nice.num.count(person_years)) %>% 
      relocate(incidence_start_date) %>% 
      relocate(incidence_end_date, .after = incidence_start_date) %>% 
      relocate(person_years, .after = n_persons)
    datatable(table,
              rownames= FALSE,
              # colnames = c('Variable' = 1),
              extensions = 'Buttons',
              # options = list(lengthChange = FALSE,
              #                pageLength = 40,
              #                buttons = c('copy', 'csv', 'excel')))
              options = list(lengthChange = FALSE,
                             pageLength = 94,
                             dom = 'tB',
                             buttons = list(list(extend = "excel", 
                                                 text = "Download table as excel",
                                                 filename = "PatientProfiles.csv"))
              ))
  } )
  
  output$plot_incidence_estimates_drugs<- renderPlotly({ 
    
    table<-get_incidence_estimates_drugs() 
    validate(need(ncol(table)>1, 
                  "No results for selected inputs"))
    
    if(is.null(input$incidence_plot_group_drugs)){
      if(!is.null(input$incidence_plot_facet_drugs)){
        p<-table %>% 
          unite("facet_var", 
                c(all_of(input$incidence_plot_facet_drugs)), remove = FALSE, sep = "; ") %>% 
          ggplot(aes_string(x=input$incidence_x_axis_drugs, y="incidence_100000_pys",
                            ymin = "incidence_100000_pys_95CI_lower",
                            ymax = "incidence_100000_pys_95CI_upper")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0) +
          facet_wrap(vars(facet_var),ncol = 2)+
          scale_y_continuous(
            limits = c(0, NA)
          ) +
          theme_bw()
      } else{
        p<-table %>% 
          ggplot(aes_string(x=input$incidence_x_axis_drugs, y="incidence_100000_pys",
                            ymin = "incidence_100000_pys_95CI_lower",
                            ymax = "incidence_100000_pys_95CI_upper")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0) +
          scale_y_continuous(
            limits = c(0, NA)
          ) +
          theme_bw()        
      }
    } 
    
    
    if(!is.null(input$incidence_plot_group_drugs) ){ 
      
      if(is.null(input$incidence_plot_facet_drugs) ){ 
        p<-table %>% 
          unite("Group", 
                c(all_of(input$incidence_plot_group_drugs)), remove = FALSE, sep = "; ") %>% 
          ggplot(aes_string(x=input$incidence_x_axis_drugs, y="incidence_100000_pys",
                            ymin = "incidence_100000_pys_95CI_lower",
                            ymax = "incidence_100000_pys_95CI_upper",
                            group="Group",
                            colour="Group")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0, position=position_dodge(width=1)) +
          theme_bw()
      }
      
      if(!is.null(input$incidence_plot_facet_drugs) ){
        if(!is.null(input$incidence_plot_group_drugs) ){ 
          p<-table %>% 
            unite("Group", 
                  c(all_of(input$incidence_plot_group_drugs)), remove = FALSE, sep = "; ") %>% 
            unite("facet_var", 
                  c(all_of(input$incidence_plot_facet_drugs)), remove = FALSE, sep = "; ") %>% 
            ggplot(aes_string(x=input$incidence_x_axis_drugs, y="incidence_100000_pys",
                              ymin = "incidence_100000_pys_95CI_lower",
                              ymax = "incidence_100000_pys_95CI_upper",
                              group="Group",
                              colour="Group")) +
            geom_point(position=position_dodge(width=1))+
            geom_errorbar(width=0, position=position_dodge(width=1)) +
            facet_wrap(vars(facet_var),ncol = 2)+  
            scale_y_continuous(
              limits = c(0, NA)
            )  +
            theme_bw()
        }
      }
      
    }
    
    p
    
  })
  
  get_incidence_attrition_drugs<-reactive({
    
    table<-incidence_attrition_drugs %>% 
      # first deselect settings which did not vary for this study
      select(!c(analysis_id, 
                analysis_interval,
                analysis_repeated_events,analysis_outcome_washout,
                analysis_complete_database_intervals,
                denominator_start_date,
                denominator_end_date)) %>% 
      filter(database_name %in% input$incidence_database_name_selector_drugs)    %>% 
      filter(denominator_age_group %in% input$incidence_denominator_age_group_selector_drugs)     %>% 
      filter(denominator_sex %in% input$incidence_denominator_sex_selector_drugs)     %>% 
      filter(denominator_days_prior_history %in% input$incidence_denominator_days_prior_history_selector_drugs)     %>% 
      filter(outcome_cohort_name %in% input$incidence_outcome_cohort_name_selector_drugs)  
    
    table
  }) 
  
  output$tbl_incidence_attrition_drugs<-  renderDataTable({
    
    table<-get_incidence_attrition_drugs() 
    
    validate(need(ncol(table)>1, 
                  "No results for selected inputs"))
    
    table <- table %>% 
      select(!c("analysis_min_cell_count",
                "denominator_cohort_id", "outcome_cohort_id")) 
    
    datatable(table,
              rownames= FALSE,
              # colnames = c('Variable' = 1),
              extensions = 'Buttons',
              # options = list(lengthChange = FALSE,
              #                pageLength = 40,
              #                buttons = c('copy', 'csv', 'excel')))
              options = list(lengthChange = FALSE,
                             pageLength = 94,
                             dom = 'tB',
                             buttons = list(list(extend = "excel", 
                                                 text = "Download table as excel",
                                                 filename = "PatientProfiles.csv"))
              ))
  })  
  
  # incidence PD
  get_incidence_estimates_PD<-reactive({
    
    table<-incidence_estimates_drugPD %>% 
      # first deselect settings which did not vary for this study
      select(!c(analysis_id, 
                analysis_interval,
                analysis_complete_database_intervals,
                denominator_start_date,
                denominator_end_date)) %>% 
      filter(database_name %in% input$incidence_database_name_selector_PD)  %>% 
      filter(as.character(incidence_start_date) %in% input$incidence_start_date_selector_PD)  %>% 
      filter(denominator_age_group %in% input$incidence_denominator_age_group_selector_PD)     %>% 
      filter(denominator_sex %in% input$incidence_denominator_sex_selector_PD)     %>% 
      filter(denominator_days_prior_history %in% input$incidence_denominator_days_prior_history_selector_PD)   %>% 
      filter(outcome_cohort_name %in% input$incidence_outcome_cohort_name_selector_PD)  
    
    table
  }) 
  
  output$tbl_incidence_estimates_PD<-  renderDataTable({
    
    table<-get_incidence_estimates_PD() 
    
    validate(need(ncol(table)>1, 
                  "No results for selected inputs"))
    
    table <- table %>% 
      mutate(incidence_100000_pys=nice.num.count(incidence_100000_pys)) %>% 
      mutate(incidence_100000_pys_95CI_lower=nice.num.count(incidence_100000_pys_95CI_lower)) %>% 
      mutate(incidence_100000_pys_95CI_upper=nice.num.count(incidence_100000_pys_95CI_upper)) %>% 
      mutate(incidence_100000_pys= ifelse(!is.na(incidence_100000_pys),
                                          paste0(incidence_100000_pys, " (",
                                                 incidence_100000_pys_95CI_lower," to ", 
                                                 incidence_100000_pys_95CI_upper, " )"))) %>% 
      select(!c("incidence_100000_pys_95CI_lower", "incidence_100000_pys_95CI_upper",
                "cohort_obscured", "result_obscured","person_days",
                "analysis_min_cell_count","analysis_repeated_events",
                "analysis_outcome_washout",
                "denominator_cohort_id", "outcome_cohort_id")) %>% 
      mutate(n_persons=nice.num.count(n_persons)) %>% 
      mutate(n_events=nice.num.count(n_events)) %>% 
      mutate(person_years=nice.num.count(person_years)) %>% 
      relocate(incidence_start_date) %>% 
      relocate(incidence_end_date, .after = incidence_start_date) %>% 
      relocate(person_years, .after = n_persons)
    datatable(table,
              rownames= FALSE,
              # colnames = c('Variable' = 1),
              extensions = 'Buttons',
              # options = list(lengthChange = FALSE,
              #                pageLength = 40,
              #                buttons = c('copy', 'csv', 'excel')))
              options = list(lengthChange = FALSE,
                             pageLength = 94,
                             dom = 'tB',
                             buttons = list(list(extend = "excel", 
                                                 text = "Download table as excel",
                                                 filename = "PatientProfiles.csv"))
              ))
  } )
  
  output$plot_incidence_estimates_PD<- renderPlotly({ 
    
    table<-get_incidence_estimates_PD() 
    validate(need(ncol(table)>1, 
                  "No results for selected inputs"))
    
    if(is.null(input$incidence_plot_group_PD)){
      if(!is.null(input$incidence_plot_facet_PD)){
        p<-table %>% 
          unite("facet_var", 
                c(all_of(input$incidence_plot_facet_PD)), remove = FALSE, sep = "; ") %>% 
          ggplot(aes_string(x=input$incidence_x_axis_PD, y="incidence_100000_pys",
                            ymin = "incidence_100000_pys_95CI_lower",
                            ymax = "incidence_100000_pys_95CI_upper")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0) +
          facet_wrap(vars(facet_var),ncol = 2)+
          scale_y_continuous(
            limits = c(0, NA)
          ) +
          theme_bw()
      } else{
        p<-table %>% 
          ggplot(aes_string(x=input$incidence_x_axis_PD, y="incidence_100000_pys",
                            ymin = "incidence_100000_pys_95CI_lower",
                            ymax = "incidence_100000_pys_95CI_upper")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0) +
          scale_y_continuous(
            limits = c(0, NA)
          ) +
          theme_bw()        
      }
    } 
    
    
    if(!is.null(input$incidence_plot_group_PD) ){ 
      
      if(is.null(input$incidence_plot_facet_PD) ){ 
        p<-table %>% 
          unite("Group", 
                c(all_of(input$incidence_plot_group_PD)), remove = FALSE, sep = "; ") %>% 
          ggplot(aes_string(x=input$incidence_x_axis_PD, y="incidence_100000_pys",
                            ymin = "incidence_100000_pys_95CI_lower",
                            ymax = "incidence_100000_pys_95CI_upper",
                            group="Group",
                            colour="Group")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0, position=position_dodge(width=1)) +
          theme_bw()
      }
      
      if(!is.null(input$incidence_plot_facet_PD) ){
        if(!is.null(input$incidence_plot_group_PD) ){ 
          p<-table %>% 
            unite("Group", 
                  c(all_of(input$incidence_plot_group_PD)), remove = FALSE, sep = "; ") %>% 
            unite("facet_var", 
                  c(all_of(input$incidence_plot_facet_PD)), remove = FALSE, sep = "; ") %>% 
            ggplot(aes_string(x=input$incidence_x_axis_PD, y="incidence_100000_pys",
                              ymin = "incidence_100000_pys_95CI_lower",
                              ymax = "incidence_100000_pys_95CI_upper",
                              group="Group",
                              colour="Group")) +
            geom_point(position=position_dodge(width=1))+
            geom_errorbar(width=0, position=position_dodge(width=1)) +
            facet_wrap(vars(facet_var),ncol = 2)+  
            scale_y_continuous(
              limits = c(0, NA)
            )  +
            theme_bw()
        }
      }
      
    }
    
    p
    
  })
  
  get_incidence_attrition_PD<-reactive({
    
    table<-incidence_attrition_drugPD %>% 
      # first deselect settings which did not vary for this study
      select(!c(analysis_id, 
                analysis_interval,
                analysis_repeated_events,analysis_outcome_washout,
                analysis_complete_database_intervals,
                denominator_start_date,
                denominator_end_date)) %>% 
      filter(database_name %in% input$incidence_database_name_selector_PD)    %>% 
      filter(denominator_age_group %in% input$incidence_denominator_age_group_selector_PD)     %>% 
      filter(denominator_sex %in% input$incidence_denominator_sex_selector_PD)     %>% 
      filter(denominator_days_prior_history %in% input$incidence_denominator_days_prior_history_selector_PD)     %>% 
      filter(outcome_cohort_name %in% input$incidence_outcome_cohort_name_selector_PD)  
    
    table
  }) 
  
  output$tbl_incidence_attrition_PD<-  renderDataTable({
    
    table<-get_incidence_attrition_PD() 
    
    validate(need(ncol(table)>1, 
                  "No results for selected inputs"))
    
    table <- table %>% 
      select(!c("analysis_min_cell_count",
                "denominator_cohort_id", "outcome_cohort_id")) 
    
    datatable(table,
              rownames= FALSE,
              # colnames = c('Variable' = 1),
              extensions = 'Buttons',
              # options = list(lengthChange = FALSE,
              #                pageLength = 40,
              #                buttons = c('copy', 'csv', 'excel')))
              options = list(lengthChange = FALSE,
                             pageLength = 94,
                             dom = 'tB',
                             buttons = list(list(extend = "excel", 
                                                 text = "Download table as excel",
                                                 filename = "PatientProfiles.csv"))
              ))
  })  
  
  # incidence Parkinsonism
  get_incidence_estimates_Parkinsonism<-reactive({
    
    table<-incidence_estimates_drugParkinsonism %>% 
      # first deselect settings which did not vary for this study
      select(!c(analysis_id, 
                analysis_interval,
                analysis_complete_database_intervals,
                denominator_start_date,
                denominator_end_date)) %>% 
      filter(database_name %in% input$incidence_database_name_selector_Parkinsonism)  %>% 
      filter(as.character(incidence_start_date) %in% input$incidence_start_date_selector_Parkinsonism)  %>% 
      filter(denominator_age_group %in% input$incidence_denominator_age_group_selector_Parkinsonism)     %>% 
      filter(denominator_sex %in% input$incidence_denominator_sex_selector_Parkinsonism)     %>% 
      filter(denominator_days_prior_history %in% input$incidence_denominator_days_prior_history_selector_Parkinsonism)   %>% 
      filter(outcome_cohort_name %in% input$incidence_outcome_cohort_name_selector_Parkinsonism)  
    
    table
  }) 
  
  output$tbl_incidence_estimates_Parkinsonism<-  renderDataTable({
    
    table<-get_incidence_estimates_Parkinsonism() 
    
    validate(need(ncol(table)>1, 
                  "No results for selected inputs"))
    
    table <- table %>% 
      mutate(incidence_100000_pys=nice.num.count(incidence_100000_pys)) %>% 
      mutate(incidence_100000_pys_95CI_lower=nice.num.count(incidence_100000_pys_95CI_lower)) %>% 
      mutate(incidence_100000_pys_95CI_upper=nice.num.count(incidence_100000_pys_95CI_upper)) %>% 
      mutate(incidence_100000_pys= ifelse(!is.na(incidence_100000_pys),
                                          paste0(incidence_100000_pys, " (",
                                                 incidence_100000_pys_95CI_lower," to ", 
                                                 incidence_100000_pys_95CI_upper, " )"))) %>% 
      select(!c("incidence_100000_pys_95CI_lower", "incidence_100000_pys_95CI_upper",
                "cohort_obscured", "result_obscured","person_days",
                "analysis_min_cell_count","analysis_repeated_events",
                "analysis_outcome_washout",
                "denominator_cohort_id", "outcome_cohort_id")) %>% 
      mutate(n_persons=nice.num.count(n_persons)) %>% 
      mutate(n_events=nice.num.count(n_events)) %>% 
      mutate(person_years=nice.num.count(person_years)) %>% 
      relocate(incidence_start_date) %>% 
      relocate(incidence_end_date, .after = incidence_start_date) %>% 
      relocate(person_years, .after = n_persons)
    datatable(table,
              rownames= FALSE,
              # colnames = c('Variable' = 1),
              extensions = 'Buttons',
              # options = list(lengthChange = FALSE,
              #                pageLength = 40,
              #                buttons = c('copy', 'csv', 'excel')))
              options = list(lengthChange = FALSE,
                             pageLength = 94,
                             dom = 'tB',
                             buttons = list(list(extend = "excel", 
                                                 text = "Download table as excel",
                                                 filename = "PatientProfiles.csv"))
              ))
  } )
  
  output$plot_incidence_estimates_Parkinsonism<- renderPlotly({ 
    
    table<-get_incidence_estimates_Parkinsonism() 
    validate(need(ncol(table)>1, 
                  "No results for selected inputs"))
    
    if(is.null(input$incidence_plot_group_Parkinsonism)){
      if(!is.null(input$incidence_plot_facet_Parkinsonism)){
        p<-table %>% 
          unite("facet_var", 
                c(all_of(input$incidence_plot_facet_Parkinsonism)), remove = FALSE, sep = "; ") %>% 
          ggplot(aes_string(x=input$incidence_x_axis_Parkinsonism, y="incidence_100000_pys",
                            ymin = "incidence_100000_pys_95CI_lower",
                            ymax = "incidence_100000_pys_95CI_upper")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0) +
          facet_wrap(vars(facet_var),ncol = 2)+
          scale_y_continuous(
            limits = c(0, NA)
          ) +
          theme_bw()
      } else{
        p<-table %>% 
          ggplot(aes_string(x=input$incidence_x_axis_Parkinsonism, y="incidence_100000_pys",
                            ymin = "incidence_100000_pys_95CI_lower",
                            ymax = "incidence_100000_pys_95CI_upper")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0) +
          scale_y_continuous(
            limits = c(0, NA)
          ) +
          theme_bw()        
      }
    } 
    
    
    if(!is.null(input$incidence_plot_group_Parkinsonism) ){ 
      
      if(is.null(input$incidence_plot_facet_Parkinsonism) ){ 
        p<-table %>% 
          unite("Group", 
                c(all_of(input$incidence_plot_group_Parkinsonism)), remove = FALSE, sep = "; ") %>% 
          ggplot(aes_string(x=input$incidence_x_axis_Parkinsonism, y="incidence_100000_pys",
                            ymin = "incidence_100000_pys_95CI_lower",
                            ymax = "incidence_100000_pys_95CI_upper",
                            group="Group",
                            colour="Group")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0, position=position_dodge(width=1)) +
          theme_bw()
      }
      
      if(!is.null(input$incidence_plot_facet_Parkinsonism) ){
        if(!is.null(input$incidence_plot_group_Parkinsonism) ){ 
          p<-table %>% 
            unite("Group", 
                  c(all_of(input$incidence_plot_group_Parkinsonism)), remove = FALSE, sep = "; ") %>% 
            unite("facet_var", 
                  c(all_of(input$incidence_plot_facet_Parkinsonism)), remove = FALSE, sep = "; ") %>% 
            ggplot(aes_string(x=input$incidence_x_axis_Parkinsonism, y="incidence_100000_pys",
                              ymin = "incidence_100000_pys_95CI_lower",
                              ymax = "incidence_100000_pys_95CI_upper",
                              group="Group",
                              colour="Group")) +
            geom_point(position=position_dodge(width=1))+
            geom_errorbar(width=0, position=position_dodge(width=1)) +
            facet_wrap(vars(facet_var),ncol = 2)+  
            scale_y_continuous(
              limits = c(0, NA)
            )  +
            theme_bw()
        }
      }
      
    }
    
    p
    
  })
  
  get_incidence_attrition_Parkinsonism<-reactive({
    
    table<-incidence_attrition_drugParkinsonism %>% 
      # first deselect settings which did not vary for this study
      select(!c(analysis_id, 
                analysis_interval,
                analysis_repeated_events,analysis_outcome_washout,
                analysis_complete_database_intervals,
                denominator_start_date,
                denominator_end_date)) %>% 
      filter(database_name %in% input$incidence_database_name_selector_Parkinsonism)    %>% 
      filter(denominator_age_group %in% input$incidence_denominator_age_group_selector_Parkinsonism)     %>% 
      filter(denominator_sex %in% input$incidence_denominator_sex_selector_Parkinsonism)     %>% 
      filter(denominator_days_prior_history %in% input$incidence_denominator_days_prior_history_selector_Parkinsonism)     %>% 
      filter(outcome_cohort_name %in% input$incidence_outcome_cohort_name_selector_Parkinsonism)  
    
    table
  }) 
  
  output$tbl_incidence_attrition_Parkinsonism<-  renderDataTable({
    
    table<-get_incidence_attrition_Parkinsonism() 
    
    validate(need(ncol(table)>1, 
                  "No results for selected inputs"))
    
    table <- table %>% 
      select(!c("analysis_min_cell_count",
                "denominator_cohort_id", "outcome_cohort_id")) 
    
    datatable(table,
              rownames= FALSE,
              # colnames = c('Variable' = 1),
              extensions = 'Buttons',
              # options = list(lengthChange = FALSE,
              #                pageLength = 40,
              #                buttons = c('copy', 'csv', 'excel')))
              options = list(lengthChange = FALSE,
                             pageLength = 94,
                             dom = 'tB',
                             buttons = list(list(extend = "excel", 
                                                 text = "Download table as excel",
                                                 filename = "PatientProfiles.csv"))
              ))
  })  
  
  # incidence VP
  get_incidence_estimates_VP<-reactive({
    
    table<-incidence_estimates_drugVP %>% 
      # first deselect settings which did not vary for this study
      select(!c(analysis_id, 
                analysis_interval,
                analysis_complete_database_intervals,
                denominator_start_date,
                denominator_end_date)) %>% 
      filter(database_name %in% input$incidence_database_name_selector_VP)  %>% 
      filter(as.character(incidence_start_date) %in% input$incidence_start_date_selector_VP)  %>% 
      filter(denominator_age_group %in% input$incidence_denominator_age_group_selector_VP)     %>% 
      filter(denominator_sex %in% input$incidence_denominator_sex_selector_VP)     %>% 
      filter(denominator_days_prior_history %in% input$incidence_denominator_days_prior_history_selector_VP)   %>% 
      filter(outcome_cohort_name %in% input$incidence_outcome_cohort_name_selector_VP)  
    
    table
  }) 
  
  output$tbl_incidence_estimates_VP<-  renderDataTable({
    
    table<-get_incidence_estimates_VP() 
    
    validate(need(ncol(table)>1, 
                  "No results for selected inputs"))
    
    table <- table %>% 
      mutate(incidence_100000_pys=nice.num.count(incidence_100000_pys)) %>% 
      mutate(incidence_100000_pys_95CI_lower=nice.num.count(incidence_100000_pys_95CI_lower)) %>% 
      mutate(incidence_100000_pys_95CI_upper=nice.num.count(incidence_100000_pys_95CI_upper)) %>% 
      mutate(incidence_100000_pys= ifelse(!is.na(incidence_100000_pys),
                                          paste0(incidence_100000_pys, " (",
                                                 incidence_100000_pys_95CI_lower," to ", 
                                                 incidence_100000_pys_95CI_upper, " )"))) %>% 
      select(!c("incidence_100000_pys_95CI_lower", "incidence_100000_pys_95CI_upper",
                "cohort_obscured", "result_obscured","person_days",
                "analysis_min_cell_count","analysis_repeated_events",
                "analysis_outcome_washout",
                "denominator_cohort_id", "outcome_cohort_id")) %>% 
      mutate(n_persons=nice.num.count(n_persons)) %>% 
      mutate(n_events=nice.num.count(n_events)) %>% 
      mutate(person_years=nice.num.count(person_years)) %>% 
      relocate(incidence_start_date) %>% 
      relocate(incidence_end_date, .after = incidence_start_date) %>% 
      relocate(person_years, .after = n_persons)
    datatable(table,
              rownames= FALSE,
              # colnames = c('Variable' = 1),
              extensions = 'Buttons',
              # options = list(lengthChange = FALSE,
              #                pageLength = 40,
              #                buttons = c('copy', 'csv', 'excel')))
              options = list(lengthChange = FALSE,
                             pageLength = 94,
                             dom = 'tB',
                             buttons = list(list(extend = "excel", 
                                                 text = "Download table as excel",
                                                 filename = "PatientProfiles.csv"))
              ))
  } )
  
  output$plot_incidence_estimates_VP<- renderPlotly({ 
    
    table<-get_incidence_estimates_VP() 
    validate(need(ncol(table)>1, 
                  "No results for selected inputs"))
    
    if(is.null(input$incidence_plot_group_VP)){
      if(!is.null(input$incidence_plot_facet_VP)){
        p<-table %>% 
          unite("facet_var", 
                c(all_of(input$incidence_plot_facet_VP)), remove = FALSE, sep = "; ") %>% 
          ggplot(aes_string(x=input$incidence_x_axis_VP, y="incidence_100000_pys",
                            ymin = "incidence_100000_pys_95CI_lower",
                            ymax = "incidence_100000_pys_95CI_upper")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0) +
          facet_wrap(vars(facet_var),ncol = 2)+
          scale_y_continuous(
            limits = c(0, NA)
          ) +
          theme_bw()
      } else{
        p<-table %>% 
          ggplot(aes_string(x=input$incidence_x_axis_VP, y="incidence_100000_pys",
                            ymin = "incidence_100000_pys_95CI_lower",
                            ymax = "incidence_100000_pys_95CI_upper")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0) +
          scale_y_continuous(
            limits = c(0, NA)
          ) +
          theme_bw()        
      }
    } 
    
    
    if(!is.null(input$incidence_plot_group_VP) ){ 
      
      if(is.null(input$incidence_plot_facet_VP) ){ 
        p<-table %>% 
          unite("Group", 
                c(all_of(input$incidence_plot_group_VP)), remove = FALSE, sep = "; ") %>% 
          ggplot(aes_string(x=input$incidence_x_axis_VP, y="incidence_100000_pys",
                            ymin = "incidence_100000_pys_95CI_lower",
                            ymax = "incidence_100000_pys_95CI_upper",
                            group="Group",
                            colour="Group")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0, position=position_dodge(width=1)) +
          theme_bw()
      }
      
      if(!is.null(input$incidence_plot_facet_VP) ){
        if(!is.null(input$incidence_plot_group_VP) ){ 
          p<-table %>% 
            unite("Group", 
                  c(all_of(input$incidence_plot_group_VP)), remove = FALSE, sep = "; ") %>% 
            unite("facet_var", 
                  c(all_of(input$incidence_plot_facet_VP)), remove = FALSE, sep = "; ") %>% 
            ggplot(aes_string(x=input$incidence_x_axis_VP, y="incidence_100000_pys",
                              ymin = "incidence_100000_pys_95CI_lower",
                              ymax = "incidence_100000_pys_95CI_upper",
                              group="Group",
                              colour="Group")) +
            geom_point(position=position_dodge(width=1))+
            geom_errorbar(width=0, position=position_dodge(width=1)) +
            facet_wrap(vars(facet_var),ncol = 2)+  
            scale_y_continuous(
              limits = c(0, NA)
            )  +
            theme_bw()
        }
      }
      
    }
    
    p
    
  })
  
  get_incidence_attrition_VP<-reactive({
    
    table<-incidence_attrition_drugVP %>% 
      # first deselect settings which did not vary for this study
      select(!c(analysis_id, 
                analysis_interval,
                analysis_repeated_events,analysis_outcome_washout,
                analysis_complete_database_intervals,
                denominator_start_date,
                denominator_end_date)) %>% 
      filter(database_name %in% input$incidence_database_name_selector_VP)    %>% 
      filter(denominator_age_group %in% input$incidence_denominator_age_group_selector_VP)     %>% 
      filter(denominator_sex %in% input$incidence_denominator_sex_selector_VP)     %>% 
      filter(denominator_days_prior_history %in% input$incidence_denominator_days_prior_history_selector_VP)     %>% 
      filter(outcome_cohort_name %in% input$incidence_outcome_cohort_name_selector_VP)  
    
    table
  }) 
  
  output$tbl_incidence_attrition_VP<-  renderDataTable({
    
    table<-get_incidence_attrition_VP() 
    
    validate(need(ncol(table)>1, 
                  "No results for selected inputs"))
    
    table <- table %>% 
      select(!c("analysis_min_cell_count",
                "denominator_cohort_id", "outcome_cohort_id")) 
    
    datatable(table,
              rownames= FALSE,
              # colnames = c('Variable' = 1),
              extensions = 'Buttons',
              # options = list(lengthChange = FALSE,
              #                pageLength = 40,
              #                buttons = c('copy', 'csv', 'excel')))
              options = list(lengthChange = FALSE,
                             pageLength = 94,
                             dom = 'tB',
                             buttons = list(list(extend = "excel", 
                                                 text = "Download table as excel",
                                                 filename = "PatientProfiles.csv"))
              ))
  })  
  
  # incidence DIP
  get_incidence_estimates_DIP<-reactive({
    
    table<-incidence_estimates_drugDIP %>% 
      # first deselect settings which did not vary for this study
      select(!c(analysis_id, 
                analysis_interval,
                analysis_complete_database_intervals,
                denominator_start_date,
                denominator_end_date)) %>% 
      filter(database_name %in% input$incidence_database_name_selector_DIP)  %>% 
      filter(as.character(incidence_start_date) %in% input$incidence_start_date_selector_DIP)  %>% 
      filter(denominator_age_group %in% input$incidence_denominator_age_group_selector_DIP)     %>% 
      filter(denominator_sex %in% input$incidence_denominator_sex_selector_DIP)     %>% 
      filter(denominator_days_prior_history %in% input$incidence_denominator_days_prior_history_selector_DIP)   %>% 
      filter(outcome_cohort_name %in% input$incidence_outcome_cohort_name_selector_DIP)  
    
    table
  }) 
  
  output$tbl_incidence_estimates_DIP<-  renderDataTable({
    
    table<-get_incidence_estimates_DIP() 
    
    validate(need(ncol(table)>1, 
                  "No results for selected inputs"))
    
    table <- table %>% 
      mutate(incidence_100000_pys=nice.num.count(incidence_100000_pys)) %>% 
      mutate(incidence_100000_pys_95CI_lower=nice.num.count(incidence_100000_pys_95CI_lower)) %>% 
      mutate(incidence_100000_pys_95CI_upper=nice.num.count(incidence_100000_pys_95CI_upper)) %>% 
      mutate(incidence_100000_pys= ifelse(!is.na(incidence_100000_pys),
                                          paste0(incidence_100000_pys, " (",
                                                 incidence_100000_pys_95CI_lower," to ", 
                                                 incidence_100000_pys_95CI_upper, " )"))) %>% 
      select(!c("incidence_100000_pys_95CI_lower", "incidence_100000_pys_95CI_upper",
                "cohort_obscured", "result_obscured","person_days",
                "analysis_min_cell_count","analysis_repeated_events",
                "analysis_outcome_washout",
                "denominator_cohort_id", "outcome_cohort_id")) %>% 
      mutate(n_persons=nice.num.count(n_persons)) %>% 
      mutate(n_events=nice.num.count(n_events)) %>% 
      mutate(person_years=nice.num.count(person_years)) %>% 
      relocate(incidence_start_date) %>% 
      relocate(incidence_end_date, .after = incidence_start_date) %>% 
      relocate(person_years, .after = n_persons)
    datatable(table,
              rownames= FALSE,
              # colnames = c('Variable' = 1),
              extensions = 'Buttons',
              # options = list(lengthChange = FALSE,
              #                pageLength = 40,
              #                buttons = c('copy', 'csv', 'excel')))
              options = list(lengthChange = FALSE,
                             pageLength = 94,
                             dom = 'tB',
                             buttons = list(list(extend = "excel", 
                                                 text = "Download table as excel",
                                                 filename = "PatientProfiles.csv"))
              ))
  } )
  
  output$plot_incidence_estimates_DIP<- renderPlotly({ 
    
    table<-get_incidence_estimates_DIP() 
    validate(need(ncol(table)>1, 
                  "No results for selected inputs"))
    
    if(is.null(input$incidence_plot_group_DIP)){
      if(!is.null(input$incidence_plot_facet_DIP)){
        p<-table %>% 
          unite("facet_var", 
                c(all_of(input$incidence_plot_facet_DIP)), remove = FALSE, sep = "; ") %>% 
          ggplot(aes_string(x=input$incidence_x_axis_DIP, y="incidence_100000_pys",
                            ymin = "incidence_100000_pys_95CI_lower",
                            ymax = "incidence_100000_pys_95CI_upper")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0) +
          facet_wrap(vars(facet_var),ncol = 2)+
          scale_y_continuous(
            limits = c(0, NA)
          ) +
          theme_bw()
      } else{
        p<-table %>% 
          ggplot(aes_string(x=input$incidence_x_axis_DIP, y="incidence_100000_pys",
                            ymin = "incidence_100000_pys_95CI_lower",
                            ymax = "incidence_100000_pys_95CI_upper")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0) +
          scale_y_continuous(
            limits = c(0, NA)
          ) +
          theme_bw()        
      }
    } 
    
    
    if(!is.null(input$incidence_plot_group_DIP) ){ 
      
      if(is.null(input$incidence_plot_facet_DIP) ){ 
        p<-table %>% 
          unite("Group", 
                c(all_of(input$incidence_plot_group_DIP)), remove = FALSE, sep = "; ") %>% 
          ggplot(aes_string(x=input$incidence_x_axis_DIP, y="incidence_100000_pys",
                            ymin = "incidence_100000_pys_95CI_lower",
                            ymax = "incidence_100000_pys_95CI_upper",
                            group="Group",
                            colour="Group")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0, position=position_dodge(width=1)) +
          theme_bw()
      }
      
      if(!is.null(input$incidence_plot_facet_DIP) ){
        if(!is.null(input$incidence_plot_group_DIP) ){ 
          p<-table %>% 
            unite("Group", 
                  c(all_of(input$incidence_plot_group_DIP)), remove = FALSE, sep = "; ") %>% 
            unite("facet_var", 
                  c(all_of(input$incidence_plot_facet_DIP)), remove = FALSE, sep = "; ") %>% 
            ggplot(aes_string(x=input$incidence_x_axis_DIP, y="incidence_100000_pys",
                              ymin = "incidence_100000_pys_95CI_lower",
                              ymax = "incidence_100000_pys_95CI_upper",
                              group="Group",
                              colour="Group")) +
            geom_point(position=position_dodge(width=1))+
            geom_errorbar(width=0, position=position_dodge(width=1)) +
            facet_wrap(vars(facet_var),ncol = 2)+  
            scale_y_continuous(
              limits = c(0, NA)
            )  +
            theme_bw()
        }
      }
      
    }
    
    p
    
  })
  
  get_incidence_attrition_DIP<-reactive({
    
    table<-incidence_attrition_drugDIP %>% 
      # first deselect settings which did not vary for this study
      select(!c(analysis_id, 
                analysis_interval,
                analysis_repeated_events,analysis_outcome_washout,
                analysis_complete_database_intervals,
                denominator_start_date,
                denominator_end_date)) %>% 
      filter(database_name %in% input$incidence_database_name_selector_DIP)    %>% 
      filter(denominator_age_group %in% input$incidence_denominator_age_group_selector_DIP)     %>% 
      filter(denominator_sex %in% input$incidence_denominator_sex_selector_DIP)     %>% 
      filter(denominator_days_prior_history %in% input$incidence_denominator_days_prior_history_selector_DIP)     %>% 
      filter(outcome_cohort_name %in% input$incidence_outcome_cohort_name_selector_DIP)  
    
    table
  }) 
  
  output$tbl_incidence_attrition_DIP<-  renderDataTable({
    
    table<-get_incidence_attrition_DIP() 
    
    validate(need(ncol(table)>1, 
                  "No results for selected inputs"))
    
    table <- table %>% 
      select(!c("analysis_min_cell_count",
                "denominator_cohort_id", "outcome_cohort_id")) 
    
    datatable(table,
              rownames= FALSE,
              # colnames = c('Variable' = 1),
              extensions = 'Buttons',
              # options = list(lengthChange = FALSE,
              #                pageLength = 40,
              #                buttons = c('copy', 'csv', 'excel')))
              options = list(lengthChange = FALSE,
                             pageLength = 94,
                             dom = 'tB',
                             buttons = list(list(extend = "excel", 
                                                 text = "Download table as excel",
                                                 filename = "PatientProfiles.csv"))
              ))
  })  
  
}