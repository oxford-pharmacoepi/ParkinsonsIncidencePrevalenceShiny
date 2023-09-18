# Data prep functions -----
# study specific reformatting of results
prepare_output<-function(result){
  result<- result %>% 
    mutate(denominator_age_group= stringr::str_replace(denominator_age_group, ";", " to ")) %>% 
    mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18 to 150", "All ages")) %>% 
    mutate(denominator_age_group = factor(denominator_age_group,
                                          levels = c("All ages",
                                                     "18 to 30", 
                                                     "31 to 40",
                                                     "41 to 50", 
                                                     "51 to 60",
                                                     "61 to 70", 
                                                     "71 to 80",
                                                     "81 to 150")))
  
  return(result)
}

# printing numbers with 3 decimal place and commas 
nice.num3<-function(x) {
  trimws(format(round(x,3),
                big.mark=",", nsmall = 3, digits=3, scientific=FALSE))}

# Load, prepare, and merge results -----
results<-list.files(here("networkResults"), full.names = TRUE)

# DISEASE
# prevalence estimates
prevalence_estimates_files<-results[stringr::str_detect(results, ".csv")]
prevalence_estimates_files<-results[stringr::str_detect(results, "prevalence_estimates")]
prevalence_estimates <- list()
for(i in seq_along(prevalence_estimates_files)){
  prevalence_estimates[[i]]<-readr::read_csv(prevalence_estimates_files[[i]], 
                                         show_col_types = FALSE)  
}
prevalence_estimates <- dplyr::bind_rows(prevalence_estimates)
prevalence_estimates <- prepare_output(prevalence_estimates)
prevalence_estimates <- prevalence_estimates %>% 
      mutate("Prevalence (95% CI)"= ifelse(!is.na(prevalence),
               paste0(paste0(nice.num3(prevalence*100), "%"), " (",
                                paste0(nice.num3(prevalence_95CI_lower*100), "%")," to ", 
                                paste0(nice.num3(prevalence_95CI_upper*100), "%"), ")"),
               NA
               ))
saveRDS(prevalence_estimates, 
          here("shiny", "data", "prevalence_estimates.rds"))

# prevalence attrition
prevalence_attrition_files<-results[stringr::str_detect(results, ".csv")]
prevalence_attrition_files<-results[stringr::str_detect(results, "prevalence_attrition")]
prevalence_attrition <- list()
for(i in seq_along(prevalence_attrition_files)){
  prevalence_attrition[[i]]<-readr::read_csv(prevalence_attrition_files[[i]], 
                                             show_col_types = FALSE)  
}
prevalence_attrition <- dplyr::bind_rows(prevalence_attrition)
prevalence_attrition <- prepare_output(prevalence_attrition)
saveRDS(prevalence_attrition, 
          here("shiny", "data", "prevalence_attrition.rds"))


# incidence estimates
incidence_estimates_files<-results[stringr::str_detect(results, ".csv")]
incidence_estimates_files<-results[stringr::str_detect(results, "incidence_estimates")]
incidence_estimates <- list()
for(i in seq_along(incidence_estimates_files)){
  incidence_estimates[[i]]<-readr::read_csv(incidence_estimates_files[[i]], 
                                             show_col_types = FALSE)  
}
incidence_estimates <- dplyr::bind_rows(incidence_estimates)
incidence_estimates <- prepare_output(incidence_estimates)
saveRDS(incidence_estimates, 
          here("shiny", "data", "incidence_estimates.rds"))

# incidence attrition
incidence_attrition_files<-results[stringr::str_detect(results, ".csv")]
incidence_attrition_files<-results[stringr::str_detect(results, "incidence_attrition")]
incidence_attrition <- list()
for(i in seq_along(incidence_attrition_files)){
  incidence_attrition[[i]]<-readr::read_csv(incidence_attrition_files[[i]], 
                                             show_col_types = FALSE)  
}
incidence_attrition <- dplyr::bind_rows(incidence_attrition)
incidence_attrition <- prepare_output(incidence_attrition)
saveRDS(incidence_attrition, 
          here("shiny", "data", "incidence_attrition.rds"))


# DRUGS IN PD

results1<-list.files(here("networkResults", "DruginPD"), full.names = TRUE)

# prevalence estimates
prevalence_estimates_files<-results1[stringr::str_detect(results1, ".csv")]
prevalence_estimates_files<-results1[stringr::str_detect(results1, "prevalence_estimates")]
prevalence_estimates <- list()
for(i in seq_along(prevalence_estimates_files)){
  prevalence_estimates[[i]]<-readr::read_csv(prevalence_estimates_files[[i]], 
                                             show_col_types = FALSE)  
}
prevalence_estimates <- dplyr::bind_rows(prevalence_estimates)
prevalence_estimates <- prepare_output(prevalence_estimates)
prevalence_estimates <- prevalence_estimates %>% 
  mutate("Prevalence (95% CI)"= ifelse(!is.na(prevalence),
                                       paste0(paste0(nice.num3(prevalence*100), "%"), " (",
                                              paste0(nice.num3(prevalence_95CI_lower*100), "%")," to ", 
                                              paste0(nice.num3(prevalence_95CI_upper*100), "%"), ")"),
                                       NA
  ))
saveRDS(prevalence_estimates, 
        here("shiny", "data", "prevalence_estimates_drugsPD.rds"))

# prevalence attrition
prevalence_attrition_files<-results1[stringr::str_detect(results1, ".csv")]
prevalence_attrition_files<-results1[stringr::str_detect(results1, "prevalence_attrition")]
prevalence_attrition <- list()
for(i in seq_along(prevalence_attrition_files)){
  prevalence_attrition[[i]]<-readr::read_csv(prevalence_attrition_files[[i]], 
                                             show_col_types = FALSE)  
}
prevalence_attrition <- dplyr::bind_rows(prevalence_attrition)
prevalence_attrition <- prepare_output(prevalence_attrition)
saveRDS(prevalence_attrition, 
        here("shiny", "data", "prevalence_attrition_drugsPD.rds"))


# incidence estimates
incidence_estimates_files<-results1[stringr::str_detect(results1, ".csv")]
incidence_estimates_files<-results1[stringr::str_detect(results1, "incidence_estimates")]
incidence_estimates <- list()
for(i in seq_along(incidence_estimates_files)){
  incidence_estimates[[i]]<-readr::read_csv(incidence_estimates_files[[i]], 
                                            show_col_types = FALSE)  
}
incidence_estimates <- dplyr::bind_rows(incidence_estimates)
incidence_estimates <- prepare_output(incidence_estimates)
saveRDS(incidence_estimates, 
        here("shiny", "data", "incidence_estimates_drugsPD.rds"))

# incidence attrition
incidence_attrition_files<-results1[stringr::str_detect(results1, ".csv")]
incidence_attrition_files<-results1[stringr::str_detect(results1, "incidence_attrition")]
incidence_attrition <- list()
for(i in seq_along(incidence_attrition_files)){
  incidence_attrition[[i]]<-readr::read_csv(incidence_attrition_files[[i]], 
                                            show_col_types = FALSE)  
}
incidence_attrition <- dplyr::bind_rows(incidence_attrition)
incidence_attrition <- prepare_output(incidence_attrition)
saveRDS(incidence_attrition, 
        here("shiny", "data", "incidence_attrition_drugsPD.rds"))


# DRUGS IN DIP

results2<-list.files(here("networkResults", "DruginDIP"), full.names = TRUE)

# prevalence estimates
prevalence_estimates_files<-results2[stringr::str_detect(results2, ".csv")]
prevalence_estimates_files<-results2[stringr::str_detect(results2, "prevalence_estimates")]
prevalence_estimates <- list()
for(i in seq_along(prevalence_estimates_files)){
  prevalence_estimates[[i]]<-readr::read_csv(prevalence_estimates_files[[i]], 
                                             show_col_types = FALSE)  
}
prevalence_estimates <- dplyr::bind_rows(prevalence_estimates)
prevalence_estimates <- prepare_output(prevalence_estimates)
prevalence_estimates <- prevalence_estimates %>% 
  mutate("Prevalence (95% CI)"= ifelse(!is.na(prevalence),
                                       paste0(paste0(nice.num3(prevalence*100), "%"), " (",
                                              paste0(nice.num3(prevalence_95CI_lower*100), "%")," to ", 
                                              paste0(nice.num3(prevalence_95CI_upper*100), "%"), ")"),
                                       NA
  ))
saveRDS(prevalence_estimates, 
        here("shiny", "data", "prevalence_estimates_drugsDIP.rds"))

# prevalence attrition
prevalence_attrition_files<-results2[stringr::str_detect(results2, ".csv")]
prevalence_attrition_files<-results2[stringr::str_detect(results2, "prevalence_attrition")]
prevalence_attrition <- list()
for(i in seq_along(prevalence_attrition_files)){
  prevalence_attrition[[i]]<-readr::read_csv(prevalence_attrition_files[[i]], 
                                             show_col_types = FALSE)  
}
prevalence_attrition <- dplyr::bind_rows(prevalence_attrition)
prevalence_attrition <- prepare_output(prevalence_attrition)
saveRDS(prevalence_attrition, 
        here("shiny", "data", "prevalence_attrition_drugsDIP.rds"))


# incidence estimates
incidence_estimates_files<-results2[stringr::str_detect(results2, ".csv")]
incidence_estimates_files<-results2[stringr::str_detect(results2, "incidence_estimates")]
incidence_estimates <- list()
for(i in seq_along(incidence_estimates_files)){
  incidence_estimates[[i]]<-readr::read_csv(incidence_estimates_files[[i]], 
                                            show_col_types = FALSE)  
}
incidence_estimates <- dplyr::bind_rows(incidence_estimates)
incidence_estimates <- prepare_output(incidence_estimates)
saveRDS(incidence_estimates, 
        here("shiny", "data", "incidence_estimates_drugsDIP.rds"))

# incidence attrition
incidence_attrition_files<-results2[stringr::str_detect(results2, ".csv")]
incidence_attrition_files<-results2[stringr::str_detect(results2, "incidence_attrition")]
incidence_attrition <- list()
for(i in seq_along(incidence_attrition_files)){
  incidence_attrition[[i]]<-readr::read_csv(incidence_attrition_files[[i]], 
                                            show_col_types = FALSE)  
}
incidence_attrition <- dplyr::bind_rows(incidence_attrition)
incidence_attrition <- prepare_output(incidence_attrition)
saveRDS(incidence_attrition, 
        here("shiny", "data", "incidence_attrition_drugsDIP.rds"))

# DRUGS IN VP

results3<-list.files(here("networkResults", "DruginVP"), full.names = TRUE)

# prevalence estimates
prevalence_estimates_files<-results3[stringr::str_detect(results3, ".csv")]
prevalence_estimates_files<-results3[stringr::str_detect(results3, "prevalence_estimates")]
prevalence_estimates <- list()
for(i in seq_along(prevalence_estimates_files)){
  prevalence_estimates[[i]]<-readr::read_csv(prevalence_estimates_files[[i]], 
                                             show_col_types = FALSE)  
}
prevalence_estimates <- dplyr::bind_rows(prevalence_estimates)
prevalence_estimates <- prepare_output(prevalence_estimates)
prevalence_estimates <- prevalence_estimates %>% 
  mutate("Prevalence (95% CI)"= ifelse(!is.na(prevalence),
                                       paste0(paste0(nice.num3(prevalence*100), "%"), " (",
                                              paste0(nice.num3(prevalence_95CI_lower*100), "%")," to ", 
                                              paste0(nice.num3(prevalence_95CI_upper*100), "%"), ")"),
                                       NA
  ))
saveRDS(prevalence_estimates, 
        here("shiny", "data", "prevalence_estimates_drugsVP.rds"))

# prevalence attrition
prevalence_attrition_files<-results3[stringr::str_detect(results3, ".csv")]
prevalence_attrition_files<-results3[stringr::str_detect(results3, "prevalence_attrition")]
prevalence_attrition <- list()
for(i in seq_along(prevalence_attrition_files)){
  prevalence_attrition[[i]]<-readr::read_csv(prevalence_attrition_files[[i]], 
                                             show_col_types = FALSE)  
}
prevalence_attrition <- dplyr::bind_rows(prevalence_attrition)
prevalence_attrition <- prepare_output(prevalence_attrition)
saveRDS(prevalence_attrition, 
        here("shiny", "data", "prevalence_attrition_drugsVP.rds"))


# incidence estimates
incidence_estimates_files<-results3[stringr::str_detect(results3, ".csv")]
incidence_estimates_files<-results3[stringr::str_detect(results3, "incidence_estimates")]
incidence_estimates <- list()
for(i in seq_along(incidence_estimates_files)){
  incidence_estimates[[i]]<-readr::read_csv(incidence_estimates_files[[i]], 
                                            show_col_types = FALSE)  
}
incidence_estimates <- dplyr::bind_rows(incidence_estimates)
incidence_estimates <- prepare_output(incidence_estimates)
saveRDS(incidence_estimates, 
        here("shiny", "data", "incidence_estimates_drugsVP.rds"))

# incidence attrition
incidence_attrition_files<-results3[stringr::str_detect(results3, ".csv")]
incidence_attrition_files<-results3[stringr::str_detect(results3, "incidence_attrition")]
incidence_attrition <- list()
for(i in seq_along(incidence_attrition_files)){
  incidence_attrition[[i]]<-readr::read_csv(incidence_attrition_files[[i]], 
                                            show_col_types = FALSE)  
}
incidence_attrition <- dplyr::bind_rows(incidence_attrition)
incidence_attrition <- prepare_output(incidence_attrition)
saveRDS(incidence_attrition, 
        here("shiny", "data", "incidence_attrition_drugsVP.rds"))

# DRUGS IN Parkinsonism

results4<-list.files(here("networkResults", "DruginParkinsonism"), full.names = TRUE)

# prevalence estimates
prevalence_estimates_files<-results4[stringr::str_detect(results4, ".csv")]
prevalence_estimates_files<-results4[stringr::str_detect(results4, "prevalence_estimates")]
prevalence_estimates <- list()
for(i in seq_along(prevalence_estimates_files)){
  prevalence_estimates[[i]]<-readr::read_csv(prevalence_estimates_files[[i]], 
                                             show_col_types = FALSE)  
}
prevalence_estimates <- dplyr::bind_rows(prevalence_estimates)
prevalence_estimates <- prepare_output(prevalence_estimates)
prevalence_estimates <- prevalence_estimates %>% 
  mutate("Prevalence (95% CI)"= ifelse(!is.na(prevalence),
                                       paste0(paste0(nice.num3(prevalence*100), "%"), " (",
                                              paste0(nice.num3(prevalence_95CI_lower*100), "%")," to ", 
                                              paste0(nice.num3(prevalence_95CI_upper*100), "%"), ")"),
                                       NA
  ))
saveRDS(prevalence_estimates, 
        here("shiny", "data", "prevalence_estimates_drugsParkinsonism.rds"))

# prevalence attrition
prevalence_attrition_files<-results4[stringr::str_detect(results4, ".csv")]
prevalence_attrition_files<-results4[stringr::str_detect(results4, "prevalence_attrition")]
prevalence_attrition <- list()
for(i in seq_along(prevalence_attrition_files)){
  prevalence_attrition[[i]]<-readr::read_csv(prevalence_attrition_files[[i]], 
                                             show_col_types = FALSE)  
}
prevalence_attrition <- dplyr::bind_rows(prevalence_attrition)
prevalence_attrition <- prepare_output(prevalence_attrition)
saveRDS(prevalence_attrition, 
        here("shiny", "data", "prevalence_attrition_drugsParkinsonism.rds"))


# incidence estimates
incidence_estimates_files<-results4[stringr::str_detect(results4, ".csv")]
incidence_estimates_files<-results4[stringr::str_detect(results4, "incidence_estimates")]
incidence_estimates <- list()
for(i in seq_along(incidence_estimates_files)){
  incidence_estimates[[i]]<-readr::read_csv(incidence_estimates_files[[i]], 
                                            show_col_types = FALSE)  
}
incidence_estimates <- dplyr::bind_rows(incidence_estimates)
incidence_estimates <- prepare_output(incidence_estimates)
saveRDS(incidence_estimates, 
        here("shiny", "data", "incidence_estimates_drugsParkinsonism.rds"))

# incidence attrition
incidence_attrition_files<-results4[stringr::str_detect(results4, ".csv")]
incidence_attrition_files<-results4[stringr::str_detect(results4, "incidence_attrition")]
incidence_attrition <- list()
for(i in seq_along(incidence_attrition_files)){
  incidence_attrition[[i]]<-readr::read_csv(incidence_attrition_files[[i]], 
                                            show_col_types = FALSE)  
}
incidence_attrition <- dplyr::bind_rows(incidence_attrition)
incidence_attrition <- prepare_output(incidence_attrition)
saveRDS(incidence_attrition, 
        here("shiny", "data", "incidence_attrition_drugsParkinsonism.rds"))

# DRUGS

results6<-list.files(here("networkResults", "Drug"), full.names = TRUE)

# prevalence estimates
prevalence_estimates_files<-results6[stringr::str_detect(results6, ".csv")]
prevalence_estimates_files<-results6[stringr::str_detect(results6, "prevalence_estimates")]
prevalence_estimates <- list()
for(i in seq_along(prevalence_estimates_files)){
  prevalence_estimates[[i]]<-readr::read_csv(prevalence_estimates_files[[i]], 
                                             show_col_types = FALSE)  
}
prevalence_estimates <- dplyr::bind_rows(prevalence_estimates)
prevalence_estimates <- prepare_output(prevalence_estimates)
prevalence_estimates <- prevalence_estimates %>% 
  mutate("Prevalence (95% CI)"= ifelse(!is.na(prevalence),
                                       paste0(paste0(nice.num3(prevalence*100), "%"), " (",
                                              paste0(nice.num3(prevalence_95CI_lower*100), "%")," to ", 
                                              paste0(nice.num3(prevalence_95CI_upper*100), "%"), ")"),
                                       NA
  ))
saveRDS(prevalence_estimates, 
        here("shiny", "data", "prevalence_estimates_drugs.rds"))

# prevalence attrition
prevalence_attrition_files<-results6[stringr::str_detect(results6, ".csv")]
prevalence_attrition_files<-results6[stringr::str_detect(results6, "prevalence_attrition")]
prevalence_attrition <- list()
for(i in seq_along(prevalence_attrition_files)){
  prevalence_attrition[[i]]<-readr::read_csv(prevalence_attrition_files[[i]], 
                                             show_col_types = FALSE)  
}
prevalence_attrition <- dplyr::bind_rows(prevalence_attrition)
prevalence_attrition <- prepare_output(prevalence_attrition)
saveRDS(prevalence_attrition, 
        here("shiny", "data", "prevalence_attrition_drugs.rds"))


# incidence estimates
incidence_estimates_files<-results6[stringr::str_detect(results6, ".csv")]
incidence_estimates_files<-results6[stringr::str_detect(results6, "incidence_estimates")]
incidence_estimates <- list()
for(i in seq_along(incidence_estimates_files)){
  incidence_estimates[[i]]<-readr::read_csv(incidence_estimates_files[[i]], 
                                            show_col_types = FALSE)  
}
incidence_estimates <- dplyr::bind_rows(incidence_estimates)
incidence_estimates <- prepare_output(incidence_estimates)
saveRDS(incidence_estimates, 
        here("shiny", "data", "incidence_estimates_drugs.rds"))

# incidence attrition
incidence_attrition_files<-results6[stringr::str_detect(results6, ".csv")]
incidence_attrition_files<-results6[stringr::str_detect(results6, "incidence_attrition")]
incidence_attrition <- list()
for(i in seq_along(incidence_attrition_files)){
  incidence_attrition[[i]]<-readr::read_csv(incidence_attrition_files[[i]], 
                                            show_col_types = FALSE)  
}
incidence_attrition <- dplyr::bind_rows(incidence_attrition)
incidence_attrition <- prepare_output(incidence_attrition)
saveRDS(incidence_attrition, 
        here("shiny", "data", "incidence_attrition_drugs.rds"))

