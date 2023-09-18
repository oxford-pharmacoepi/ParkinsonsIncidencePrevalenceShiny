#### If one is using the latest version, one may also wish to run this R file for different rds files 
# in order to be compatible with the current shiny. For example:
# rm(list = ls())
prevalence_estimates_drugs<-prevalence_estimates_drugs %>% select(-cdm_name) %>% mutate(database_name = "CPRD_GOLD")
saveRDS(prevalence_estimates_drugs, file = here::here("data", "prevalence_estimates_drugs.rds"))


