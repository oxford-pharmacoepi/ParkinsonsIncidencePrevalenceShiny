library(here)
library(dplyr)
library(tidyr)
library(stringr)

# printing numbers with 1 decimal place and commas 
nice.num<-function(x) {
  trimws(format(round(x,1),
                big.mark=",", nsmall = 1, digits=1, scientific=FALSE))}
# printing numbers with 2 decimal place and commas 
nice.num2<-function(x) {
  trimws(format(round(x,2),
                big.mark=",", nsmall = 2, digits=2, scientific=FALSE))}
# printing numbers with 3 decimal place and commas 
nice.num3<-function(x) {
  trimws(format(round(x,3),
                big.mark=",", nsmall = 3, digits=3, scientific=FALSE))}
# for counts- without decimal place
nice.num.count<-function(x) {
  trimws(format(x,
                big.mark=",", nsmall = 0, digits=1, scientific=FALSE))}



#### Load data -----
#data
prevalence_estimates <- readRDS(here("shiny", "data","prevalence_estimates.rds"))
prevalence_attrition <- readRDS(here("shiny", "data","prevalence_attrition.rds"))
incidence_estimates <- readRDS(here("shiny", "data","incidence_estimates.rds"))
incidence_attrition <- readRDS(here("shiny", "data","incidence_attrition.rds"))

# UPDATED THIS TO READ IN OTHER DATA
#drug
prevalence_estimates_drugs <- readRDS(here("shiny", "data","prevalence_estimates_drugs.rds"))
prevalence_attrition_drugs <- readRDS(here("shiny", "data","prevalence_attrition_drugs.rds"))
incidence_estimates_drugs <- readRDS(here("shiny", "data","incidence_estimates_drugs.rds"))
incidence_attrition_drugs <- readRDS(here("shiny", "data","incidence_attrition_drugs.rds"))

#drug in PD
prevalence_estimates_drugPD <- readRDS(here("shiny", "data","prevalence_estimates_drugsPD.rds"))
prevalence_attrition_drugPD <- readRDS(here("shiny", "data","prevalence_attrition_drugsPD.rds"))
incidence_estimates_drugPD <- readRDS(here("shiny", "data","incidence_estimates_drugsPD.rds"))
incidence_attrition_drugPD <- readRDS(here("shiny", "data","incidence_attrition_drugsPD.rds"))

#drug in DIP
prevalence_estimates_drugDIP <- readRDS(here("shiny", "data","prevalence_estimates_drugsDIP.rds"))
prevalence_attrition_drugDIP <- readRDS(here("shiny", "data","prevalence_attrition_drugsDIP.rds"))
incidence_estimates_drugDIP <- readRDS(here("shiny", "data","incidence_estimates_drugsDIP.rds"))
incidence_attrition_drugDIP <- readRDS(here("shiny", "data","incidence_attrition_drugsDIP.rds"))

#drug in VP
prevalence_estimates_drugVP <- readRDS(here("shiny", "data","prevalence_estimates_drugsVP.rds"))
prevalence_attrition_drugVP <- readRDS(here("shiny", "data","prevalence_attrition_drugsVP.rds"))
incidence_estimates_drugVP <- readRDS(here("shiny", "data","incidence_estimates_drugsVP.rds"))
incidence_attrition_drugVP <- readRDS(here("shiny", "data","incidence_attrition_drugsVP.rds"))

#drug in Parkinsonism
prevalence_estimates_drugParkinsonism <- readRDS(here("shiny", "data","prevalence_estimates_drugsParkinsonism.rds"))
prevalence_attrition_drugParkinsonism <- readRDS(here("shiny", "data","prevalence_attrition_drugsParkinsonism.rds"))
incidence_estimates_drugParkinsonism <- readRDS(here("shiny", "data","incidence_estimates_drugsParkinsonism.rds"))
incidence_attrition_drugParkinsonism <- readRDS(here("shiny", "data","incidence_attrition_drugsParkinsonism.rds"))