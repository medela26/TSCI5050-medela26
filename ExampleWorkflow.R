#'---
#' title: "TSCI 5050: Introduction to Data Science"
#' author: 'AJ Medel ^1^'
#' abstract: |
#'  | Read source data and process it
#' documentclass: article
#' description: 'Manuscript'
#' clean: false
#' self_contained: true
#' number_sections: false
#' keep_md: true
#' fig_caption: true
#' output:
#'  html_document:
#'    toc: true
#'    toc_float: true
#'    code_folding: show
#' ---
#'
#+ init, echo=FALSE, message=FALSE, warning=FALSE
# init ----
# This part does not show up in your rendered report, only in the script,
# because we are using regular comments instead of #' comments
debug <- 0;
seed <- 130409
knitr::opts_chunk$set(echo=debug>-1, warning=debug>0, message=debug>0, class.output="scroll-20", attr.output='style="max-height: 150px; overflow-y: auto;"');

library(ggplot2); # visualisation
library(GGally);
library(rio);# simple command for importing and exporting
library(pander); # format tables
#library(printr); # set limit on number of lines printed
library(broom); # allows to give clean dataset
library(dplyr); #add dplyr library
library(constellation); #simulated patient data
library(tidymodels)
library(skimr)
library(tidyr)


options(max.print=500);
panderOptions('table.split.table',Inf); panderOptions('table.split.cells',Inf);

# state global variables at beginning of script
labs_file <- "labs.csv" 
orders_file <- "orders.xlsx"
vitals_file <- "vitals.tsv"

#export data sets to file only relevant for example data from R; real data already on computer or server
export(constellation::labs, labs_file)
export(constellation::vitals, vitals_file)
export(constellation::orders, orders_file)



# Obtain Data ----
labs <- import(labs_file) %>% mutate(PAT_ID=paste0('x',PAT_ID))
vitals <- import(vitals_file) %>% mutate(PAT_ID=paste0('x',PAT_ID))
orders <- import(orders_file) %>% mutate(PAT_ID=paste0('x',PAT_ID))

#Explore Data ----
#' # Explore Data
#' 
#' ## Labs
#' 
#' Summary
summary(labs) %>% pander()
#' Data Type
sapply(labs,class) %>%pander()
#' Patient Count
labs$PAT_ID %>% unique() %>% length()

#' ## Orders
#' 
#' Summary
summary(orders) %>%pander()
#' Data Type
sapply(orders,class) %>%pander()
#' Patient Count
orders$PAT_ID %>% unique() %>% length()

#' ## Vitals
#' 
#' Summary
summary(vitals) %>%pander()
#' Data Type
sapply(vitals,class) %>%pander()
#' Patient Count
vitals$PAT_ID %>% unique() %>% length()


# Intersect and Set Diff Commands----
#' comparing differences between dataframes. If they're zero, we can rely on "vitals" as source of truth. We want results to be zero. 
setdiff(labs$PAT_ID,vitals$PAT_ID)
setdiff(orders$PAT_ID,vitals$PAT_ID)
setdiff(labs$PAT_ID,orders$PAT_ID)
setdiff(orders$PAT_ID,labs$PAT_ID)

#' ## How many types of labs and vitals there are
#' Labs
labs$VARIABLE %>% table %>% pander
#' Vitals
vitals$VARIABLE %>% table %>% pander

#' Optional but highly recommended assignment for next session: Figure out (with an LLM's help wherever necessary, upload your script if you do ask it for help):
 
"How do I convert the vitals and the labs data frames to ones that have a granularity of patient and day, with separate columns for each type of variables in those respective data frames?"

#' # Create Analytic Data Set
#'
set.seed(seed)
D0 <- bind_rows(labs,vitals) %>% 
  mutate(RECORDED_TIME = as.Date(RECORDED_TIME)) %>% 
  pivot_wider(names_from = VARIABLE,values_from = VALUE,values_fn = median) %>%
  group_by(PAT_ID) %>% 
  group_initial_split(prop = 0.75,group = PAT_ID)

D1 <- training(D0)
nrow(D0); nrow(D1)
skim(D1)

D2 <- group_by(D1,PAT_ID) %>% arrange(RECORDED_TIME) %>% mutate(diff = c(NA,diff(RECORDED_TIME)))
ggplot(D2,aes(y=diff))+geom_histogram()

## To do: convert dates into number of days since that patient's first day


c()
