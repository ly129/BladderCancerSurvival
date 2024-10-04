# iptw to balance covariates in three cohorts

setwd("~/Library/CloudStorage/Box-Box/RWD/BladderCancerSurvival/BladderCancerSurvival/")

source('cohort_creation.R')

head(dd); dim(dd)
summary(dd)

# choose confounders
# good quality
# low missingness
# clinically sound

dd$PrimarySite
dd$SmokingStatus
dd$Surgery
dd$AgeAtDiagnosis
dd$Gender