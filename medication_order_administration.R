setwd("~/Library/CloudStorage/Box-Box/RWD/BladderCancerSurvival/BladderCancerSurvival/")

# Diagnosis contains multiple diagnoses per patient
medorder <- read.csv("../Bladder/MedicationOrder.csv")
medadmin <- read.csv("../Bladder/MedicationAdministration.csv")

# also includes (Detailed)DrugCategory as in DrugEpisode, but is for all drugs for all diseases. No need to use these datasets to determine UC treatment category.

# Can be used to find out what ever treatments the patients are receiving.

dim(medorder)
dim(medadmin)
