setwd("~/Library/CloudStorage/Box-Box/RWD/BladderCancerSurvival/code/")

# Contains a mapping of medication administrations and medication orders to lines of therapy.
# Diagnosis contains multiple diagnoses per patient
# Used DetailedDrugCategory to determine category of treatment
## However, attention needed.. e.g. Enfortumab + Pembrolizumab is categorized as immunotherapy only
drugepisode <- read.csv("../Bladder/DrugEpisode.csv")

drugepisode$LineStartDate <- as.Date(drugepisode$LineStartDate)
drugepisode$LineEndDate <- as.Date(drugepisode$LineEndDate)
drugepisode$EpisodeDate <- as.Date(drugepisode$EpisodeDate)
drugepisode$IsMaintenanceTherapy <- as.logical(drugepisode$IsMaintenanceTherapy)

dim(drugepisode)
head(drugepisode)

# Does not include everyone with UC
# comparing to LineOfTherapy that has 16111 records
# same as lineoftherapy 9003 treated patients.
# meaning 3000+ receiving no treatment at all.
length(unique(drugepisode$PatientID)) # 9003

length(unique(drugepisode$LineName))
table(drugepisode$DetailedDrugCategory)
table(drugepisode$EnhancedCohort) # all bladder


# unique.therapy <- unique(drugepisode$LineName)
# csd <- unique.therapy[grep("Clinical", unique.therapy)]
# # csd.patients <- unique(subset(lineoftherapy, LineName %in% csd)$PatientID)
