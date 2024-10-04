setwd("~/Library/CloudStorage/Box-Box/RWD/BladderCancerSurvival/code/")

# Records are included for each documented ECOG observation for each patient in the cohort. Records are excluded if they are not a valid ECOG value or if EcogDate is after the data cutoff date or NULL.
ecog <- read.csv("../Bladder/ECOG.csv")
dim(ecog)

# # Contains structured and NLP-extracted ECOG information centered on the baseline window around treatment initiation
# baselineecog <- read.csv("../Bladder/BaselineECOG.csv")
# dim(baselineecog)
# # 16111 records
# # same as LineOfTherapy
# # before each line of therapy, ecog is evaluated
# 
# # one known (structured) or extracted ECOG for each prescription
# table(baselineecog$ECOGSource)
# baselineecog$ECOGDate <- as.Date(baselineecog$ECOGDate)
# baselineecog$LineStartDate <- as.Date(baselineecog$LineStartDate)

# ecog at diagnosis, has to use ecog instead of baselineecog
ecog$EcogDate <- as.Date(ecog$EcogDate)
