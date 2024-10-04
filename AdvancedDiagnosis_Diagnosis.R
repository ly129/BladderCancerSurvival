setwd("~/Library/CloudStorage/Box-Box/RWD/BladderCancerSurvival/BladderCancerSurvival/")

# Diagnosis contains multiple diagnoses per patient
diagnosis <- read.csv("../Bladder/Diagnosis.csv")
# Enhanced_advUrothelial is one line per patient
eauc <- read.csv("../Bladder/Enhanced_AdvUrothelial.csv")

# Diagnosis of AdvUC on or after January 1, 2011 to December 1, 2021
# Diagnosis of primary UC with distant metastasis defined as stage IV.

# change (Advanced)DiagnosisDate to date format
eauc$DiagnosisDate <- as.Date(eauc$DiagnosisDate)
eauc$AdvancedDiagnosisDate <- as.Date(eauc$AdvancedDiagnosisDate)
summary(eauc$DiagnosisDate); summary(eauc$AdvancedDiagnosisDate)


# check stages -- GroupStage ~50% missing
eauc$GroupStage <- as.factor(eauc$GroupStage)
summary(eauc$GroupStage)

eauc$TStage <- as.factor(eauc$TStage)
summary(eauc$TStage)

eauc$NStage <- as.factor(eauc$NStage)
summary(eauc$NStage)

eauc$MStage <- as.factor(eauc$MStage)
summary(eauc$MStage)

summary(diagnosis)
# no cancer stage
# only used exclude patients with other cancers
