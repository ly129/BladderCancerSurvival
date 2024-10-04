setwd("~/Library/CloudStorage/Box-Box/RWD/BladderCancerSurvival/BladderCancerSurvival/")

diagnosis <- read.csv("../Bladder/Diagnosis.csv")

diagnosis$DiagnosisDate <- as.Date(diagnosis$DiagnosisDate)

# # first use ICD10 codes to compute comorbidity
# icd10 <- subset(diagnosis, DiagnosisCodeSystem == "ICD-10-CM")
# 
# icd10 <- icd10[, c("PatientID", "DiagnosisCode")]
# icd10 <- icd10[complete.cases(icd10), ]
# 
# icd10$DiagnosisCode <- gsub("\\.", "", icd10$DiagnosisCode)
# 
# elix <- comorbidity(icd10,
#                     "PatientID",
#                     "DiagnosisCode",
#                     map = "elixhauser_icd10_quan",
#                     assign0 = TRUE)
# elix.score.10 <- score(elix, assign0 = TRUE)
# 
# elix.score.10 <- data.frame(PatientID = elix$PatientID, Elixhauser = elix.score.10)
# 
# # then use ICD9 for chose with missingness
# icd9 <- subset(diagnosis, DiagnosisCodeSystem == "ICD-9-CM")
# 
# icd9 <- icd9[, c("PatientID", "DiagnosisCode")]
# icd9 <- icd9[complete.cases(icd9), ]
# 
# icd9$DiagnosisCode <- gsub("\\.", "", icd9$DiagnosisCode)
# 
# elix <- comorbidity(icd9,
#                     "PatientID",
#                     "DiagnosisCode",
#                     map = "elixhauser_icd9_quan",
#                     assign0 = TRUE)
# elix.score.9 <- score(elix, assign0 = TRUE)
# 
# elix.score.9 <- data.frame(PatientID = elix$PatientID, Elixhauser = elix.score.9)
# 
# use9 <- setdiff(elix.score.9$PatientID, elix.score.10$PatientID)
# 
# elix.score <- rbind(elix.score.10,
#                     subset(elix.score.9, PatientID %in% use9))
