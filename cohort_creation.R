################################################################################
# Cohort Creation
################################################################################

# 1. Build around Enhanced_AdvUrothelial.csv, which includes advanced UC patients

setwd("~/Library/CloudStorage/Box-Box/RWD/BladderCancerSurvival/code/")

eauc <- read.csv("../Bladder/Enhanced_AdvUrothelial.csv")

# Enhanced_advUrothelial is one line per patient
## 12681 patients
nrow(eauc) == length(unique(eauc$PatientID))

# Diagnosis of AdvUC on or after January 1, 2011
## change DiagnosisDate to date format
## DiagnosisDate: date of initial diagnosis
eauc$DiagnosisDate <- as.Date(eauc$DiagnosisDate)
eauc$Surgery <- as.logical(eauc$Surgery)
summary(eauc$DiagnosisDate)
## AdvancedDiagnosisDate: date of diagnosis of advanced disease
### inconsistent with GroupStage
eauc$AdvancedDiagnosisDate <- as.Date(eauc$AdvancedDiagnosisDate)
summary(eauc$AdvancedDiagnosisDate)

# 2. Diagnosed after 2011-01-01, before 2021-12-01
## everyone has advanced diagnosis after 20110101 - granted data starts 20110101
min(eauc$AdvancedDiagnosisDate)
max(eauc$AdvancedDiagnosisDate) # 2023-06-27

ptid <- subset(eauc, subset = AdvancedDiagnosisDate >= as.Date("2011-01-01") & 
                              AdvancedDiagnosisDate <= as.Date("2021-12-01"))$PatientID
length(ptid)
# down to 11276

# # Maybe this is wrong. eauc contains advanced UC, so everyone is advanced.
# # Diagnosis of primary UC with distant metastasis defined as stage IV.
# ## GroupStage: group stage at initial diagnosis
# ### check table(eauc$GroupStage, eauc$MStage), pretty much missing are not Stage IV
# table(eauc$GroupStage)
# eauc <- subset(eauc,
#                subset = GroupStage %in% c("Stage IVA", "Stage IVB", "Stage IV"))
# 
# ptid <- eauc$PatientID
# 
# length(ptid)
# # 4020 after stage IV diagnosed after 2011-01-01.

###########################################################################
# Do not exclude concurrent primary cancer following discussion with Teja
###########################################################################
# # 3. exclude other concurrent primary cancer
# # Advanced UC (ICD-10-C67.x or C67.9, C65x, C66x, C67x, C68.0)
# # Any other concurrent primary cancers (ICD-10-Cx, except C34 or C39.9)
# # Diagnosis contains multiple diagnoses per patient
# 
# diagnosis <- read.csv("../Bladder/Diagnosis.csv")
# diagnosis$DiagnosisDate <- as.Date(diagnosis$DiagnosisDate)
# 
# icd10.all <- unique(diagnosis$DiagnosisCode)
# icd10.cancer <- icd10.all[regexpr("C", icd10.all) == 1]
# icd10.uc <- c(icd10.cancer[grep("C67", icd10.cancer)],
#               icd10.cancer[grep("C65", icd10.cancer)],
#               icd10.cancer[grep("C66", icd10.cancer)],
#               "C68.0")
# icd10.other <- setdiff(icd10.cancer, icd10.uc)
# icd10.exception <- c(icd10.other[grep("C34", icd10.other)],
#                      "C39.9")
# icd10.other <- sort(setdiff(icd10.other, icd10.exception))
# # 5688 with other cancer
# ptid.other <- unique(subset(diagnosis,
#                             DiagnosisCode %in% icd10.other)$PatientID)
# 
# ptid <- setdiff(ptid, ptid.other)
# # 6395 after removing 1777 other concurrent primary cancer
# length(ptid)

# 3. exclude patients who have received clinical study drug
source("lineoftherapy.R")
# line of therapy also starts 2011-01
min(lineoftherapy$StartDate)

# total treatment types -- 541
unique.therapy <- unique(lineoftherapy$LineName)
(length(unique.therapy))

# remove Patients receiving therapy in the context of a clinical study
## Clinical Study Drug
csd <- unique.therapy[grep("Clinical", unique.therapy)]
ptid.csd <- unique(subset(lineoftherapy, LineName %in% csd)$PatientID)

ptid <- setdiff(ptid, ptid.csd)

# No more Clinical Study Drug
# table(subset(lineoftherapy, PatientID %in% ptid)$LineName)

# 10742 after removing patients who have taken clinical study drug
length(ptid)

# 4. exclude untreated patients
ptid.treated <- unique(lineoftherapy$PatientID)
ptid <- intersect(ptid, ptid.treated)

# 7429 after removing untreated patients
length(ptid)


# # 5. exclude patients with less than two years of followup
# max(eauc$AdvancedDiagnosisDate)
# 
# fu2yr <- subset(eauc, subset = AdvancedDiagnosisDate <= as.Date("2021-06-30"))$PatientID
# 
# ptid <- intersect(ptid, fu2yr)
# # 5995 have >= 2 years of followup

# source("demographics.R")
# ptid.missgender <- demographics$PatientID[which(is.na(demographics$Gender))]
# ptid <- setdiff(ptid, ptid.missgender)


# Find censoring date and event date (death)
# Define censoring date as the latest of Visit, Enhanced_AdvUrothelial_Orals and DrugEpisode
# and AdvancedDiagnosisDate itself
source("oral.R")
source("visit.R")
source("drugepisode.R")
source("mortality.R")

dd <- subset(eauc, PatientID %in% ptid)

dd <- merge(dd, mortality, all.x = TRUE)

dd$Death <- !is.na(dd$DateOfDeath)

pt.censored <- dd$PatientID[!dd$Death]

for (pp in pt.censored) {
  date.oral <- oral$StartDate[oral$PatientID == pp]
  date.visit <- visit$VisitDate[visit$PatientID == pp]
  date.episode <- drugepisode$EpisodeDate[drugepisode$PatientID == pp]
  date.advdiagnosis <- dd$AdvancedDiagnosisDate[dd$PatientID == pp]
  censored.date <- max(c(date.oral, date.visit, date.episode, date.advdiagnosis))
  dd$DateOfDeath[dd$PatientID == pp] <- censored.date
}

names(dd)[ncol(dd) - 1] <- "EventDate"

###############################################################
# crude analyses show no improvements in survival
###############################################################

dd$DiagnosisPeriod <- ifelse(dd$AdvancedDiagnosisDate < as.Date("2016-05-18"), "Before ICI", ifelse(dd$AdvancedDiagnosisDate > as.Date("2019-12-18"), "After ADC", "Between ICI & ADC"))

dd$DiagnosisPeriod <- factor(dd$DiagnosisPeriod, levels = c("Before ICI", "Between ICI & ADC", "After ADC"))

# merge other information
source("demographics.R")
demo <- subset(demographics, select = c("PatientID", "BirthYear", "Gender", "Race", "Location"))

dd <- merge(dd, demo, all.x = TRUE)

dd$AdvancedDiagnosisYear <- as.integer(format(dd$AdvancedDiagnosisDate,"%Y"))

dd$AgeAtDiagnosis <- dd$AdvancedDiagnosisYear - dd$BirthYear
summary(dd$AgeAtDiagnosis)

dd$Gender <- factor(dd$Gender)
dd$Race <- factor(dd$Race)
dd$Location <- factor(dd$Location)

dd$StageAtDiagnosis <- "Unknown/not documented"
for (i in 1:nrow(dd)) {
  if (dd$GroupStage[i] %in% c("Stage 0a", "Stage 0is")) dd$StageAtDiagnosis[i] <- "Stage0"
  if (dd$GroupStage[i] %in% c("Stage I")) dd$StageAtDiagnosis[i] <- "Stage1"
  if (dd$GroupStage[i] %in% c("Stage II")) dd$StageAtDiagnosis[i] <- "Stage2"
  if (dd$GroupStage[i] %in% c("Stage III", "Stage IIIA", "Stage IIIB")) dd$StageAtDiagnosis[i] <- "Stage3"
  if (dd$GroupStage[i] %in% c("Stage IV", "Stage IVA", "Stage IVB")) dd$StageAtDiagnosis[i] <- "Stage4"
}
dd$StageAtDiagnosis <- factor(dd$StageAtDiagnosis)

dd$SmokingStatus <- factor(dd$SmokingStatus)

# practice type -- ever treated in academic
source("practice.R")
prac.type <- character(length(unique(practice$PatientID)))

prac <- data.frame(PatientID = unique(practice$PatientID),
                   PracticeType = prac.type)

for (i in 1:length(unique(practice$PatientID))) {
  id.tmp <- prac$PatientID[i]
  types <- subset(practice, PatientID == id.tmp)$PracticeType
  if (any(types == "ACADEMIC")) {
    prac$PracticeType[i] <- "ACEDEMIC"
  } else {
    prac$PracticeType[i] <- "COMMUNITY"
  }
}

dd <- merge(dd, prac, all.x = TRUE)
dd$PracticeType <- factor(dd$PracticeType)


# source("drugepisode.R")
dd$Chemotherapy <- dd$Immunotherapy <- dd$AntibodyConjugate <- dd$OtherDrug <- FALSE
dd$DrugTreatment <- TRUE

# should probably consider therapies received after index date, i.e. AdvancedDiagnosisDate
for (i in 1:nrow(dd)) {
  idtmp <- dd$PatientID[i]
  drugname.tmp <- subset(drugepisode, PatientID == idtmp)$DrugName
  drugcategory.tmp <- subset(drugepisode, PatientID == idtmp)$DetailedDrugCategory
  
  if (any(drugcategory.tmp == "chemotherapy")) {
    dd$Chemotherapy[i] <- TRUE
  }
  
  if (any(drugname.tmp %in% c("pembrolizumab", "avelumab", "atezolizumab", "nivolumab"))) {
    dd$Immunotherapy[i] <- TRUE
  }
  
  if (any(drugname.tmp %in% c("enfortumab vedotin-ejfv", "sacituzumab govitecan-hziy"))) {
    dd$AntibodyConjugate[i] <- TRUE
  }
}

# sdoh
source("sdoh.R")
dd <- merge(dd, sdoh, all.x = TRUE)

# biomarker (fgfr, pdl1 mutation)
source("biomarker.R")
bm <- subset(biomarker, PatientID %in% dd$PatientID)
bm <- subset(bm, !BiomarkerStatus %in% c("No interpretation given in report",
                                         "Unknown",
                                         "Unsuccessful/indeterminate test",
                                         "Results pending",
                                         "PD-L1 equivocal"))
pdl1 <- subset(bm, BiomarkerName == "PDL1")
fgfr <- subset(bm, BiomarkerName == "FGFR")

pdl1$PDL1 <- ifelse(pdl1$BiomarkerStatus == "PD-L1 positive", TRUE, FALSE)
fgfr$FGFR <- ifelse(fgfr$BiomarkerStatus == "Positive", TRUE, FALSE)

## multiple records for some patients
## if there are both positive and negative, set to positive
## interpretation: ever positive during followup
pdl1.unique <- unique(pdl1$PatientID)
for (pt in pdl1.unique) {
  pdl1.tmp <- subset(pdl1, PatientID == pt)
  results <- pdl1.tmp$PDL1
  if (length(unique(results)) != 1) pdl1[pdl1$PatientID == pt, "PDL1"] <- TRUE
}
pdl1 <- pdl1[, c("PatientID", "PDL1")]

fgfr.unique <- unique(fgfr$PatientID)
for (pt in fgfr.unique) {
  fgfr.tmp <- subset(fgfr, PatientID == pt)
  results <- fgfr.tmp$FGFR
  if (length(unique(results)) != 1) fgfr[fgfr$PatientID == pt, "FGFR"] <- TRUE
}
fgfr <- fgfr[, c("PatientID", "FGFR")]

pdl1 <- pdl1[!duplicated(pdl1), ]
fgfr <- fgfr[!duplicated(fgfr), ]

dd <- merge(dd, pdl1, all.x = TRUE)
dd <- merge(dd, fgfr, all.x = TRUE)

# insurance


# Elixhauser comorbidity
source("diagnosis.R")

# use baseline comorbidity (before advanced diagnosis date) and remove urothelial cancer
dx <- subset(diagnosis, DiagnosisCodeSystem == "ICD-10-CM")
dx <- subset(dx, PatientID %in% ptid)
dx <- subset(dx, !is.na(dx$DiagnosisDate))

icd10.all <- unique(dx$DiagnosisCode)
icd10.cancer <- icd10.all[regexpr("C", icd10.all) == 1]
icd10.uc <- c(icd10.cancer[grep("C67", icd10.cancer)],
              icd10.cancer[grep("C65", icd10.cancer)],
              icd10.cancer[grep("C66", icd10.cancer)],
              "C68.0")
dx <- subset(dx, !DiagnosisCode %in% icd10.uc)

dx.before.index <- logical(nrow(dx))
for (i in 1:nrow(dx)) {
  row.tmp <- dx[i, ]
  adv.dx.date <- subset(dd, PatientID == row.tmp$PatientID)$AdvancedDiagnosisDate
  if (row.tmp$DiagnosisDate < adv.dx.date) {
    dx.before.index[i] <- TRUE
  } else {
    dx.before.index[i] <- FALSE
  }
}
dx <- dx[dx.before.index, ]
  
library(comorbidity)

icd10 <- dx[, c("PatientID", "DiagnosisCode")]
icd10 <- icd10[complete.cases(icd10), ]

icd10$DiagnosisCode <- gsub("\\.", "", icd10$DiagnosisCode)

elix <- comorbidity(icd10,
                    "PatientID",
                    "DiagnosisCode",
                    map = "elixhauser_icd10_quan",
                    assign0 = TRUE)
elix.score <- score(elix, assign0 = TRUE)
elix.score <- data.frame(PatientID = elix$PatientID, Elixhauser = elix.score)

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


dd <- merge(dd, elix.score, all.x = TRUE)
# end of comorbidity

# add lines of treatment received
lot <- subset(lineoftherapy, PatientID %in% ptid)
no.lines <- aggregate(LineNumber ~ PatientID, FUN = max, data = lot)
dd <- merge(dd, no.lines, all.x = TRUE)

# add ecog at diagnosis
source("ecog.R")
library(dplyr)
# multiple ecog value on the same day for some patients
ecog.unique <- aggregate(EcogValue ~ PatientID + EcogDate, data = ecog, FUN = max)

dd <- dd |>
  left_join(ecog.unique, join_by(PatientID == PatientID, closest(AdvancedDiagnosisDate > EcogDate))) # %>%
  # Filter the rows where the diagnosis date is within the insurance coverage period
  # filter(EcogDate < AdvancedDiagnosisDate)

# add insurance at diagnosis
source("insurance.R")
# multiple insurance on the same day for some patients
insurance$PayerCategory <- factor(insurance$PayerCategory, levels = names(sort(table(insurance$PayerCategory), decreasing = FALSE)))
select.insurance <- function(x) {
  if (length(x) > 1) {
    sort(x)[1]
  } else {
    x
  }
}
insurance.unique <- aggregate(PayerCategory ~ PatientID + StartDate, data = insurance, FUN = select.insurance)

dd <- dd |>
  left_join(select(insurance.unique, c(PatientID, StartDate, PayerCategory)), join_by(PatientID == PatientID, closest(AdvancedDiagnosisDate > StartDate)))

# table 1
library(table1)

pvalue <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    p <- summary(aov(y ~ g))[[1]]$`Pr(>F)`[1]
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}

table1(~ AgeAtDiagnosis + Gender + Race + SmokingStatus + Location + SESIndex2015_2019 + PracticeType + PrimarySite + StageAtDiagnosis + LineNumber + Surgery + PDL1 + FGFR + Elixhauser + EcogValue + PayerCategory| DiagnosisPeriod,
       # data = subset(dd, DiagnosisPeriod %in% c("Before ICI", "After ADC")),
       data = dd,
       overall = FALSE,
       extra.col = list(`P-value` = pvalue))

# fewer has surgery
# more likely to be diagnosed during earlier stages
# higher elixhauser comorbidity score
# # this is suspicious
# more likely to have PDL1 FGFR mutation checked


table(dd$Immunotherapy, dd$StageAtDiagnosis)
table(dd$AntibodyConjugate, dd$StageAtDiagnosis)


table(dd$Immunotherapy, dd$AdvancedDiagnosisYear)
table(dd$AntibodyConjugate, dd$AdvancedDiagnosisYear)





prop.table(table(dd$Immunotherapy, dd$StageAtDiagnosis), margin = 1)


# survival of advanced diagnosis 





