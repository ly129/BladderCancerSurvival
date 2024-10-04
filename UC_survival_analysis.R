################################################################################
# Cohort Creation
################################################################################

# Build around Enhanced_AdvUrothelial.csv, which includes advanced UC patients
# 1. In Enhanced_AdvUrothelial, find pts with GroupStage IV

setwd("~/Library/CloudStorage/Box-Box/RWD/BladderCancerSurvival/BladderCancerSurvival/")

eauc <- read.csv("../Bladder/Enhanced_AdvUrothelial.csv")

# Enhanced_advUrothelial is one line per patient
## 12681 patients
nrow(eauc) == length(unique(eauc$PatientID))

# Diagnosis of AdvUC on or after January 1, 2011
## change DiagnosisDate to date format
## DiagnosisDate: date of initial diagnosis
eauc$DiagnosisDate <- as.Date(eauc$DiagnosisDate)
summary(eauc$DiagnosisDate)
## AdvancedDiagnosisDate: date of diagnosis of advanced disease
### inconsistent with GroupStage
eauc$AdvancedDiagnosisDate <- as.Date(eauc$AdvancedDiagnosisDate)
summary(eauc$AdvancedDiagnosisDate)

# 2. Diagnosed after 2011-01-01
## everyone has advanced diagnosis after 20110101 - granted data starts 20110101
dd <- subset(eauc, subset = AdvancedDiagnosisDate >= as.Date("2011-01-01"))
ptid <- dd$PatientID

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

# 3. exclude other concurrent primary cancer
# Advanced UC (ICD-10-C67.x or C67.9, C65x, C66x, C67x, C68.0)
# Any other concurrent primary cancers (ICD-10-Cx, except C34 or C39.9)
# Diagnosis contains multiple diagnoses per patient

diagnosis <- read.csv("../Bladder/Diagnosis.csv")
diagnosis$DiagnosisDate <- as.Date(diagnosis$DiagnosisDate)

icd10.all <- unique(diagnosis$DiagnosisCode)
icd10.cancer <- icd10.all[regexpr("C", icd10.all) == 1]
icd10.uc <- c(icd10.cancer[grep("C67", icd10.cancer)],
              icd10.cancer[grep("C65", icd10.cancer)],
              icd10.cancer[grep("C66", icd10.cancer)],
              "C68.0")
icd10.other <- setdiff(icd10.cancer, icd10.uc)
icd10.exception <- c(icd10.other[grep("C34", icd10.other)],
                     "C39.9")
icd10.other <- sort(setdiff(icd10.other, icd10.exception))
# 5688 with other cancer
ptid.other <- unique(subset(diagnosis,
                            DiagnosisCode %in% icd10.other)$PatientID)

ptid <- setdiff(ptid, ptid.other)
# 6993 after removing 1777 other concurrent primary cancer
length(ptid)

# 4. exclude patients who have received clinical study drug
lineoftherapy <- read.csv("../Bladder/LineOfTherapy.csv")
lineoftherapy$StartDate <- as.Date(lineoftherapy$StartDate)
lineoftherapy$EndDate <- as.Date(lineoftherapy$EndDate)
lineoftherapy$IsMaintenanceTherapy <- as.logical(lineoftherapy$IsMaintenanceTherapy)
# line of therapy also starts 2011-01
min(lineoftherapy$StartDate)

# total treatment types -- 541
unique.therapy <- unique(lineoftherapy$LineName)
(length(unique.therapy))

# remove Patients receiving therapy in the context of a clinical study
## Clinical Study Drug
csd <- unique.therapy[grep("Clinical", unique.therapy)]
csd.patients <- unique(subset(lineoftherapy, LineName %in% csd)$PatientID)

ptid <- setdiff(ptid, csd.patients)

# No more Clinical Study Drug
# table(subset(lineoftherapy, PatientID %in% ptid)$LineName)

# 6783 after removing patients who have taken clinical study drug
length(ptid)

# # at least two years of followup
# max(eauc$AdvancedDiagnosisDate)
# 
# fu2yr <- subset(eauc, subset = AdvancedDiagnosisDate <= as.Date("2021-06-30"))$PatientID
# 
# ptid <- intersect(ptid, fu2yr)
# # 5995 have >= 2 years of followup



# # check discrepancy between DiagnosisDate and AdvancedDiagnosisDate
# # with all the exclusions, the two for the remaining cohort are exactly the same 
# sum(subset(eauc, PatientID %in% ptid)$DiagnosisDate != 
#       subset(eauc, PatientID %in% ptid)$AdvancedDiagnosisDate)

################################################################################
# Build analysis data
################################################################################

dd <- subset(lineoftherapy, PatientID %in% ptid)

# earlist enfortumab StartDate 2020-01-06
min(subset(dd, grepl("Enfortumab", LineName))$StartDate)

# merge DetailedDrugCategory from DrugEpisode

## use DrugEpisode to find correspondence between LineName and DetailedDrugCategory
drugepisode <- read.csv("../Bladder/DrugEpisode.csv")
drugepisode$LineStartDate <- as.Date(drugepisode$LineStartDate)
drugepisode$LineEndDate <- as.Date(drugepisode$LineEndDate)
drugepisode$EpisodeDate <- as.Date(drugepisode$EpisodeDate)
drugepisode$IsMaintenanceTherapy <- as.logical(drugepisode$IsMaintenanceTherapy)
# remove clinical study drug
drugepisode <- subset(drugepisode, !grepl("Clinical Study Drug", LineName))

# LineName and DetailedDrugCategory do not have 1-to-1 correspondence
# Teja's annotation: solution-fluid has a mix of chemotherapies and immunotherapies
# check
length(unique(drugepisode$DrugName))

drugname_drugcat <- matrix(nrow = length(unique(drugepisode$DrugName)),
                           ncol = 2)
colnames(drugname_drugcat) <- c("DrugName", "DetailedDrugCategory")

i <- 1
for (dn in sort(unique(drugepisode$DrugName))) {
  drugname_drugcat[i, "DrugName"] <- dn
  tmp <- table(subset(drugepisode,
                      subset = DrugName == dn,
                      select = "DetailedDrugCategory"))
  # some chemo and immuno drugs are labeled chemo/immuno and solution-fluid
  if (length(tmp) > 1) {
    names <- attr(tmp, "dimnames")$DetailedDrugCategory
    drugname_drugcat[i, "DetailedDrugCategory"] <- setdiff(names, "solution-fluid")
  } else {
    drugname_drugcat[i, "DetailedDrugCategory"] <- attr(tmp, "dimnames")$DetailedDrugCategory
  }
  i <- i + 1
}

drugcat <- as.data.frame(drugname_drugcat)
chemos <- subset(drugcat, DetailedDrugCategory == "chemotherapy")$DrugName
immunos <- subset(drugcat, DetailedDrugCategory == "immunotherapy")$DrugName
adcs <- subset(drugcat, DetailedDrugCategory == "antibody-conjugate")$DrugName
otherdrugs <- subset(drugcat, !(DetailedDrugCategory %in% c("antibody-conjugate", "immunotherapy", "chemotherapy") ))$DrugName

# breakdown LineName in dd and identify DetailedDrugCategory
# A lot of combination therapies, e.g. chemo + immuno
dd.DrugName <- strsplit(dd$LineName, ",")
dd.DrugName <- lapply(dd.DrugName, tolower)
# find out the one or combo of treatments that each patient in dd has received
chemo <- immuno <- adc <- other <- logical(nrow(dd))

for (i in 1:nrow(dd)) {
  if (sum(dd.DrugName[[i]] %in% chemos) > 0) chemo[i] <- TRUE
  if (sum(dd.DrugName[[i]] %in% immunos) > 0) immuno[i] <- TRUE
  if (sum(dd.DrugName[[i]] %in% adcs) > 0) adc[i] <- TRUE
  if (sum(dd.DrugName[[i]] %in% otherdrugs) > 0) other[i] <- TRUE
}
dd$Chemotherapy <- chemo
dd$Immunotherapy <- immuno
dd$ADC <- adc
dd$OtherDrug <- other

# # use of drugs by diagnosis year extracted from Enhanced_AdvUthelial 
# dd <- merge(dd, eauc[, c("PatientID", "DiagnosisDate")])
# dd$DiagnosisYear <- format(dd$DiagnosisDate, "%Y")
# Use of drugs stratified by start year
dd$StartYear <- format(dd$StartDate, "%Y")

chemoyr <- lapply(split(chemo, dd$StartYear), sum)
immunoyr <- lapply(split(immuno, dd$StartYear), sum)
adcyr <- lapply(split(adc, dd$StartYear), sum)
otheryr <- lapply(split(other, dd$StartYear), sum)

twoplus <- lapply(lapply(split(chemo+immuno+adc+other, dd$StartYear), ">", 1), sum)
untreated <- lapply(lapply(split(chemo+immuno+adc+other, dd$StartYear), "<", 1), sum)


yeartrend <- data.frame(Chemotherapy = unlist(chemoyr),
                        Immunotherapy = unlist(immunoyr),
                        `Antibody Drug Conjugate` = unlist(adcyr),
                        Other = unlist(otheryr))
# plot breakdown of therapies by year

# ggplot(data = yeartrend, aes)
#   geom_line()
# 



# dd <- merge(dd,
#             drugepisode
#             # by = c("PatientID", "LineName", "LineNumber")
#             # # by.y = c("PatientID", "LineName", "LineNu")
# )
