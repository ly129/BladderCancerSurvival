setwd("~/Library/CloudStorage/Box-Box/RWD/BladderCancerSurvival/BladderCancerSurvival/")


lineoftherapy <- read.csv("../Bladder/LineOfTherapy.csv")

# multiple lines per patient
dim(lineoftherapy)
head(lineoftherapy)

lineoftherapy$StartDate <- as.Date(lineoftherapy$StartDate)
lineoftherapy$EndDate <- as.Date(lineoftherapy$EndDate)
lineoftherapy$IsMaintenanceTherapy <- as.logical(lineoftherapy$IsMaintenanceTherapy)

# What we have here is a subset containing BLADDER cohort
# EnhancedCohort: Disease setting of the therapy
# Should be enough for identifying treatments received by advanced UC patients
# and no need to use MedicationOrder and/or MedicationAdministration
table(lineoftherapy$EnhancedCohort)


# check line number
table(lineoftherapy$LineNumber[grepl("Pembrolizumab", lineoftherapy$LineName)])

# Earliest Pembrolizumab as first line on 2015-09-17
table(lineoftherapy$LineNumber[lineoftherapy$LineName == "Pembrolizumab"])
min(lineoftherapy$StartDate[lineoftherapy$LineName == "Pembrolizumab" & lineoftherapy$LineNumber == 1])


# Earliest Enfortumab as first line on 2020-02-03
table(lineoftherapy$LineNumber[lineoftherapy$LineName == "Enfortumab Vedotin-Ejfv"])
min(lineoftherapy$StartDate[lineoftherapy$LineName == "Enfortumab Vedotin-Ejfv" & lineoftherapy$LineNumber == 1])


# Earliest Enfortumab + Pembrolizumab as first line on 2020-02-28
table(lineoftherapy$LineNumber[lineoftherapy$LineName == "Enfortumab Vedotin-Ejfv,Pembrolizumab"])
min(lineoftherapy$StartDate[lineoftherapy$LineName == "Enfortumab Vedotin-Ejfv,Pembrolizumab" & lineoftherapy$LineNumber == 1])

# # # Should have not done this
# # # DrugEpisode has EnhancedCohort == BLADDER for all so it can be directly used
# # # to decide treatment received for patients.
# 
# # earlist enfortumab StartDate 2020-01-06
# min(subset(lineoftherapy, grepl("Enfortumab", LineName))$StartDate)
# 
# # merge DetailedDrugCategory from DrugEpisode
# # drug <==> drug category (chemo, immuno, etc.)
# 
# ## use DrugEpisode to find correspondence between LineName and DetailedDrugCategory
# source("drugepisode.R")
# 
# # remove clinical study drug
# nocsd <- subset(drugepisode, !grepl("Clinical Study Drug", LineName))
# 
# # LineName and DetailedDrugCategory do not have 1-to-1 correspondence
# # Teja's annotation: solution-fluid has a mix of chemotherapies and immunotherapies
# # check
# length(unique(nocsd$DrugName))
# 
# drugname_drugcat <- matrix(nrow = length(unique(nocsd$DrugName)),
#                            ncol = 2)
# colnames(drugname_drugcat) <- c("DrugName", "DetailedDrugCategory")
# 
# i <- 1
# for (dn in sort(unique(nocsd$DrugName))) {
#   drugname_drugcat[i, "DrugName"] <- dn
#   tmp <- table(subset(nocsd,
#                       subset = DrugName == dn,
#                       select = "DetailedDrugCategory"))
#   # some chemo and immuno drugs are labeled chemo/immuno and solution-fluid
#   if (length(tmp) > 1) {
#     names <- attr(tmp, "dimnames")$DetailedDrugCategory
#     drugname_drugcat[i, "DetailedDrugCategory"] <- setdiff(names, "solution-fluid")
#   } else {
#     drugname_drugcat[i, "DetailedDrugCategory"] <- attr(tmp, "dimnames")$DetailedDrugCategory
#   }
#   i <- i + 1
# }
# 
# drugname_drugcat[drugname_drugcat[,2] == "immunotherapy", ]
# drugname_drugcat[drugname_drugcat[,2] == "antibody-conjugate", ]
# 
# 
# drugcat <- as.data.frame(drugname_drugcat)
# chemos <- subset(drugcat, DetailedDrugCategory == "chemotherapy")$DrugName
# immunos <- subset(drugcat, DetailedDrugCategory == "immunotherapy")$DrugName
# adcs <- subset(drugcat, DetailedDrugCategory == "antibody-conjugate")$DrugName
# otherdrugs <- subset(drugcat, !(DetailedDrugCategory %in% c("antibody-conjugate", "immunotherapy", "chemotherapy") ))$DrugName
# 
# # breakdown LineName in dd and identify DetailedDrugCategory
# # A lot of combination therapies, e.g. chemo + immuno
# DrugName <- strsplit(nocsd$LineName, ",")
# DrugName <- lapply(DrugName, tolower)
# # find out the one or combo of treatments that each patient in dd has received
# chemo <- immuno <- adc <- other <- logical(nrow(nocsd))
# 
# for (i in 1:nrow(dd)) {
#   if (sum(DrugName[[i]] %in% chemos) > 0) chemo[i] <- TRUE
#   if (sum(DrugName[[i]] %in% immunos) > 0) immuno[i] <- TRUE
#   if (sum(DrugName[[i]] %in% adcs) > 0) adc[i] <- TRUE
#   if (sum(DrugName[[i]] %in% otherdrugs) > 0) other[i] <- TRUE
# }
# nocsd$Chemotherapy <- chemo
# nocsd$Immunotherapy <- immuno
# nocsd$ADC <- adc
# nocsd$OtherDrug <- other
