setwd("~/Library/CloudStorage/Box-Box/RWD/BladderCancerSurvival/BladderCancerSurvival/")

biomarker <- read.csv("../Bladder/Enhanced_AdvUrothelialBiomarkers.csv")
# eauc <- read.csv("../Bladder/Enhanced_AdvUrothelial.csv")
# # 4018/12681 has biomarkers
# length(intersect(unique(biomarker$PatientID), unique(eauc$PatientID)))

biomarker$SpecimenCollectedDate <- as.Date(biomarker$SpecimenCollectedDate)
biomarker$SpecimenReceivedDate <- as.Date(biomarker$SpecimenReceivedDate)
biomarker$ResultDate <- as.Date(biomarker$ResultDate)

biomarker$Date <- as.Date(apply(biomarker[, c("SpecimenCollectedDate",
                                              "SpecimenReceivedDate",
                                              "ResultDate")],
                                MARGIN = 1,
                                FUN = min,
                                na.rm = TRUE))

# # Ideally, we want mutation status at time of advanced diagnosis.
# ## However what can be done depends on data availability
# 
# pdl1 <- subset(biomarker, BiomarkerName == "PDL1")
# fgfr <- subset(biomarker, BiomarkerName == "FGFR")
# # 1950 patients have both PDL1 and FGFR
# length(intersect(unique(pdl1$PatientID), unique(fgfr$PatientID)))
# 
# pdl1$PDL1 <- ifelse(pdl1$BiomarkerStatus %in% c("PD-L1 positive", "Positive"),
#                     1,
#                     ifelse(pdl1$BiomarkerStatus %in% c("PD-L1 negative/not detected", "Negative"),
#                            0, NA))
# 
# fgfr$FGFR <- ifelse(fgfr$BiomarkerStatus == "Positive",
#                     1,
#                     ifelse(fgfr$BiomarkerStatus == "Negative",
#                            0, NA))
# 
# pdl1 <- pdl1[, c("PatientID", "BiomarkerName", "Date", "PDL1")]
# fgfr <- fgfr[, c("PatientID", "BiomarkerName", "Date", "FGFR")]
# # pdl1$FGFR <- NA
# # fgfr$PDL1 <- NA
# 
# # remove duplicated rows
# pdl1 <- pdl1[!duplicated(pdl1), ]
# fgfr <- fgfr[!duplicated(fgfr), ]
# 
# # merge
# biomarker <- merge(pdl1, fgfr, by = "PatientID", all = TRUE)
# 
# # biomarker <- subset(biomarker, PatientID %in% ptid)
