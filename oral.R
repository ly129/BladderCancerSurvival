# only 264 oral drugs
# a subset of LineOfTherapy, minor differences exist
oral <- read.csv("../Bladder/Enhanced_AdvUrothelial_Orals.csv")
oral$StartDate <- as.Date(oral$StartDate)

# subset(lineoftherapy, PatientID == oral$PatientID[2])
# # basically only clinical study drug and erdafitinib (FGFR inhibitor) are oral drugs
# table(oral$DrugName)