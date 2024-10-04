setwd("~/Library/CloudStorage/Box-Box/RWD/BladderCancerSurvival/BladderCancerSurvival/")

insurance <- read.csv("../Bladder/Insurance.csv")

# multiple records per patient
# match with 
dim(insurance)

table(insurance$PayerCategory)

insurance$StartDate <- as.Date(insurance$StartDate)
insurance$EndDate <- as.Date(insurance$EndDate)

# insurance <- insurance[order(insurance$StartDate), ]
# insurance <- insurance[order(insurance$PatientID), ]

# when building clean data, compare start date of insurance and diagnosis/treatment