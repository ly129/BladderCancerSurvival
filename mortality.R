setwd("~/Library/CloudStorage/Box-Box/RWD/BladderCancerSurvival/code/")

# 8767 deaths, with year-month of death
mortality <- read.csv("../Bladder/Enhanced_Mortality_V2.csv")

# only to the month - set to the 28th of each month
mortality$DateOfDeath <- paste0(mortality$DateOfDeath, "-15")

mortality$DateOfDeath <- as.Date(mortality$DateOfDeath)

length(setdiff(eauc$PatientID, mortality$PatientID))

# a subset of eauc pts
any(!mortality$PatientID %in% eauc$PatientID)

# assuming the rest are still alive