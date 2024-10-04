setwd("~/Library/CloudStorage/Box-Box/RWD/BladderCancerSurvival/BladderCancerSurvival/")

# visit date, type, ...
# 550K + records
visit <- read.csv("../Bladder/visit.csv")
visit$VisitDate <- as.Date(visit$VisitDate)

head(visit)