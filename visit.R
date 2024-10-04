setwd("~/Library/CloudStorage/Box-Box/RWD/BladderCancerSurvival/code/")

# visit date, type, ...
# 550K + records
visit <- read.csv("../Bladder/visit.csv")
visit$VisitDate <- as.Date(visit$VisitDate)

head(visit)