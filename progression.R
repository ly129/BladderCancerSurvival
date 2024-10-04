setwd("~/Library/CloudStorage/Box-Box/RWD/BladderCancerSurvival/code/")

progression <- read.csv("../Bladder/Enhanced_AdvUrothelial_Progression.csv")
dim(progression)
head(progression)
# has a variable LastClinicNoteDate that may be useful.
# progression is not in the FlatIron data table descriptions



# only dates for next-gen sequencing 3901 pts
ngs <- read.csv("../Bladder/Enhanced_AdvUrothelialNGS.csv")

enhancedcohort <- read.csv("../Bladder/EnhancedCohort.csv")

# lab <- read.csv("../Bladder/Lab.csv")

