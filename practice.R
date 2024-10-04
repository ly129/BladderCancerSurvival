setwd("~/Library/CloudStorage/Box-Box/RWD/BladderCancerSurvival/BladderCancerSurvival/")

practice <- read.csv("../Bladder/Practice.csv")

# more than one per patient
dim(practice)

# no dates -- define PracticeType as ever academic
table(practice$PracticeType)


