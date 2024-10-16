setwd("~/Library/CloudStorage/Box-Box/RWD/BladderCancerSurvival/BladderCancerSurvival/")

demographics <- read.csv("../Bladder/Demographics.csv")
table(demographics$State)
dim(demographics)

# one line per patient
# a lot of missing state

summary(demographics$BirthYear)
table(demographics$Gender)
table(demographics$Ethnicity)
table(demographics$Race)
table(demographics$State)

for (i in 1:nrow(demographics)) {
  if (demographics$State[i] == "") {
    demographics$Location[i] <- NA
  } else if (demographics$State[i] %in% c('CT','MA','ME','NH','RI','VT','NY','NJ','PA')) {
    demographics$Location[i] <- "Northeast"
  } else if (demographics$State[i] %in% c('DC','DE','FL','GA','MD','NC','SC','VA','WV','AL','KY','MS','TN','AR','OK','LA','TX')) {
    demographics$Location[i] <- "South"
  } else if (demographics$State[i] %in% c('IA','KS','MN','MO','ND','NE','SD','IL','IN','MI','OH','WI')) {
    demographics$Location[i] <- "Midwest"
  } else if (demographics$State[i] %in% c('AZ','CO','ID','MT','NM','NV','UT','WY','AK','CA','HI','OR','WA')) {
    demographics$Location[i] <- "West"
  }
  
  if (demographics$Race[i] == "") {
    demographics$Race[i] <- NA
  } else if (demographics$Race[i] %in% c("Hispanic or Latino", "Asian")) {
    demographics$Race[i] <- "Other Race"
  }
  
  if (demographics$Gender[i] == "") {
    demographics$Gender[i] <- NA
  }
  
  if (demographics$Ethnicity[i] == "") {
    demographics$Ethnicity[i] <- NA
  }
}

