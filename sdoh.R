setwd("~/Library/CloudStorage/Box-Box/RWD/BladderCancerSurvival/BladderCancerSurvival/")

sdoh <- read.csv("../Bladder/SocialDeterminantsOfHealth.csv")
# very clean data, one patient per record

sdoh$SESIndex2015_2019 <- ifelse(sdoh$SESIndex2015_2019 == "", NA, sdoh$SESIndex2015_2019)

sdoh$SESIndex2015_2019 <- as.factor(sdoh$SESIndex2015_2019)
