setwd("~/Library/CloudStorage/Box-Box/RWD/BladderCancerSurvival/BladderCancerSurvival/")

# everyone is "BLADDER"
enhancedcohort <- read.csv("../Bladder/EnhancedCohort.csv")

# # very baseic lab tests
# [1] "body weight"                                                  
# [2] "oxygen saturation in arterial blood by pulse oximetry"        
# [3] "body height"                                                  
# [4] "body temperature"                                             
# [5] "diastolic blood pressure"                                     
# [6] "systolic blood pressure"                                      
# [7] "heart rate"                                                   
# [8] "pain severity - 0-10 verbal numeric rating [score] - reported"
# [9] "body mass index (bmi) [ratio]"                                
# [10] "body surface area"                                            
# [11] "respiratory rate"                                             
# [12] "head occipital-frontal circumference by tape measure"         
vitals <- read.csv("../Bladder/vitals.csv")
dim(vitals)
head(vitals)


# only dates for telemed visits
telemed <- read.csv("../Bladder/telemedicine.csv")