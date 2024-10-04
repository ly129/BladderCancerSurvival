setwd("~/Library/CloudStorage/Box-Box/RWD/BladderCancerSurvival/BladderCancerSurvival/")

lineoftherapy <- read.csv("../Bladder/LineOfTherapy.csv")

# total treatment types -- 541
unique.therapy <- unique(lineoftherapy$LineName)
(length(unique.therapy))

# remove Patients receiving therapy in the context of a clinical study
## Clinical Study Drug
csd <- unique.therapy[grep("Clinical", unique.therapy)]
csd.patients <- unique(subset(lineoftherapy, LineName %in% csd)$PatientID)

lineoftherapy <- subset(lineoftherapy, subset = !(PatientID %in% csd.patients))

# remaining patient #
length(unique(lineoftherapy$PatientID))

# which ones include Enfortumab -- 29 therapies
unique.therapy[grep("Enfortumab", unique.therapy)]
# which ones include Sacituzumab -- 9 therapies
unique.therapy[grep("Sacituzumab", unique.therapy)]

treated.by.adcs <- subset(lineoftherapy,
                          subset = LineName %in% c(
                            unique.therapy[grep("Enfortumab", unique.therapy)],
                            unique.therapy[grep("Sacituzumab", unique.therapy)]))
# View(treated.by.adcs)

# check which ADCs can be first line
table(subset(treated.by.adcs,
             subset = LineNumber == 1,
             select = LineName))
























# file.names <- list.files("Data for bladder cancer project")
# 
# 
# dd <- vector(mode = "list", length = length(file.names))
# library(haven)
# for (i in 1:length(file.names)) {
#   dd[[i]] <- read_sas(paste0("Data for bladder cancer project/", file.names[i]), NULL)
# }
# 
# names(dd) <- gsub(".sas7bdat", "", file.names)
# dd <- lapply(dd, as.data.frame)
# 
# # Includion criteria
# 
# # In enhanced_advurothelial: GroupStage == IV