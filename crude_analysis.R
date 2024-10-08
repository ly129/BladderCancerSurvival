##################
# crude analysis #
##################
setwd("~/Library/CloudStorage/Box-Box/RWD/BladderCancerSurvival/BladderCancerSurvival/")

source("cohort_creation.R")

library(survival)
library(ggplot2)
library(survminer)
surv.dd <- with(dd, Surv(time = as.numeric(EventDate - AdvancedDiagnosisDate),
                         event = Death))

fit1 <- survfit(surv.dd ~ 1)
plot(fit1)

ggsurvplot(fit1,
           data = dd,
           conf.int = TRUE,
           surv.median.line = c("hv"),
           risk.table = TRUE,
           break.time.by = 90,
           xlim = c(0, 1000),
           ggtheme = theme_light(),
           risk.table.y.text.col = TRUE, # colour risk table text annotations.
           risk.table.y.text = FALSE,
           xlab = "Time in Days")

summary(fit1)$table
summary(fit1,time=c(365.25))
summary(fit1,time=c(365.25 * 2))


ggsurvplot(fit = survfit(surv.dd ~ 1), data = dd,
           pval = FALSE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"))

dd$PostICI <- dd$AdvancedDiagnosisDate > as.Date("2016-05-18")
dd$PostADC <- dd$AdvancedDiagnosisDate > as.Date("2019-12-18")


fit2 <- survfit(surv.dd ~ dd$PostICI)

summary(fit2)$table
summary(fit2,time=c(365.25))
summary(fit2,time=c(365.25 * 2))

ggsurvplot(fit = fit2,
           data = dd,
           xlim = c(0, 1000),
           break.time.by = 90,
           pval = TRUE,
           conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"),
           risk.table.y.text = FALSE,
           xlab = "Time in Days")

fit3 <- survfit(surv.dd ~ dd$PostADC)
summary(fit3)$table
summary(fit3,time=c(365.25))
summary(fit3,time=c(365.25 * 2))

ggsurvplot(fit = fit3,
           data = dd,
           xlim = c(0, 1000),
           break.time.by = 90,
           pval = TRUE,
           conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"),
           risk.table.y.text = FALSE,
           xlab = "Time in Days")



# log rank test
survdiff(surv.dd ~ dd$PostICI, rho = 0)
survdiff(surv.dd ~ dd$PostADC, rho = 0)



head(dd)

# 2-year restricted mean survival time
fit.rmst.ICI <- survRM2::rmst2(time = dd$EventDate - dd$AdvancedDiagnosisDate,
                               status = dd$Death,
                               arm = dd$PostICI,
                               tau = 730)
plot(fit.rmst.ICI)
fit.rmst.ICI


fit.rmst.ADC <- survRM2::rmst2(time = dd$EventDate - dd$AdvancedDiagnosisDate,
                               status = dd$Death,
                               arm = dd$PostADC,
                               tau = 730)
plot(fit.rmst.ADC)
fit.rmst.ADC


# 1-year restricted mean survival time
fit.rmst1.ICI <- survRM2::rmst2(time = dd$EventDate - dd$AdvancedDiagnosisDate,
                                status = dd$Death,
                                arm = dd$PostICI,
                                tau = 365)
plot(fit.rmst1.ICI)
fit.rmst1.ICI


fit.rmst1.ADC <- survRM2::rmst2(time = dd$EventDate - dd$AdvancedDiagnosisDate,
                                status = dd$Death,
                                arm = dd$PostADC,
                                tau = 365)
plot(fit.rmst1.ADC)
fit.rmst1.ADC



# utilization
# by year of adv diagnosis and by year of prescription
head(lineoftherapy)

de <- subset(drugepisode, PatientID %in% ptid)
chemodrugnames <- names(table(de[de$DetailedDrugCategory == "chemotherapy", "DrugName"]))
pd1pdl1names <- c("pembrolizumab", "avelumab", "atezolizumab", "nivolumab", "durvalumab")
adcnames <- c("enfortumab vedotin-ejfv", "sacituzumab govitecan-hziy")

de <- merge(de, dd[, c("PatientID", "AdvancedDiagnosisYear")], by = "PatientID", all.x = TRUE)
de$LineStartYear <- as.integer(format(de$LineStartDate,"%Y"))

library(tidyr)
de <- de[, c("PatientID", "LineStartYear", "AdvancedDiagnosisYear", "DrugName")]
de$DrugCat <- ifelse(de$DrugName %in% chemodrugnames,
                     "chemo",
                     ifelse(de$DrugName %in% pd1pdl1names,
                            "immuno",
                            ifelse(de$DrugName %in% adcnames,
                                   "adc", "other")
                     ))
de <- subset(de, select = -DrugName)
de.unique <- unique(de)

de.unique$value <- 1
de.wide <- pivot_wider(de.unique, names_from = DrugCat, values_from = value)

no.rx <- aggregate(PatientID ~ AdvancedDiagnosisYear + LineStartYear,
                   data = de,
                   FUN = function(x) {length(unique(x))})

no.chemo <- aggregate(chemo ~ AdvancedDiagnosisYear + LineStartYear,
                      data = de.wide,
                      FUN = sum, na.rm = TRUE)

no.immuno <- aggregate(immuno ~ AdvancedDiagnosisYear + LineStartYear,
                       data = de.wide,
                       FUN = sum, na.rm = TRUE)

no.adc <- aggregate(adc ~ AdvancedDiagnosisYear + LineStartYear,
                    data = de.wide,
                    FUN = sum, na.rm = TRUE)

no.other <- aggregate(other ~ AdvancedDiagnosisYear + LineStartYear,
                      data = de.wide,
                      FUN = sum, na.rm = TRUE)

no.rx <- merge(no.rx, no.chemo, all = TRUE)
no.rx <- merge(no.rx, no.immuno, all = TRUE)
no.rx <- merge(no.rx, no.adc, all = TRUE)
no.rx <- merge(no.rx, no.other, all = TRUE)

no.rx[is.na(no.rx)] <- 0

no.rx <- subset(no.rx, AdvancedDiagnosisYear <= LineStartYear)

# no.rx <- no.rx[order(no.rx$LineStartYear, decreasing = TRUE), ]

no.rx

# 3D plots
library(latticeExtra)

cloud(chemo/PatientID~AdvancedDiagnosisYear+LineStartYear,
      no.rx,
      panel.3d.cloud=panel.3dbars,
      col.facet='grey',
      xbase=0.4,
      ybase=0.4,
      screen = list(z = 100, x = -60),
      scales=list(arrows=FALSE, col=1),
      par.settings = list(axis.line = list(col = "transparent")))

cloud(immuno/PatientID~AdvancedDiagnosisYear+LineStartYear,
      no.rx,
      panel.3d.cloud=panel.3dbars,
      col.facet='grey80',
      xbase=0.4,
      ybase=0.4,
      scales=list(arrows=FALSE, col=1),
      par.settings = list(axis.line = list(col = "transparent")))

cloud(adc/PatientID~AdvancedDiagnosisYear+LineStartYear,
      no.rx,
      panel.3d.cloud=panel.3dbars,
      col.facet='grey80',
      xbase=0.4,
      ybase=0.4,
      scales=list(arrows=FALSE, col=1),
      par.settings = list(axis.line = list(col = "transparent")))

# 2d plots

