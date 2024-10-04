##################
# crude analysis #
##################
setwd("~/Library/CloudStorage/Box-Box/RWD/BladderCancerSurvival/code/")

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