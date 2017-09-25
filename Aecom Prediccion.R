library(plotly)
library(dplyr)
library(lubridate)
library(googleVis)
library(magrittr)
setwd("C:/Users/johnalva/Downloads")
ae <- read.csv("incident (8).csv")
ae$resolved_at <- mdy_hms(ae$resolved_at)

aed <- ae
aed <- mutate(aed, Days_resolved = day(aed$resolved_at))
aed <- mutate(aed, Month_resolved = month(aed$resolved_at))
aed$calendar_stc <- as.numeric(aed$calendar_stc)
aed$calendar_stc <- aed$calendar_stc/3600

p1 <- filter(aed, priority == "P1 - Critical")
p1_correlation <- select(aed, assignment_group, calendar_stc)

names(p1_correlation)

p1_cor <- p1_correlation %>%
    group_by(assignment_group)


p1_cor_summarise <- p1_cor %>%
    summarise(
        calendar_stc = mean(calendar_stc)
    )

names(p1)
plot(lm(Days_resolved ~ calendar_stc, data = p1, xlab = "Month days", ylab = "Duration Time"))

summary(p1)
p1_fixed <- select(p1,Days_resolved, calendar_stc)
p1_fixed$calendar_stc <- as.numeric(p1_fixed$calendar_stc)
boxplot(p1_fixed$calendar_stc)
summary(p1_fixed)
plot(p1_fixed)

regaecomP1 <- lm(calendar_stc ~ Days_resolved, data = p1_fixed)
plot(calendar_stc ~ Days_resolved, data = p1)
anova(regaecomP1)
abline(regaecomP1)



new_resolutions <- data.frame(Days_resolved = seq(32,50))
predict(regaecomP1, new_resolutions)
predict(regaecomP1, data.frame(new_resolutions), level = 0.95, interval = "confidence")


confint(regaecomP1, level = 0.95)
confint(regaecomP1, level = 0.99)

# Intervalos de confianza de prediccion
new_resolutions <- data.frame(Days_resolved = seq(1,31))
ic <- predict(regaecomP1, new_resolutions, interval = "confidence")
ic

lines(new_resolutions$Days_resolved, ic[,2], lty = 5, col ="blue")
lines(new_resolutions$Days_resolved, ic[,3], lty = 5, col ="blue")

ic <- predict(regaecomP1, new_resolutions, interval = "prediction")
lines(new_resolutions$Days_resolved, ic[,2], lty = 5, col ="red")
lines(new_resolutions$Days_resolved, ic[,3], lty = 5, col ="red")




# Agrupamiento ------------------------------------------------------------

aedCount <- aed %>%
    group_by(Days_resolved, priority) %>%
    mutate(count1 = sum(n()))

aesum <- aedCount %>%
    summarise(
        n = n()
    )

aedCountPriAssi <- aed %>%
    group_by(priority, assignment_group) %>%
    mutate(count1 = sum(n()))

aesumPriAssi <- aedCountPriAssi %>%
    summarise(n = n())


library(shiny)
runGist('9289cfacf40d3951842cc6d62d2371ea')

