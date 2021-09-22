rm(list = ls())

install.packages("tidyverse")
install.packages("mosaic")
install.packages("Hmisc")
install.packages("readxl")

library(Hmisc)
library(tidyverse)
library(mosaic)
library(readxl)

data <- read.csv("C:/Users/Rohan.000/Desktop/1st_Semester/project_on_covid_R/COVID19_line_list_data.csv")
colnames(data)
summary(data)

##Cleaning out inconsistent entries in death Attribute
unique(data$death)
data$death_cleaned <- as.integer(data$death != 0)

##Calculating Death Rate
sum(data$death_cleaned)/nrow(data)

##Analyzing fatality rate vs age

fatal <- subset(data, death_cleaned == 1)
not_fatal <- subset(data, death_cleaned == 0)
mean(fatal$age, na.rm = TRUE)
mean(not_fatal$age, na.rm = TRUE)

##Therefore, the average age of fatal cases is about 20 years higher than not fatal cases.
##Testing the significance of this result
t.test(fatal$age, not_fatal$age, alternative = "two.sided", conf.level = 0.95)
## Since the p-value is very insignificant, we can conclude that fatality is indeed higher at an older age.


##Analysing effect of gender on fatality
maleg <- subset(data, gender == "male")
femaleg <- subset(data, gender == "female")
mean(maleg$death_cleaned, na.rm = TRUE)
mean(femaleg$death_cleaned, na.rm = TRUE)

##Therefore, the average age of fatal cases is about 5% higher for males than females.
##Testing the significance of this result
t.test(maleg$death_cleaned, femaleg$death_cleaned, alternative = "two.sided", conf.level = 0.95)
## Since the p-value is less than 0.05, we can conclude that the results are statistically significant.

