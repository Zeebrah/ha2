setwd("/Users/ivanloktaev/Yandex.Disk.localized/Uni/4\ Курс/Data\ Science/HWs/ha2\ materials/")
library(haven)
data_1 <- read_dta("data_1.dta")
# What is done above is initial setup 

#1
# Create two additional dummy variables unem74, unem75
# Create new variables and then cbind them to the data_1 date.frame
# Add new variables to the data.frame
data_1 <- cbind(data_1,unem74 = data_1$re74 == 0,unem75 = data_1$re75 == 0)

# Running univariate regressions
reg_age <- lm(data_1$treat ~ data_1$age)
reg_edu <- lm(data_1$treat ~ data_1$education)
reg_black <- lm(data_1$treat ~ data_1$black)
reg_hisp <- lm(data_1$treat ~ data_1$hispanic)
reg_married <- lm(data_1$treat ~ data_1$married)
reg_nodeg <- lm(data_1$treat ~ data_1$nodegree)
reg_re74 <- lm(data_1$treat ~ data_1$re74)
reg_re75 <- lm(data_1$treat ~ data_1$re75)
reg_unem74 <- lm(data_1$treat ~ data_1$unem74)
reg_unem75 <- lm(data_1$treat ~ data_1$unem75)


