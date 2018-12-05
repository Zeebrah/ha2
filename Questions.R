setwd(
  "/Users/ivanloktaev/Yandex.Disk.localized/Uni/4\ Курс/Data\ Science/HWs/ha2\ materials/"
)
library(haven)
data_1 <- read_dta("data_1.dta")
# What is done above is initial setup

#1
# Create two additional dummy variables unem74, unem75
# Create new variables and then cbind them to the data_1 date.frame
# Add new variables to the data.frame
data_1 <-
  cbind(data_1, unem74 = data_1$re74 == 0, unem75 = data_1$re75 == 0)

# Running univariate regressions, and writing their summaries to respective variables
reg_age <- summary(lm(data_1$treat ~ data_1$age))
reg_edu <- summary(lm(data_1$treat ~ data_1$education))
reg_black <- summary(lm(data_1$treat ~ data_1$black))
reg_hisp <- summary(lm(data_1$treat ~ data_1$hispanic))
reg_married <- summary(lm(data_1$treat ~ data_1$married))
reg_nodeg <- summary(lm(data_1$treat ~ data_1$nodegree))
reg_re74 <- summary(lm(data_1$treat ~ data_1$re74))
reg_re75 <- summary(lm(data_1$treat ~ data_1$re75))
reg_unem74 <- summary(lm(data_1$treat ~ data_1$unem74))
reg_unem75 <- summary(lm(data_1$treat ~ data_1$unem75))

# Collecting p-values into a matrix
p_matrix <- c(
  p_age = reg_age$coefficients[2, 4],
  p_edu = reg_edu$coefficients[2, 4],
  p_black = reg_black$coefficients[2, 4],
  p_hisp = reg_hisp$coefficients[2, 4],
  p_married = reg_married$coefficients[2, 4],
  p_nodeg = reg_nodeg$coefficients[2, 4],
  p_re74 = reg_re74$coefficients[2, 4],
  p_re75 = reg_re75$coefficients[2, 4],
  p_unem74 = reg_unem74$coefficients[2, 4],
  p_unem75 = reg_unem75$coefficients[2, 4]
)
