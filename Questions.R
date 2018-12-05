setwd(
  "/Users/ivanloktaev/Yandex.Disk.localized/Uni/4\ Курс/Data\ Science/HWs/ha2\ materials/"
)
library(haven)
library(data.table)
data_1 <- read_dta("data_1.dta")
# What is done above is initial setup

#1
# Create two additional dummy variables unem74, unem75
# Create new variables and then cbind them to the data_1 date.frame
# Add new variables to the data.frame making sure
# they are written as 1 and 0 no TRUE and FALSE
data_1 <-
  cbind(data_1,
        unem74 = as.numeric(data_1$re74 == 0),
        unem75 = as.numeric(data_1$re75 == 0))

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

# Examining the assuption
View(p_matrix)
# as can be seen from the p-values, for majority of
# covariates the assumption holds true
# only nodeg has explanatory power in relation to treat at s.l = 0.05
# and variables hispanic and unem75 have expl.power at s.l = 0.01

#2
# Сonverting re78 to thousands
data_1$re78 <- data_1$re78 / 1000
# Running regression for #2 and displaying its summary
summary(lm(data_1$re78 ~ data_1$treat))

# Now look at stimates slope coefficient
# If for some person job training program was done,
# earnings in 1978 increase on average by 1.7943 thousand of dollars

#3
# Running regression for #3 and displaying its summary
summary(
  lm(
    data_1$re78 ~ data_1$treat + data_1$age + data_1$education + data_1$black + data_1$hispanic + data_1$married +
      data_1$nodegree + data_1$re74 + data_1$re75 + data_1$unem74 + data_1$unem75
  )
)

# Only treat coefficient is significant (at alpha = 0.05) in this model
# The st.error of treat in model 1 was 0.6329,
# While in the new model it is 6.411e-01, which is sligtly larger.
# Thus, we do not need to add additional (insignificant) variables
# and can drop them off the regression

#4
data_2 <- read_dta("data_2.dta")
