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


# Running univariate regressions for each variable except re78, taking summaries from them
# and writing particlur p-value form the summary's coefficient table
# to the respective slot in the p-value matrix
# (For a more step-by-step appeoach look at the comments right below)
p_matrix <- c(
  p_age = summary(lm(data_1$treat ~ data_1$age))$coefficients[2, 4],
  p_edu = summary(lm(data_1$treat ~ data_1$education))$coefficients[2, 4],
  p_black = summary(lm(data_1$treat ~ data_1$black))$coefficients[2, 4],
  p_hisp = summary(lm(data_1$treat ~ data_1$hispanic))$coefficients[2, 4],
  p_married = summary(lm(data_1$treat ~ data_1$married))$coefficients[2, 4],
  p_nodeg = summary(lm(data_1$treat ~ data_1$nodegree))$coefficients[2, 4],
  p_re74 = summary(lm(data_1$treat ~ data_1$re74))$coefficients[2, 4],
  p_re75 = summary(lm(data_1$treat ~ data_1$re75))$coefficients[2, 4],
  p_unem74 = summary(lm(data_1$treat ~ data_1$unem74))$coefficients[2, 4],
  p_unem75 = summary(lm(data_1$treat ~ data_1$unem75))$coefficients[2, 4]
)

# Alternatively this can be done in 2 consequtive steps
# but it would create a lot of unnecessary variables

# Running univariate regressions, and writing their summaries to respective variables
# reg_age <- summary(lm(data_1$treat ~ data_1$age))
# reg_edu <- summary(lm(data_1$treat ~ data_1$education))
# reg_black <- summary(lm(data_1$treat ~ data_1$black))
# reg_hisp <- summary(lm(data_1$treat ~ data_1$hispanic))
# reg_married <- summary(lm(data_1$treat ~ data_1$married))
# reg_nodeg <- summary(lm(data_1$treat ~ data_1$nodegree))
# reg_re74 <- summary(lm(data_1$treat ~ data_1$re74))
# reg_re75 <- summary(lm(data_1$treat ~ data_1$re75))
# reg_unem74 <- summary(lm(data_1$treat ~ data_1$unem74))
# reg_unem75 <- summary(lm(data_1$treat ~ data_1$unem75))
#
# Collecting p-values (from a coefficient tabls) into a matrix

# p_matrix <- c(
#   p_age = reg_age$coefficients[2, 4],
#   p_edu = reg_edu$coefficients[2, 4],
#   p_black = reg_black$coefficients[2, 4],
#   p_hisp = reg_hisp$coefficients[2, 4],
#   p_married = reg_married$coefficients[2, 4],
#   p_nodeg = reg_nodeg$coefficients[2, 4],
#   p_re74 = reg_re74$coefficients[2, 4],
#   p_re75 = reg_re75$coefficients[2, 4],
#   p_unem74 = reg_unem74$coefficients[2, 4],
#   p_unem75 = reg_unem75$coefficients[2, 4]
# )


# Examining the assuption
View(p_matrix)
# as can be seen from the p-values, for majority of
# covariates the assumption holds true
# only nodeg has explanatory power in relation to treat at s.l = 0.05
# and variables hispanic and unem75 have expl.power at s.l = 0.01

#2
# Сonverting re78 to thousands
data_1_ths <- data_1
data_1_ths$re78 <- data_1_ths$re78 / 1000
# Running regression for #2 and displaying its summary
summary(lm(data_1_ths$re78 ~ data_1_ths$treat))

# Now look at estimated slope coefficient
# If for some person job training program was done,
# earnings in 1978 increase on average by 1.7943 thousand of dollars

#3
# Running regression for #3 and displaying its summary
summary(
  lm(
    data_1_ths$re78 ~ data_1_ths$treat + data_1_ths$age + data_1_ths$education + data_1_ths$black + data_1_ths$hispanic + data_1_ths$married +
      data_1_ths$nodegree + data_1_ths$re74 + data_1_ths$re75 + data_1_ths$unem74 + data_1_ths$unem75
  )
)

# Only treat coefficient is significant (at alpha = 0.05) in this model
# The st.error of treat in model 1 was 0.6329,
# While in the new model it is 6.411e-01, which is sligtly larger.
# Thus, we do not need to add additional (insignificant) variables
# and can drop them off the regression

#4
# Loading new data and adding dummy columns unem74, unem75
data_2 <- as.data.table(read_dta("data_2.dta"))
data_2 <-
  cbind(data_2,
        unem74 = as.numeric(data_2$re74 == 0),
        unem75 = as.numeric(data_2$re75 == 0))

# Dropping all obs. where treat is 0
data_1 <- as.data.table(data_1)
data_1_dropped <- data_1[treat == 1]

# Dropping all obs. where treat is 1
data_2_dropped <- data_2[treat == 0]

# Merging two cleaned up data tables
data_non_experimental <- rbind(data_1_dropped, data_2_dropped)

#1 redone for non-experimental
# We would employ the same methodology here as before
# Dummie varibles were added already

p_matrix_nonex <- c(
  p_age = summary(lm(
    data_non_experimental$treat ~ data_non_experimental$age
  ))$coefficients[2, 4],
  p_edu = summary(
    lm(
      data_non_experimental$treat ~ data_non_experimental$education
    )
  )$coefficients[2, 4],
  p_black = summary(
    lm(data_non_experimental$treat ~ data_non_experimental$black)
  )$coefficients[2, 4],
  p_hisp = summary(
    lm(data_non_experimental$treat ~ data_non_experimental$hispanic)
  )$coefficients[2, 4],
  p_married = summary(
    lm(data_non_experimental$treat ~ data_non_experimental$married)
  )$coefficients[2, 4],
  p_nodeg = summary(
    lm(data_non_experimental$treat ~ data_non_experimental$nodegree)
  )$coefficients[2, 4],
  p_re74 = summary(lm(
    data_non_experimental$treat ~ data_non_experimental$re74
  ))$coefficients[2, 4],
  p_re75 = summary(lm(
    data_non_experimental$treat ~ data_non_experimental$re75
  ))$coefficients[2, 4],
  p_unem74 = summary(
    lm(data_non_experimental$treat ~ data_non_experimental$unem74)
  )$coefficients[2, 4],
  p_unem75 = summary(
    lm(data_non_experimental$treat ~ data_non_experimental$unem75)
  )$coefficients[2, 4]
)

View(p_matrix_nonex)
# Everything is extremely significant at any reasonable s.l
# So, the assumption cleary does not hold

#2 redone for non-experimental
# Сonverting re78 to thousands
data_non_experimental_ths <- data_non_experimental
data_non_experimental_ths$re78 <-
  data_non_experimental_ths$re78 / 1000
# Running regression for #2 and displaying its summary
summary(lm(
  data_non_experimental_ths$re78 ~ data_non_experimental_ths$treat
))

# Now look at etimated slope coefficient
# If for some person job training program1 was done,
# earnings in 1978 decrease on average by 8.49752 thousand of dollars

#3 redone for non-experimental
# Running regression for #3 and displaying its summary
summary(
  lm(
    data_non_experimental_ths$re78 ~ data_non_experimental_ths$treat + 
      data_non_experimental_ths$age + data_non_experimental_ths$education + 
      data_non_experimental_ths$black + data_non_experimental_ths$hispanic + 
      data_non_experimental_ths$married + data_non_experimental_ths$nodegree + 
      data_non_experimental_ths$re74 + data_non_experimental_ths$re75 + 
      data_non_experimental_ths$unem74 + data_non_experimental_ths$unem75
  )
)

# Only treat coefficient is significant (at alpha = 0.05) in this model
# The st.error of treat in model 1 was 0.6329,
# While in the new model it is 6.411e-01, which is sligtly larger.
# Thus, we do not need to add additional (insignificant) variables
# and can drop them off the regression