setwd(
  "/Users/ivanloktaev/Yandex.Disk.localized/Uni/4\ Курс/Data\ Science/HWs/ha2\ materials/"
)
library(haven)
library(data.table)
data_1 <- read_dta("data_1.dta")
# Obviously, seed should be 42
set.seed(42)
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
# Сonverting re78 to thousands and crating extra data frame
# so as not to disrupt the original form the file
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
# Dummie varibles were added already (look couple of lines above)

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

# Extimated effect of treat is +1066 dollars of income as a result of training completion
# Only treat coefficient is significant (at alpha = 0.05) in this model
# The st.error of treat in model 1 was 0.6329,
# While in the new model it is 6.411e-01, which is sligtly larger.
# Thus, we do not need to add additional (insignificant) variables
# and can drop them off the regression

#5
library(glmnet)
# Here we use re78 in dollars not thousands since we WERE NOT specifiaccly
# asked to do it in thousands, it doesn't change much anyways

# Creating a matrix of treat and all linear covariates:

#a) All linear covariates and treat
# i.e everything except column of re78

# Some trickery to arrange columns so that dummies are on the left
# First exclude every non-dummy columns (including re78)
# from the data set using appropriate column numbers
# Then cbind these columns to all excluded factors' columns
x <-
  as.matrix(cbind(data_non_experimental[, c(-2, -3, -8, -9, -10)], data_non_experimental[, c(2, 3, 8, 9)]))

# Same thing but more detailed:
# So we cbind  dummies and non-dummie factors' columns
# dummies = data_non_experimental[, c(-2,-3,-8,-9,-10)]
# non_dummy_factors = data_non_experimental[, c(2,3,8,9)]

#b) Their interactions
# Combinations for use in filling of matrices
# It includes all first order interaction b/w covariates
# So we add 1 to exclude treat variable
combinations <- combn(10, 2) + 1

# Predefining the dimensions of linear terms interaction matrix
inter <- matrix(nrow = nrow(x), ncol = ncol(combinations))
# Creating a set of empty column names for inter
colnames(inter) <- rep(NA, ncol(combinations))
# Loop to fill the matrix and set the names
for (i in 1:ncol(combinations)) {
  inter[, i] <- x[, combinations[1, i]] * x[, combinations[2, i]]
  # Setting the name of the respective column to represent the contents
  colnames(inter)[i] <-
    paste(colnames(x)[combinations[1, i]], colnames(x)[combinations[2, i]], sep =
            "*")
}

# Merge matrices of covariates and their interactions
first_order_regressors <- cbind(x, inter)

#c) All second order terms of covariates
# Second order terms of covariates (except dummies)
# Since squared dummie would be collinear to just dummy
covariates_sq <- matrix(nrow = nrow(x), ncol = 4)
for (i in 1:4) {
  covariates_sq[, i] <- x[, i + 7] ^ 2
}
# Giving meaningful names to factor columns
colnames(covariates_sq) <-
  c("age^2", "education^2", "re74^2", "re75^2")

#d) All second order ineractions of covariates
# Here we find all the cross-terms for factors^2 and dummies
# Again since dummie squared is just dummies
# Following line defines factors which cross terms we need
# it contains all dummies except treat and all squares of non-dimmies
inter_sq_input <- cbind(x[, 2:7], covariates_sq)

#To go through all combinations
# we can't use same approach as in b)
# as it would make a ton of excess dummie interactions to appear
# which are already included
# combinations_for_sq <- combn(10, 2) - not used

# instead, this specific design of combination matrix would insure
# that all second order interactions of 2nd order terms are included
# while not repeating the variables included before
combinations_d <- rbind(rep(1:6, 4), rep(7:10, 6))

# Predefine the matrix for output
inter_sq <- matrix(nrow = nrow(x), ncol = ncol(combinations_d))
# Creating a set of empty column names for inter_sq
colnames(inter_sq) <- rep(NA, ncol(combinations_d))

# Loop to fill the matrix
for (i in 1:ncol(combinations_d)) {
  inter_sq[, i] <-
    inter_sq_input[, combinations_d[1, i]] * inter_sq_input[, combinations_d[2, i]]
  # Setting the name of the respective column to represent the contents
  colnames(inter_sq)[i] <-
    paste(colnames(inter_sq_input)[combinations_d[1, i]],
          colnames(inter_sq_input)[combinations_d[2, i]], sep = "*")
}

# Merge all second order regressors
second_order_regressors <- cbind(covariates_sq, inter_sq)

#e) All third order terms of covariates
covariates_cube <- matrix(nrow = nrow(x), ncol = 4)
for (i in 1:4) {
  covariates_cube[, i] <-  x[, i + 7] ^ 3
}

# Giving meaningful names to factor columns
colnames(covariates_cube) <-
  c("age^3", "education^3", "re74^3", "re75^3")


# Combine ALL REGRESSORS into a final matrix of all 1st and 2nd order regressors,
# Adding also third order for non-dummies from e)
final_regressors <-
  as.matrix(cbind(
    first_order_regressors,
    second_order_regressors,
    covariates_cube
  ))

# Fit the lasso model using all computed variables
cvfit <-
  cv.glmnet(x = final_regressors, y = data_non_experimental$re78, alpha =
              1)
# Obtain coefficients of the model with minimum MSE
result <- coef(cvfit, s = cvfit$lambda.min)
result
# thus by model chosen through LASSO
# the effect of treat on re78 is strictly positive and is interpreted as
# is an average increase in income (in dollars) from the training

# 6 Matching estimator
# Estimate model_baseline
# a) : i,ii,iii
library(lmtest)

# 1st iteration
data_non_experimental <- as.data.frame(data_non_experimental)
cut_1 <- matrix(NA, nrow = 6)
model_baseline <- list('vector')
model_aux <- list('vector')
for (i in 2:7) {
  model_baseline[[i]] <-
    glm(treat ~ re74 + re75 + unem74 + unem75 ,
        data = data_non_experimental,
        family = "binomial")
  model_aux[[i]] <-
    glm(
      data_non_experimental$treat ~ data_non_experimental$re74 + data_non_experimental$re75
      + data_non_experimental$unem74 + data_non_experimental$unem75 + data_non_experimental[, i],
      family = "binomial"
    )
  cut_1[[i]] <- lrtest(model_baseline[[i]], model_aux[[i]])$Chisq[2]
}

cut_1
# 1st position is left blank since it corresponds to treat variable
# which os not a covariate
# So now, we can clearly see that regressor number 4, i.e "black"
# Has the highest cutoff at 530.1558815, so we updatej model with it and iterate

# 2nd iteration
cut_2 <- matrix(NA, nrow = 6)
model_baseline <- list('vector')
model_aux <- list('vector')
for (i in c(2, 3, 5, 6, 7)) {
  # i doesn't go through 4 since black is now in base model
  model_baseline[[i]] <-
    glm(treat ~ re74 + re75 + unem74 + unem75 + black ,
        data = data_non_experimental,
        family = "binomial")
  model_aux[[i]] <-
    glm(
      data_non_experimental$treat ~ data_non_experimental$re74 + data_non_experimental$re75
      + data_non_experimental$unem74 + data_non_experimental$unem75 +
        data_non_experimental$black + data_non_experimental[, i],
      family = "binomial"
    )
  cut_2[[i]] <- lrtest(model_baseline[[i]], model_aux[[i]])$Chisq[2]
}
cut_2

# now positions 1,4 are blank for the same reason
# So now, we can clearly see that regressor number 6, i.e "married"
# Has the highest cutoff at 42.95838, so we update model with it and iterate

# 3rd iteration
cut_3 <- matrix(NA, nrow = 6)
model_baseline <- list('vector')
model_aux <- list('vector')
for (i in c(2, 3, 5, 7)) {
  # i doesn't go through 4 since black is now in base model
  model_baseline[[i]] <-
    glm(
      treat ~ re74 + re75 + unem74 + unem75 + black + married ,
      data = data_non_experimental,
      family = "binomial"
    )
  model_aux[[i]] <-
    glm(
      data_non_experimental$treat ~ data_non_experimental$re74 + data_non_experimental$re75
      + data_non_experimental$unem74 + data_non_experimental$unem75 +
        data_non_experimental$black + data_non_experimental$married +
        data_non_experimental[, i],
      family = "binomial"
    )
  cut_3[[i]] <- lrtest(model_baseline[[i]], model_aux[[i]])$Chisq[2]
}
cut_3

# now positions 1,4,6 are blank for the same reason
# So now, we can clearly see that regressor number 7, i.e "nodegree"
# Has the highest cutoff at 42.95838, so we update model with it and iterate

# 4th iteration
cut_4 <- matrix(NA, nrow = 6)
model_baseline <- list('vector')
model_aux <- list('vector')
for (i in c(2, 3, 5)) {
  # i doesn't go through 4 since black is now in base model
  model_baseline[[i]] <-
    glm(
      treat ~ re74 + re75 + unem74 + unem75 + black + married + nodegree,
      data = data_non_experimental,
      family = "binomial"
    )
  model_aux[[i]] <-
    glm(
      data_non_experimental$treat ~ data_non_experimental$re74 + data_non_experimental$re75
      + data_non_experimental$unem74 + data_non_experimental$unem75 +
        data_non_experimental$black + data_non_experimental$married +
        data_non_experimental$nodegree + data_non_experimental[, i],
      family = "binomial"
    )
  cut_4[[i]] <- lrtest(model_baseline[[i]], model_aux[[i]])$Chisq[2]
}
cut_4

# now positions 1,4,6,7 are blank for the same reason
# So now, we can clearly see that regressor number 5, i.e "hispanic"
# Has the highest cutoff at 17.4678228, so we update model with it and iterate


# 5th iteration
cut_5 <- matrix(NA, nrow = 6)
model_baseline <- list('vector')
model_aux <- list('vector')
for (i in c(2, 3)) {
  # i doesn't go through 4 since black is now in base model
  model_baseline[[i]] <-
    glm(
      treat ~ re74 + re75 + unem74 + unem75 + black + married + nodegree + hispanic,
      data = data_non_experimental,
      family = "binomial"
    )
  model_aux[[i]] <-
    glm(
      data_non_experimental$treat ~ data_non_experimental$re74 + data_non_experimental$re75
      + data_non_experimental$unem74 + data_non_experimental$unem75 +
        data_non_experimental$black + data_non_experimental$married +
        data_non_experimental$nodegree + data_non_experimental$hispanic +
        data_non_experimental[, i],
      family = "binomial"
    )
  cut_5[[i]] <- lrtest(model_baseline[[i]], model_aux[[i]])$Chisq[2]
}
cut_5

# now positions 1,4,6,7,5 are blank for the same reason
# So now, we can clearly see that regressor number 2, i.e "age"
# Has the highest cutoff at 3.4608758, so we update model with it and iterate

# 6th iteration
cut_6 <- matrix(NA, nrow = 6)
model_baseline <- list('vector')
model_aux <- list('vector')
for (i in c(3)) {
  # i doesn't go through 4 since black is now in base model
  model_baseline[[i]] <-
    glm(
      treat ~ re74 + re75 + unem74 + unem75 + black + married + nodegree + hispanic + age,
      data = data_non_experimental,
      family = "binomial"
    )
  model_aux[[i]] <-
    glm(
      data_non_experimental$treat ~ data_non_experimental$re74 + data_non_experimental$re75
      + data_non_experimental$unem74 + data_non_experimental$unem75 +
        data_non_experimental$black + data_non_experimental$married +
        data_non_experimental$nodegree + data_non_experimental$hispanic +
        data_non_experimental$age  + data_non_experimental[, i],
      family = "binomial"
    )
  cut_6[[i]] <- lrtest(model_baseline[[i]], model_aux[[i]])$Chisq[2]
}
cut_6

# FINALLY, case we found a model where all cutoffs are below 1,
# i.e containing variales to include in the regression for the propensity score

# b) : i,ii,iii
# first, let's establish what is the current model_baseline
# and which interactions ar e candidates for addition
# First we need to put new interaction covariates into convinient form
# inter is interactions of linear covatiater and
# covariates_sq contains all the relevant squares to add to the model
covariates_6b <- cbind(inter, covariates_sq)

# But we also need treat and base equation to be contained in the data set
data_6b <- cbind(data_non_experimental, covariates_6b)
# while loop seem a logical addition on top of existing for loop
# it would help us choose the best addition to best model in each iteration
# Let's first save what we had as an optimal equation in 6a)
base_equation <-
  as.character("treat ~ re74 + re75 + unem74 + unem75 + black + married + nodegree + hispanic + age")

# Preparation
model_baseline <- list('vector')
model_aux <- list('vector')
# 100 rows just in case
cut_6b <- matrix(100, nrow = ncol(covariates_6b))

while (max(cut_6b) > 1) {
  # update the baseline model from the result of the last while loop cicle
  model_baseline <-
    glm(as.formula(base_equation),
        data = data_6b,
        family = "binomial")
  
  # Go as long  as needed to go through all the candidates
  for (i in 1:ncol(covariates_6b)) {
    # introducing new regressor from the covariate_6b set by its name
    add_reg <- colnames(covariates_6b)[i]
    # Update the formula with new regressor and establish the model_aux
    model_aux <-
      glm(as.formula(paste(base_equation, add_reg, sep  = " + ")), data = data_6b,
          family = "binomial")
    cut_6b[[i]] <- lrtest(model_baseline, model_aux)$Chisq[2]
  }
  # Upadate base model with a regressor corresponding to maximal cutoff
  update_reg <- colnames(covariates_6b)[which.max(cut_6b)]
  # introduce break so that we only update the
  # equation if in current cutoff matrix max value is above 1
  if (max(cut_6b) < 1) {
    break
  }
  # Update equation for baseline model
  base_equation <- paste(base_equation, update_reg, sep = " + ")
}
base_equation

#7 perdict peropensity score for each observation
prediction_model <-
  glm(as.formula(base_equation),
      data = data_6b,
      family = "binomial")
prediction_df <-
  data.frame(
    propensity_score = predict(prediction_model, type = "response"),
    treat = prediction_model$model$treat
  )
