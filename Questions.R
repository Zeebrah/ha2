setwd("/Users/ivanloktaev/Yandex.Disk.localized/Uni/4\ Курс/Data\ Science/HWs/ha2\ materials/")
library(haven)
data_2 <- read_dta("data_2.dta")
# What is done above is initial setup 

#1
# Create two additional dummy variables unem74, unem75

# Create new variables
unem74 <- data_2$re74 == 0
unem75 <- data_2$re75 == 0

# Add new variables to the data.frame
data_2 <- cbind(data_2, unem74, unem75)
