# analysis for final project

# install packages
install.packages("tidyverse")
install.packages("ggcorrplot")


# libraries
library(readxl) 
library(tidyverse)
library(ggcorrplot)

data <- read_excel("./data/PW Client Risk Assessment Results Coded_cleaned.xlsx")

# Turn unanswered 999s into 0s
drug_use_columns <- c("30A","30B","30C","30D","30E","30F","30G","30H","30I","30J")
for(val in drug_use_columns) {
  data[val][data[val] > 998] <- 0
}

#1,5,8,10,13, 18, 20, 27,28,30,35,37,38
# create subset of dataframe
testable_data <- data.frame(data$`1`, data$`6`,data$`8`, data$`10`, data$`13`, data$`18`,
                            data$`20`, data$`27`, data$`28`,data$`31`,data$`35`, data$`36`, data$`37`, data$`38`, data$`44`)
# Rename columns
testable_data <- testable_data %>%
  rename(
    age = data..1.,
    dependents = data..6.,
    housing = data..8.,
    jail = data..10.,
    health_care = data..13.,
    hiv = data..18.,
    hiv_med = data..20.,
    domestic_abuse = data..27.,
    career_duration = data..28.,
    female_partners = data..35.,
    male_partners = data..37.,
    injected = data..31.,
    unprotected_female = data..36.,
    money = data..44.,
    unprotected_male = data..38.
  )

# collapse all drug use cases into single column
testable_data$drug_use <- data$`30A` + data$`30B` + data$`30C` +
  data$`30D`+ data$`30E`+ data$`30F` +
  data$`30G`+ data$`30H`+ data$`30I` +
  data$`30J`


# create pairwise plot of all variables
pairs(testable_data)

housing_data <- testable_data %>%
  group_by(housing)

# start plotting
housing <- table(testable_data$housing)
pie(housing_data)


ggplot(testable_data, aes(x = housing)) +
  geom_histogram(bins = 2, color = "black", fill = "gray") +
  xlab("Housing Situation") +
  ylab("Count")


ggplot(testable_data, aes(x = housing)) +
  geom_bar(stat="identity", position = "dodge") +
  xlab("Housing Situation") +
  ylab("Count")

# Prep for analysis
testable_data$hiv[testable_data$hiv == 1] <- c("0")
testable_data$hiv[testable_data$hiv == 2] <- c("1")
testable_data$hiv[testable_data$hiv == 4] <- c("1")

testable_data$housing[testable_data$housing == 1] <- c("0")
testable_data$housing[testable_data$housing == 2] <- c("1")

testable_data$jail[testable_data$jail == 1] <- c("0")
testable_data$jail[testable_data$jail == 2] <- c("1")

testable_data$injected[testable_data$injected == 1] <- c("0")
testable_data$injected[testable_data$injected == 2] <- c("1")

testable_data$drug_use[testable_data$drug_use <= 0] <- c("0")
testable_data$drug_use[testable_data$drug_use > 0] <- c("1")

testable_data$hiv <- as.integer(testable_data$hiv)

a <- testable_data[c("age", "dependents", "career_duration", "female_partners", "unprotected_female", "male_partners", "unprotected_male")]
a$unprotected_female <- as.integer(a$unprotected_female)
M <- cor(a)
ggcorrplot(M, lab = TRUE)+ scale_fill_gradient2(limit = c(-1,1), 
                                                low = "white", high =  "black", mid = "grey", 
                                                midpoint = 0)













