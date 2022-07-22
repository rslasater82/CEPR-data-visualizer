#Remove comment when running locally, needs to be on when publishing.
#library(shiny)
library(tidyverse)
library(haven)
library(broom)

#load in data
setwd("C:/Users/rslas/OneDrive - West Point/Research/TeachingData")
data <- read_dta("./dat/cepr_march_2018.dta")

#set data to only interested variables, would be really cool to do this dynamically with user input
finaldata <- data %>% select(educ92, occly2d_03, female, incp_wag, age, marstat, empl, wbhao, perhh)

#set factor variables to remove errors when plotting
finaldata$occly2d_03 <- factor(finaldata$occly2d_03)
finaldata <- finaldata %>%
  mutate(#Education = if_else(educ92 < 12, "Less than HS",
    #          if_else(educ92 == 12, "HS Graduate",
    #          if_else(educ92 == 13, "Some College/Associates Degree",
    #          if_else(educ92 == 14, "Bachelors Degree",
    #          if_else(educ92 == 15, "Graduate Degree",
    #          "PhD, MD, JD"))))),
    Education = case_when(
      educ92 <  12 ~ "Less than HS",
      educ92 == 12 ~ "HS Graduate",
      educ92 == 13 ~ "Some College/Associates",
      educ92 == 14 ~ "Bachelors",
      educ92 == 15 ~ "Masters",
      educ92 == 16 ~ "PhD/MD/JD"),
    Education = factor(Education, levels = c("Less than HS",
                                             "HS Graduate", 
                                             "Some College/Associates", 
                                             "Bachelors", 
                                             "Masters", 
                                             "PhD/MD/JD")),
    Sex = if_else(female == 1, 'F', 'M'),
    Sex = factor(Sex, levels = c('F','M')),
    Occupation = occly2d_03,
    Occupation = as.ordered(Occupation),
    Age = age,
    Earnings = incp_wag,
    MaritalStatus = case_when(
      marstat == 1 ~ "Married",
      marstat == 2 ~ "Widowed", 
      marstat == 3 ~ "Divorced", 
      marstat == 4 ~ "Separated",
      marstat == 5 ~ "Never Married"),
    MaritalStatus = factor(MaritalStatus, levels = c("Never Married", 
                                                     "Married", 
                                                     "Separated", 
                                                     "Divorced", 
                                                     "Widowed")),
    Race = case_when(
      wbhao == 1 ~"White",
      wbhao == 2 ~"Black",
      wbhao == 3 ~"Hispanic",
      wbhao == 4 ~"Asian",
      wbhao == 5 ~"Other"),
    Race = factor(Race, levels = c("White", 
                                   "Black",
                                   "Hispanic",
                                   "Asian",
                                   "Other")),
    agesq = age^2,
    FamilySize = perhh,
    FamilyMakeup = case_when(
      perhh == 1 ~ "Single",
      perhh == 2 & marstat == 1 ~ "Married, No Kids",
      perhh == 2 & marstat != 1 ~ "Single, one child",
      perhh == 3 & marstat == 1 ~ "Married, two children",
      perhh == 3 & marstat != 1 ~ "Single, two children",
      perhh == 4 & marstat == 1 ~ "Married, three children",
      perhh == 4 & marstat != 1 ~ "Single, three children",
      perhh > 4 & marstat == 1 ~ "Married, > three children",
      perhh > 4 & marstat != 1 ~ "Single, > three children"),
    
    FamilyMakeup = factor(FamilyMakeup, levels = c("Single",
                                                   "Married, No Kids",
                                                   "Single, one child",
                                                   "Married, two children",
                                                   "Single, two children",
                                                   "Married, three children",
                                                   "Single, three children",
                                                   "Married, > three children",
                                                   "Single, > three children"))
  )
write.csv(finaldata, file ="./dat/CEPR_reduced.csv")
