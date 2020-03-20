# data <- read.delim(file.choose("adult.data"), sep=" ")
# data
library(tidyverse)
data<-read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data",header=FALSE, na.strings=c("\\?", "N/A"))
data<-data %>% 
  rename("age"=V1, "workclass"=V2, "fnlweight"=V3, "education"=V4, "educationNum"=V5, "maritalStatus"=V6, 
         "occupation"=V7, "relationship"=V8, "race"=V9, "sex"=V10, "capitalGain"=V11, "capitalLoss"=V12,
         "hoursPerWeek"=V13, "nativeCountry"=V14, "income"=V15)
data2<-data %>% filter(!str_detect(age, "\\?"), !str_detect(workclass, "\\?"), !str_detect(fnlweight, "\\?"), 
         !str_detect(educationNum, "\\?"), !str_detect(maritalStatus, "\\?"), !str_detect(occupation, "\\?"),
         !str_detect(relationship, "\\?"), !str_detect(race, "\\?"), !str_detect(sex, "\\?"),
         !str_detect(capitalGain, "\\?"), !str_detect(capitalLoss, "\\?"), !str_detect(hoursPerWeek, "\\?"),
         !str_detect(nativeCountry, "\\?"), !str_detect(income, "\\?"))

