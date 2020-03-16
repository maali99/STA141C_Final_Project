library(ggplot2)
library(tidyverse)
library(readr)

# read data 
url1 <- "https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"
data1 <- read.csv(url1, header = FALSE)

colnames(data1) <- c("age",
                     "workclass",
                     "fnlwgt",
                     "education",
                     "education_num",
                     "marital_status",
                     "occupation",
                     "relationship",
                     "race",
                     "sex",
                     "capital_gain",
                     "capital_loss",
                     "hours_per_week",
                     "native-country",
                     "income")
attach(data1)  

#age + race + sex + education_num + hours_per_week

# Plot ratios of income in male group and female group
fig = ggplot(data = data1, mapping = aes(x = sex, fill = income))
fig = fig + geom_bar(position = "fill")
ggsave(filename="sex_income.pdf", plot = fig)

# plot the number of income > 50K and income <=50K grouped by sex and education_num
fig = ggplot(data = data1, mapping = aes(x = education_num, fill = income))
fig = fig + geom_bar() + facet_grid(sex, scales = "free_y")
ggsave(filename="education_sex_income.pdf", plot = fig)

# plot  the number of income > 50K and income <=50K grouped by race and education
fig = ggplot(data = data1, mapping = aes(x = education_num, fill = income))
fig = fig + geom_bar() + facet_grid(race, scales = "free_y")
ggsave(filename="education_race_income.pdf", plot = fig)

# plot the distribution of age groupped by income
fig = ggplot(data = data1, mapping = aes(x = age, fill = income, color = income))
fig = fig + geom_density(alpha = 0.3)
ggsave(filename="age_income.pdf", plot = fig)

# plot the percentage of income >= 50K in each hours_per_week group
hours_income = table(data1[,c('hours_per_week','income')])
hours_income <- as.data.frame(prop.table(hours_income, 1))
hours_income_over50 = hours_income[(nrow(hours_income)/2+1):nrow(hours_income),]
fig = ggplot(data = hours_income_over50, 
             mapping = aes(
             x = hours_per_week, 
             y = Freq,
             group = income))
fig = fig + geom_area(fill = "chartreuse4",alpha = 0.7) + geom_smooth()+
  scale_x_discrete(
    breaks = c(1, 20, 40, 60, 80, 99)
  )+
  labs(
    y = "Percentage of Income >= 50K"
  )
ggsave(filename="hours_income.pdf", plot = fig)




