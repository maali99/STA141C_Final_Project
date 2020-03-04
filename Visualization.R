library(ggplot2)
library(tidyverse)



adult = read.table("adult.data",sep = ",")
colnames(adult) = c("Age","Workclass","fnlwgt","Education","Education_num",
                   "Marital_status","Occupation","Relationship","Race",
                   "Sex","Capital_gain","Capital-loss","Hours_per_week",
                   "Native_country","Income")
adult[adult == " ?"] = NA
data = na.omit(adult)


fig = ggplot(data = adult, mapping = aes(x = Education_num, fill = Income))
fig = fig + geom_bar() + facet_grid(Relationship~Sex, scales = "free_y")
ggsave(filename="Income_Sex_Education_Relative.pdf", plot = fig)