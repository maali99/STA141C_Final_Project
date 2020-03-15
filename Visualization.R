library(ggplot2)
library(tidyverse)



adult = read.table("adult.data",sep = ",")
colnames(adult) = c("Age","Workclass","fnlwgt","Education","Education_num",
                   "Marital_status","Occupation","Relationship","Race",
                   "Sex","Capital_gain","Capital-loss","Hours_per_week",
                   "Native_country","Income")
adult[adult == " ?"] = NA
adult = na.omit(adult)


fig = ggplot(data = adult, mapping = aes(x = Education_num, fill = Income))
fig = fig + geom_bar() + facet_grid(Relationship~Sex, scales = "free_y")
ggsave(filename="Income_Sex_Education_Relative.pdf", plot = fig)





set.seed(141)
m <- 10
groups <- sample(seq(1,m), nrow(adult), replace = TRUE)
dir.create("adults/", showWarnings = FALSE)
for(i in seq_len(m)){
  write.csv(filter(adult, groups == i), paste("adults/",i,".csv"))
}

file_names <- file.path("adults", list.files("adults"))

each_boot <- function(i, data){
  freqs <- rmultinom(1, nrow(data), rep(1, nrow(data)))/nrow(data)
  
}