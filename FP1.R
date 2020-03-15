# Final Project 

# read data 
library(readr)
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

# Full Model 
fmodel <- glm(income ~., family = binomial, data = data1)
summary(fmodel)

# Model w/ continuous and categorical variables 
model1 <- glm(income ~ age + race + sex + education_num + hours_per_week, family = binomial, data = data1)
er1 <- summary(model1)$coef[,1]
er2 <- model1$coefficients

################ Bootstrap Coefficients for Best Model#####################

# For every iteration
# randomize data
# find the slopes/pvalue

suppressMessages(library(tidyverse))
library(furrr)
plan(multiprocess, workers = 4)
B = 1000
n = nrow(data1)

m <- 10
groups <- sample(seq_len(m), n, replace = TRUE)
map(seq_len(m), ~write_csv(data1[groups==.,], str_c("part",.,".csv")))

single_boot <- function(i, ind_part){
  subsample = read_csv(str_c("part",ind_part, ".csv"))
  freqs = rmultinom(1, n, rep(1, nrow(subsample)))
  model1 <- glm(as.factor(income) ~ age + race + sex + education_num + hours_per_week, weight = freqs, family = binomial, data = subsample)
  as.data.frame(t(as.matrix(model1$coefficients)))
}

ci_list = future_map(seq_len(m), ~{
  map_dfr(seq_len(B), single_boot, ind_part =.) %>%
    map_dfr(~quantile(., c(0.025,0.975))) } )

ci_list
ci_list[[1]]

var1 <- reduce(ci_list, `+`) / length(ci_list)

# If subjct is Other, age = 30, female = 1, bachelors, work 40 hrs
est1 = -1.010339*(1) + 0.04323100*(30)-10.151366 + 0.3403326*(13)+0.03281120*(40)
# odds of having income greater than 50k for subject
odd1 = exp(est1)
# probabiliy of income greater than 50k of subject
prob1 = odd1/(1 + odd1)


#### Lower Bound 
# Actual subject from data
# If subjct is Black, age = 37, male = 1, edu_num = 10, work 80 hrs
est2 = -0.1799829*(1) + 0.04323100*(37) - 10.151366 + 0.3403326*(10) + 0.03281120*(80) + 1.074467*(1)
# odds of having income greater than 50k for subject
odd2 = exp(est2)
# probabiliy of income greater than 50k of subject
prob2 = odd2/(1 + odd2)

#### Upper Bound
est3 = 0.6243565*(1) + 0.04755516*(37) - 9.240312 + 0.3665408*(10) + 0.03805404*(80) + 1.225831*(1)
# odds of having income greater than 50k for subject
odd3 = exp(est3)
# probabiliy of income greater than 50k of subject
prob3 = odd3/(1 + odd3)



med_list = future_map(seq_len(m), ~{
  map_dfr(seq_len(B), single_boot, ind_part =.) %>%
    map_dfr(~median(.)) } )

med1 <- reduce(med_list, `+`) / length(med_list)
med1

# Same subject 
#### Median - Reduced
est4 = 0.1957653*(1) + 0.04541634*(37) -9.670722 +  0.3532976*(10) + 0.03542691*(80) + 1.148214*(1)
# odds of having income greater than 50k for subject
odd4 = exp(est4)
# probabiliy of income greater than 50k of subject
prob4 = odd4/(1 + odd4)



