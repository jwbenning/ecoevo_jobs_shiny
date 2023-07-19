library(caret)
library(caTools)
library(tidyverse)


jobs_train <- sheet_data %>%
  slice_sample(n = 150) 

jobs_test <- sheet_data %>%
  anti_join(jobs_train)

lm_model <- train(`Offer Rate` ~ `Gender` + `Total Publications`, 
                  data = sheet_data, 
                  method = "lm",
                  na.action = "na.omit")

summary(lm_model)


tree_model <- train(`Offer Rate` ~ `Years Since PhD` + `Gender` + `First Author Pubs` + `Classes Taught`, 
                  data = jobs_train, 
                  method = "rpart",
                  na.action = "na.omit")

summary(lm_model)