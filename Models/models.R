#library(caret)
#library(caTools)
library(tidyverse)
#library(glmnet)
#library(brms)
#library(brmstools)
#library(bayesplot)
#library(tidybayes)

vars <- c("Current Position", 
          "Years Since PhD", 
          "Gender", 
          "First Author Pubs", 
          "Total Publications", 
          "Fellowships", 
          "Major Grants", 
          "Classes Taught", 
          "Applications", 
          "Offers")

lasso_df <- sheet_data %>%
  filter(!if_any(all_of(vars), is.na)) %>%
  mutate(reject = Applications - Offers,
         gender_bi = ifelse(Gender == "Male", 0, 1)) %>%
  rename_all(~str_replace_all(., " ", ""))


#######################################################
####### Bayesian logistic regression
#######################################################

skeptical_prior <- set_prior("normal(0, 0.2)", class = "b")

#horseshoe_prior <- set_prior("horseshoe(df=2)")

bayes_model <- brm(
  Offers | trials(Applications) ~ YearsSincePhD + CurrentPosition + Gender + FirstAuthorPubs + TotalPublications + Fellowships + MajorGrants + ClassesTaught,
  data = lasso_df,
  family = binomial(),
  prior = skeptical_prior,
  chains = 4,
  iter = 3000,
  cores = 4
)

##############
### Model summary
##############

summary(bayes_model)

#plot(conditional_effects(bayes_model), ask = F)

#posterior_samples <- as.matrix(bayes_model)
#color_scheme_set("blue")
#mcmc_areas(posterior_samples, 
#           pars = c("b_YearsSincePhD"))

mcmc_plot(bayes_model, prob_outer = 0.95)




####################
####### PREDICTION
####################

new_values_data <- data.frame(Applications = 150,
                              YearsSincePhD = 5,
                              CurrentPosition = "Postdoc",
                              Gender = "Male",
                              FirstAuthorPubs = 7,
                              TotalPublications = 10,
                              Fellowships = 2,
                              MajorGrants = 1,
                              ClassesTaught = 0)

##############
### Predictions -- count
##############

predictions <- posterior_predict(bayes_model, newdata = new_values_data)

mean(predictions[,1])

hist(predictions)

##############
### Predictions -- probability
##############

predictions_prob <- fitted(bayes_model, newdata = new_values_data)
predict(bayes_model, newdata = new_values_data, type = "response")















#######################################################
####### LASSO
#######################################################
# Extracting predictors and response


x <- as.matrix(lasso_df[,c('Years Since PhD', 'First Author Pubs', 'Classes Taught', 'gender_bi')])
y <- as.matrix(lasso_df[, c("reject", "Offers")])

cv_lasso <- cv.glmnet(x, y, family = "binomial", alpha=1)
best_lambda <- cv_lasso$lambda.min

lasso_model <- glmnet(x, y, family = "binomial", alpha=1, lambda=best_lambda) # alpha=1 indicates Lasso regression

coef(lasso_model, s=best_lambda)

lasso_df$prediction <- predict(lasso_model, newx = new_values, type = "response", s = best_lambda)


#######################################################
####### LASSO with bootstrap
#######################################################
# Initialize
set.seed(123) # for reproducibility
n <- nrow(lasso_df)
n_bootstraps <- 100
bootstrap_predictions <- matrix(NA, n, n_bootstraps)

new_values <- matrix(c(3, 5, 0, 0,
                 3, 5, 0, 1), 
               ncol=4, byrow=TRUE)

# Get predictions for these new values
predictions_for_new_values <- predict(lasso_model, newx = new_values, type = "response", s = best_lambda)

# Bootstrap to get SEs for these new predictions
bootstrap_predictions_new_values <- matrix(NA, nrow(new_values), n_bootstraps)

for (i in 1:n_bootstraps) {
  
  # Sample with replacement (from original dataset)
  bootstrap_indices <- sample(1:n, n, replace=TRUE)
  bootstrap_data <- lasso_df[bootstrap_indices, ]
  
  x_boot <- as.matrix(bootstrap_data[,c('Years Since PhD', 'First Author Pubs', 'Classes Taught', 'gender_bi')])
  y_boot <- as.matrix(bootstrap_data[, c("reject", "Offers")])
  
  cv_lasso_boot <- cv.glmnet(x_boot, y_boot, family = "binomial", alpha=1)
  best_lambda_boot <- cv_lasso_boot$lambda.min
  
  lasso_model_boot <- glmnet(x_boot, y_boot, family = "binomial", alpha=1, lambda=best_lambda_boot)
  
  bootstrap_predictions_new_values[, i] <- predict(lasso_model_boot, newx = new_values, type = "response", s = best_lambda_boot)
}

# Get standard error for each new prediction
prediction_se_new_values <- apply(bootstrap_predictions_new_values, 1, sd)

# Get 95% confidence intervals for each new prediction
prediction_ci_lower_new_values <- apply(bootstrap_predictions_new_values, 1, function(x) quantile(x, 0.025))
prediction_ci_upper_new_values <- apply(bootstrap_predictions_new_values, 1, function(x) quantile(x, 0.975))

# Combine into a data frame
predicted_values_df <- data.frame(
  `Years Since PhD` = new_values[, 1],
  `First Author Pubs` = new_values[, 2],
  `Classes Taught` = new_values[, 3],
  `gender_bi` = new_values[, 4],
  Predicted_Probability = predictions_for_new_values,
  Prediction_SE = prediction_se_new_values,
  CI_Lower = prediction_ci_lower_new_values,
  CI_Upper = prediction_ci_upper_new_values
)




#######################################################
####### caret
#######################################################

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