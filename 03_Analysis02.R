##========================================
## Project : Instacart Market Basket Analysis
## Source  : Kaggle
## Title   : Data Pre-processing
## Created : September 13, 2017
## Modified: September 13, 2017
## Authors : Parvathy & Raghuprasad
##========================================

## Objective : Create predictive models, add new features to the existing number of orders variable

rm(list = ls(all = TRUE))

## Load necessary libraries
library(dplyr)
library(summarytools)
library(ggplot2)
library(scales)
library(vcd)
library(forcats)
library(devtools)
library(treemapify)
library(wordcloud2)
library(data.table)

load("currentSession.RData")

## 1. Data preparation for modeling
#=======================================================
## Combine orders in train data with products train. We will be using the training data itself to create a validation set since the test data provided does not have a label to validate predictions.
## Get the list of users to train
users.train <- unique(orders.train$user_id)

## Get all prior order details for the train users
orders.prior.train <- left_join(orders.prior,order_products__prior, by = "order_id") %>% 
  arrange(user_id, order_id, order_number) %>% 
  filter(user_id %in% users.train)

print(orders.prior.train %>% filter(user_id == 1 & product_id == 196))
## product 196 was bought 10 times by user 1. Assuming days since prior order to be same as for the particular product  as it is for the order overall - calculate the average days since a particular product was purchased.

## Get all products in train orders
orders.train.all <- left_join(orders.train,order_products__train, by = "order_id") %>% 
  arrange(user_id, order_id, order_number)

# users123 <- filter(orders.prior.products, user_id %in% c(1,2,3))
# write.csv(users123, "users123.csv")

## Find the list of unique products and the number of times it has been ordered by the user.
## Find the average days since a product has been reordered.
priors.count <- orders.prior.train %>% 
  group_by(user_id, product_id) %>% 
  summarise(avg_reorder.days = mean(days_since_prior_order, na.rm = TRUE), no.orders = n()) 

head(priors.count)

## Split this data to create test and train (95% train, 5% test)
split.criteria <- unique(priors.count$user_id)
split <- caTools::sample.split(split.criteria, SplitRatio = 0.95)

train_users <- subset(split.criteria, split == TRUE) 
test_users <- subset(split.criteria, split == FALSE)

output <- NULL

getSampleRuns <- function(samp_size) {
  sample_train <- samp_size
  train_users_run <- train_users %>% tbl_df() %>% sample_frac(sample_train)
  
  ## Data for analysis
  #=======================
  ## the analysis data tries to learn a logistic regression model from the prior orders on the user predicting the      train order.
  train.data <- left_join(data.table(priors.count)[user_id %in% train_users_run$value], 
                          orders.train.all %>% filter(user_id %in% train_users_run$value) %>% 
                            mutate(selected = 1) %>% 
                            select(user_id, product_id, selected), by = c("user_id", "product_id")) %>% 
    mutate(selected = ifelse(is.na(selected), 0, selected))
  
  test.data <- left_join(data.table(priors.count)[user_id %in% test_users], 
                         orders.train.all %>% filter(user_id %in% test_users) %>% 
                           mutate(selected = 1) %>% 
                           select(user_id, product_id, selected), by = c("user_id", "product_id")) %>% 
    mutate(selected = ifelse(is.na(selected), 0, selected))
  
  ## Model 1. Logistic model
  #=========================
  ## This model uses a single feature - number of prior orders for a product to determine if it will be purchased      or not.
  glm.fit <- glm(selected ~ no.orders + avg_reorder.days, data = train.data, family = binomial)
  glm.fit
  
  glm.prob <- predict(glm.fit, test.data, type = "response")
  glm.pred <- rep(0, length(glm.prob))
  glm.pred[glm.prob > 0.5]  <- 1
  
  ## For Probabilistic Prediction
  # glm.pred = (runif(length(glm.prob)) <= glm.prob) * 1
  
  table(glm.pred, test.data$selected)
  
  ## Prediction Accuracy
  acc <- mean(test.data$selected == glm.pred)
  
  ## Test Error
  err <- 1 - acc
  
  ## Precision
  precsn <- mean(test.data$selected[glm.pred == 1] == 1)
  
  ## Recall
  recall <- mean(glm.pred[test.data$selected== 1] == 1)
  
  temp <- cbind(sample = nrow(train_users_run), acc, err, precsn, recall)
  
  output <<- as.data.frame(rbind(output, temp))
}


getSampleRuns(0.0001)
getSampleRuns(0.0005)
getSampleRuns(0.001)
getSampleRuns(0.005)
getSampleRuns(0.01)
getSampleRuns(0.05)
getSampleRuns(0.10)
getSampleRuns(0.05)
output

