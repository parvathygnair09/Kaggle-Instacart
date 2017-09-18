##========================================
## Project : Instacart Market Basket Analysis
## Source  : Kaggle
## Title   : Data Pre-processing
## Created : September 13, 2017
## Modified: September 13, 2017
## Authors : Parvathy & Raghuprasad
##========================================

rm(list = ls(all = TRUE))
##=====================
## Read Files
##=====================

##       readFiles <- function(fileName){
##         print(fileName)
##         assign(fileName,read.csv(unz(paste0(fileName, ".csv.zip"), paste0(fileName, ".csv"))),envir=globalenv())
##       }
##       
##       ## Function call to read
##       filesList <- c("order_products__prior", "order_products__train", "orders")
##       lapply(filesList, readFiles)
##       
##       ## Separate test, train and prior from orders dataset
##       orders.train <- orders %>% filter(eval_set == "train")
##       temp <- unique(orders.train$user_id)
##       set.seed(123)
##       temp <- temp[runif(length(temp))<0.1]
##       orders.train <- orders.train %>% filter(user_id %in% temp)
##       orders.prior <- orders %>% filter(eval_set == "prior") %>% filter(user_id %in% temp)
##       
##       order_products__prior <- order_products__prior %>% filter(order_id %in% orders.prior$order_id)
##       order_products__train <- order_products__train %>% filter(order_id %in% orders.train$order_id)
##       rm(list=c('orders','temp','readFiles','filesList'))
##       save.image("~/parvathy/Kaggle-Instacart/dataForAnalysis.RData")
load("~/parvathy/Kaggle-Instacart/dataForAnalysis2.RData")

## Objective : Create predictive models, add new features to the existing number of orders variable

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


## 1. Data preparation for modeling
#===================================
## Combine orders in train data with products train. We will be using the training data itself to create a validation set since the test data provided does not have a label to validate predictions.
## Get the list of users to train
users.train <- unique(orders.train$user_id)

## Append details
order_products__prior <- left_join(orders.prior,order_products__prior, by = "order_id") %>% 
  arrange(user_id, order_id, order_number)

order_products__train <- left_join(orders.train,order_products__train, by = "order_id") %>% 
  arrange(user_id, order_id, order_number)


## Run the following 3 sets together in sequence each time
## Find the average days since a product has been reordered.
days_between_orders <- order_products__prior %>% 
  filter(add_to_cart_order == 1) %>% 
  select(order_id,user_id,order_number,days_since_prior_order) %>% 
  arrange(user_id,order_number) %>%   # find days since last order
  group_by(user_id) %>% 
  arrange(user_id, order_number) %>% 
  mutate(rev.days_since_prior_order = ifelse(is.na(days_since_prior_order), 0, days_since_prior_order)) %>% 
           mutate(days_FromFirstOrder = cumsum(rev.days_since_prior_order)) %>% 
  select(user_id, order_number, rev.days_since_prior_order, days_FromFirstOrder)

## merge the new column to all prior orders data
days_between_orders <- left_join(order_products__prior, days_between_orders, by = c("user_id", "order_number")) %>% 
  group_by(user_id, product_id) %>% 
  arrange(user_id, order_number) %>% 
  mutate(interOrder.time = days_FromFirstOrder - lag(days_FromFirstOrder))



## Compute the periodicity for each product from the days between orders for each product
product.periodicity <- days_between_orders %>% 
  group_by(product_id) %>% 
  summarise(prod.periodicity = mean(interOrder.time, na.rm = TRUE))

forPeriodicty <- days_between_orders %>% group_by(user_id) %>% mutate(days_LastPriorOrder=max(days_FromFirstOrder)) %>% 
  group_by(user_id,product_id) %>% 
  summarise(days_LastTimeProductOrdered = max(days_LastPriorOrder - days_FromFirstOrder)) %>% 
  ungroup  %>% left_join(product.periodicity,by='product_id')




## Find the list of unique products and the number of times it has been ordered by the user.

priors.count <- order_products__prior %>% 
  group_by(user_id, product_id) %>% 
  summarise(no.orders = n()) 
  

#  for(iloop in 1:nrow(most_ordered)){
#    priors.count[[paste0('avg_reorder.days',most_ordered$product_id[iloop])]] <- with(priors.count, avg_reorder.days * 1*(product_id==most_ordered$product_id[iloop]) )
#  }
#  priors.count %>% head


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
                          order_products__train %>% filter(user_id %in% train_users_run$value) %>% mutate(selected = 1) %>% select(user_id, product_id, selected), 
                          by = c("user_id", "product_id")) %>% 
    mutate(selected = ifelse(is.na(selected), 0, selected)) %>% 
    left_join(forPeriodicty,by = c("user_id", "product_id")) %>% 
    left_join(order_products__train %>% select(user_id,days_since_prior_order) %>% unique,by='user_id') %>% 
    mutate(daysSinceLastPdtOrdOnTrnOrdDay=days_LastTimeProductOrdered+days_since_prior_order) %>%
    mutate(prod.periodicity=ifelse(is.na(prod.periodicity),1,prod.periodicity)) %>% 
    mutate(prod.periodicity=ifelse(prod.periodicity<7,7,prod.periodicity)) %>% 
    mutate(attractivenessOnPurchaseDay=ifelse(daysSinceLastPdtOrdOnTrnOrdDay<prod.periodicity,
                                              daysSinceLastPdtOrdOnTrnOrdDay/prod.periodicity,
                                              1-((daysSinceLastPdtOrdOnTrnOrdDay-prod.periodicity)/3/prod.periodicity)  )
           )

  test.data <- left_join(data.table(priors.count)[user_id %in% test_users], 
                          order_products__train %>% filter(user_id %in% test_users) %>% mutate(selected = 1) %>% select(user_id, product_id, selected), 
                          by = c("user_id", "product_id")) %>% 
    mutate(selected = ifelse(is.na(selected), 0, selected)) %>% 
    left_join(forPeriodicty,by = c("user_id", "product_id")) %>% 
    left_join(order_products__train %>% select(user_id,days_since_prior_order) %>% unique,by='user_id') %>% 
    mutate(daysSinceLastPdtOrdOnTrnOrdDay=days_LastTimeProductOrdered+days_since_prior_order) %>%
    mutate(prod.periodicity=ifelse(is.na(prod.periodicity),1,prod.periodicity)) %>% 
    mutate(prod.periodicity=ifelse(prod.periodicity<7,7,prod.periodicity)) %>% 
    mutate(attractivenessOnPurchaseDay=ifelse(daysSinceLastPdtOrdOnTrnOrdDay<prod.periodicity,
                                              daysSinceLastPdtOrdOnTrnOrdDay/prod.periodicity,
                                              1-((daysSinceLastPdtOrdOnTrnOrdDay-prod.periodicity)/3/prod.periodicity)  )
    )
  
  train.data %>% head(50)
  train.data <- train.data %>% select(selected,attractivenessOnPurchaseDay,no.orders)
  
  ## Model 1. Logistic model
  #=========================
  ## This model uses a single feature - number of prior orders for a product to determine if it will be purchased      or not.
  glm.fit <- glm(selected ~ ., data = train.data, family = binomial)
  glm.fit
  rslt <<- glm.fit
  
  glm.prob <- predict(glm.fit, test.data, type = "response")
  glm.pred <- rep(0, length(glm.prob))
  glm.pred[glm.prob > 0.9]  <- 1
  
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


output <- NULL
getSampleRuns(0.001)
getSampleRuns(0.005)
getSampleRuns(0.01)
getSampleRuns(0.05)
getSampleRuns(0.10)
getSampleRuns(0.50)
output
rslt
