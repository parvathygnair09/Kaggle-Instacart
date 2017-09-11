##========================================
## Project : Instacart Market Basket Analysis
## Source  : Kaggle
## Title   : Data Pre-processing
## Created : August 23, 2017
## Modified: August 23, 2017
## Authors : Parvathy & Raghuprasad
##========================================

## Objective : Preparing the data for modeling

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

## Data Summary
#=================
## Actual orders - base dataset
head(orders) ## order id, user id, order #, dow, hour, days since last order, eval_set
table(orders$eval_set)
## users, identified by user_id in the orders csv, make orders which are identified by order_id
## 3.2M - prior, 75K test, 131K - train => orders 
## unique orders - user ids not unique

head(order_products__prior) ## previous orders for each customer (from both test and train) - prior eval set
head(order_products__train) ## previous orders for each customer - train eval set

## 1. Combine orders in train data with products train. 
#=======================================================
## Get the list of users to train
users.train <- unique(orders.train$user_id)

## users in test set
users.test <- unique(orders.test$user_id)

## Get all prior order details for the train orders
orders.prior.train <- left_join(orders.prior,order_products__prior, by = "order_id") %>% 
  arrange(user_id, order_id, order_number) %>% 
  filter(user_id %in% users.train)

orders.prior.all.test <- filter(orders.prior.all, user_id %in% c(1, 2, 3))

## Get all products in train orders
orders.train.all <- left_join(orders.train,order_products__train, by = "order_id") %>% 
  arrange(user_id, order_id, order_number)


## Get product names for clarity
orders.prior.products <- left_join(orders.prior.all, products, by = "product_id") 
orders.train.products <- left_join(orders.train.all, products, by = "product_id") 
# users123 <- filter(orders.prior.products, user_id %in% c(1,2,3))
# write.csv(users123, "users123.csv")

orders.prior.all.test <- orders.prior.all
orders.train.all.test <- orders.train.all

# orders.prior.all.test <- filter(orders.prior.all, user_id %in% c(1:200))
# orders.train.all.test <- filter(orders.train.all, user_id %in% c(1:200))

## Find the list of unique products and the number of times it has been ordered by the user.
priors.count <- orders.prior.all.test %>% 
  group_by(user_id, product_id) %>% 
  summarise(no.orders = n())

## Prepare test data
orders.prior.test <- left_join(orders.test,order_products__prior, by = "order_id") %>% 
  arrange(user_id, order_id, order_number) %>% 
  filter(user_id %in% users.test)




## Data for analysis
analz.data <- left_join(priors.count, orders.train.all %>% 
                          mutate(selected = 1) %>% 
                          select(user_id, product_id, selected), by = c("user_id", "product_id")) %>% 
  mutate(selected = ifelse(is.na(selected), 0, selected))

head(analz.data)
print(analz.data %>% 
        filter(user_id %in% c(1:3)), n = 100)

## Model 1. Logistic model
glm.fit <- glm(selected ~ no.orders, data = analz.data, family = binomial)
glm.fit


glm.prob <- predict(glm.fit, type = "response")
glm.pred <- rep(0, length(analz.data$selected))
glm.pred[glm.prob > 0.5]  <- 1

table(glm.pred, analz.data$selected)

## Prediction Accuracy
acc <- mean(analz.data$selected == glm.pred)
# accuracy 90.17%

## Precision
precsn <- mean(analz.data$selected[glm.pred == 1] == 1)
# precision 47.5%

## Recall
recall <- mean(glm.pred[analz.data$selected == 1] == 1)
# Recall 4.7%











#==================================================================
### Preliminary market basket analysis

# trans <- as(split(orders.prior.products[,"product_name"], orders.prior.products[, "order_id"]), "transactions")

trans <- data.table(orders.prior.products)[,list(product_name),by='order_id']
temp <- lapply(unique(orders.prior.products$order_id),function(x) data.table(orders.prior.products)[order_id==x,product_name])
trans <- as(temp,"transactions")


#Run the apriori algorithm
m1 <- apriori(Groceries,parameter = list(support=.007, confidence=.25,minlen=2))

#Check the rules
inspect(m1[1:10])
inspect(sort(m1,by="lift")[1:4])



