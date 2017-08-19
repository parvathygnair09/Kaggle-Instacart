## Kaggle
## Instacart Market Basket Analysis
## Data Pre-processing
## Created: August 18, 2017
## Modified: August 19, 2017
##========================================

rm(list = ls(all = TRUE))

library(dplyr)

## Read Files
readFiles <- function(fileName){
        print(fileName)
        assign(fileName,read.csv(unz(paste0(fileName, ".csv.zip"), paste0(fileName, ".csv"))),envir=globalenv())
}

## Function call to read
filesList <- c("aisles", "departments", "order_products__prior", 
               "order_products__train", "orders", "products")
lapply(filesList, readFiles)

## Save as R datasets
save(aisles, file = "aisles.RData")
save(departments, file = "departments.RData")
save(orders, file = "orders.RData")
save(products, file = "products.RData")

## Data Summary
## 3 million instacart orders
head(aisles) ## aisle id, aisle
head(departments)  ## department id, department
head(orders) ## order id, user id, order #, dow, hour, days since last order, eval_set
table(orders$eval_set)
## 3.2M - prior, 75K test, 131K - train

head(products)  ## product id, product name, aisle id, dept. id

head(order_products__prior) ## previous orders for each customer??
head(order_products__train) ## ??

## Separate test, train and prior from orders dataset
orders.train <- orders %>% filter(eval_set == "train")
orders.test <- orders %>% filter(eval_set == "test")
orders.prior <- orders %>% filter(eval_set == "prior")

## Exploratory Data Analysis


