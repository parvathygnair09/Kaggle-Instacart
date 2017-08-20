##========================================
## Project : Instacart Market Basket Analysis
## Source  : Kaggle
## Title   : Data Pre-processing
## Created : August 18, 2017
## Modified: August 19, 2017
## Authors : Parvathy & Raghuprasad
##========================================

rm(list = ls(all = TRUE))

library(dplyr)
library(summarytools)
library(ggplot2)
library(scales)
library(vcd)

##=====================
## Read Files
##=====================

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

##===================
## Data Summary
##===================

## 3 million instacart orders
head(aisles) ## aisle id, aisle
head(departments)  ## department id, department
head(orders) ## order id, user id, order #, dow, hour, days since last order, eval_set
table(orders$eval_set)
## 3.2M - prior, 75K test, 131K - train

head(products)  ## product id, product name, aisle id, dept. id
head(order_products__prior) ## previous orders for each customer - prior eval set
head(order_products__train) ## previous orders for each customer - train eval set

## Separate test, train and prior from orders dataset
orders.train <- orders %>% filter(eval_set == "train")
# orders.test <- orders %>% filter(eval_set == "test")
# orders.prior <- orders %>% filter(eval_set == "prior")

##===========================
## Exploratory Data Analysis
##============================

## Look at train data for now
head(orders.train)

## Combine orders from train set and extract product names, department ids and aisles
orders.combined <- left_join(order_products__train,orders.train, by = "order_id") %>% 
                      left_join(products, by="product_id") %>% 
                          left_join(departments, by = "department_id") %>% 
                              left_join(aisles, by = "aisle_id")

head(orders.combined)
dfSummary(orders.combined, style = "grid", plain.ascii = TRUE)

orders.combined <- orders.combined %>%
                           mutate(orders.weekday = ifelse(order_dow == 0, "Sunday",
                                 ifelse(order_dow == 1, "Monday",
                                        ifelse(order_dow == 2, "Tuesday",
                                               ifelse(order_dow == 3, "Wednesday",
                                                      ifelse(order_dow == 4, "Thursday",
                                                             ifelse(order_dow == 5, "Friday", "Saturday"))))))) %>% 
  mutate(orders.weekday = factor(orders.weekday,levels=c('Sunday','Monday','Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'),ordered=TRUE))

table(orders.combined$order_dow, orders.combined$orders.weekday)

## Day of the week analysis
orders.combined %>% 
  ggplot(aes(x = orders.weekday, fill = orders.weekday)) +
  geom_bar(color = "black", width = 1) +
  labs( x = "Weekday", y = "Number of Orders") +
  labs(title = "Purchase Pattern Day of the Week") +
  guides(fill = FALSE) + 
  # coord_flip() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=10)) +
  scale_y_continuous(label = scales::comma) +
  scale_fill_brewer(palette = "pastel1")
## Maximum orders in Sunday, followed by Saturday and Monday. Lowest on Wednesday, Thursday. 

orders.combined %>% 
  group_by(orders.weekday, order_hour_of_day) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = order_hour_of_day, y = count, color = department)) + 
  geom_line()


