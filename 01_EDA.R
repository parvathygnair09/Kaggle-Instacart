##========================================
## Project : Instacart Market Basket Analysis
## Source  : Kaggle
## Title   : Data Pre-processing
## Created : August 18, 2017
## Modified: August 23, 2017
## Authors : Parvathy & Raghuprasad
##========================================

## Citation : “The Instacart Online Grocery Shopping Dataset 2017”, Accessed from https://www.instacart.com/datasets/grocery-shopping-2017 on August 18, 2017 from Kaggle.

## Objective : Use data for exploratory analysis, test models for predicting products that a user will buy again, try for the first time etc.

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
# lapply(filesList, readFiles)

# Saving the environment to avoid reading the files everytime
# save.image("./initialRead.RData")
# load("./initialRead.RData")

##===================
## Data Summary
##===================

load("currentSession.RData")

## 3 million instacart orders

## Product/aisle/department information
head(aisles) ## aisle id, aisle
head(departments)  ## department id, department
head(products)  ## product id, product name, aisle id, dept. id

## Actual orders - base dataset
head(orders) ## order id, user id, order #, dow, hour, days since last order, eval_set
table(orders$eval_set)
## users, identified by user_id in the orders csv, make orders which are identified by order_id
## 3.2M - prior, 75K test, 131K - train => orders 
## unique orders - user ids not unique

head(order_products__prior) ## previous orders for each customer (from both test and train) - prior eval set
head(order_products__train) ## previous orders for each customer - train eval set

##===========================
## Exploratory Data Analysis
##============================

orders.mod <- orders %>% 
  mutate(orders.weekday = ifelse(order_dow == 0, "Sunday",
                                 ifelse(order_dow == 1, "Monday",
                                        ifelse(order_dow == 2, "Tuesday",
                                               ifelse(order_dow == 3, "Wednesday",
                                                      ifelse(order_dow == 4, "Thursday",
                                                             ifelse(order_dow == 5, "Friday", "Saturday"))))))) %>% 
  mutate(orders.weekday = factor(orders.weekday,levels=c('Sunday','Monday','Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'),ordered=TRUE))

## Separate test, train and prior from orders dataset
orders.train <- orders.mod %>% filter(eval_set == "train")
orders.test <- orders.mod %>% filter(eval_set == "test")
orders.prior <- orders.mod %>% filter(eval_set == "prior")

## Note: for each user we may have n-1 prior orders and 1 train order OR n-1 prior orders and 1 test order in which we have to state what products have been reordered

## Analysis with the dataset with orders from the train set only - without considering the prior orders of the users
head(orders.train) 
## 131K unique orders and users

## Combine orders from train set and extract product names, department ids and aisles
orders.combined <- left_join(order_products__train,orders.train, by = "order_id") %>% 
                      left_join(products, by="product_id") %>% 
                          left_join(departments, by = "department_id") %>% 
                              left_join(aisles, by = "aisle_id")

## This dataset has 131,209 unique order_ids and user ids (KEYs) with multiple products in each order

head(orders.combined)
dfSummary(orders.combined, style = "grid", plain.ascii = TRUE)

table(orders.combined$order_dow, orders.combined$orders.weekday)

## 1. Day of the week analysis
## Looking at unique orders in the train data
orders.train %>% 
        mutate(orders.weekday=factor(orders.weekday,levels=rev(levels(orders.combined$orders.weekday)),ordered=T)) %>% 
        group_by(orders.weekday) %>% 
        summarise(tot.orders = n()) %>% 
        ggplot(aes(x = orders.weekday, y = tot.orders, fill = orders.weekday)) +
        geom_bar(stat = "identity", width = 0.5) +
        geom_text(aes(label = tot.orders), hjust = -0.1) +
        labs( x = "Weekday", y = "Number of Orders") +
        labs(title = "Purchase Pattern Day of the Week - Train Data Only") +
        guides(fill = FALSE) +
        coord_flip() +
        theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
        theme(plot.title = element_text(size=10)) +
        scale_y_continuous(label = scales::comma) +
        scale_fill_hue()
## Maximum orders in Sunday, followed by Saturday and Monday. Lowest on Wednesday, Thursday. 

## 2. Hour of the day analysis
## Again looking at the train data only
orders.train %>% 
  mutate(orders.weekday=factor(orders.weekday,levels=rev(levels(orders.combined$orders.weekday)),ordered=T)) %>% 
  group_by(orders.weekday, order_hour_of_day) %>% 
  summarise(tot.orders = n()) %>% group_by(orders.weekday) %>% mutate(pctOrder=100*tot.orders/sum(tot.orders)) %>% 
  ggplot(aes(x = order_hour_of_day, y = orders.weekday, fill = pctOrder)) + 
  geom_raster() +
  scale_fill_gradientn(colours = c("white", "lightgreen", "green", "darkgreen")) +
  labs( x = "Hour of the Day", y = "Day of the Week") +
  labs(title = "Purchase Pattern by Day of the Week and Hour") +
  labs(fill = "% of Daily Orders") 
## Order numbers peak during Sunday 10 - 4, with maximum orders during 2PM. 
  

## 3. Hour of the day analysis
## The following analyses use the combined train datset with info on products, departments and 
orders.combined %>% 
        mutate(orders.weekday=factor(orders.weekday,levels=rev(levels(orders.combined$orders.weekday)),ordered=T)) %>% 
        group_by(orders.weekday, order_hour_of_day) %>% 
        summarise(tot.orders = n()) %>% 
        ggplot(aes(x = order_hour_of_day,y = tot.orders, color = orders.weekday, group = orders.weekday)) + 
        geom_line(size = 1.2) +
        labs( x = "Hour of the Day", y = "Number of Orders") +
        labs(title = "Purchase Pattern by Day of the Week and Hour") +
        theme(legend.position=c(0.9, 0.8)) +
        theme(legend.background=element_blank()) + 
        theme(legend.key=element_blank()) +
        labs(color = "Day of the Week") +
        guides(color = guide_legend(reverse = TRUE)) +
        scale_color_brewer(palette = "Dark2")


## 4. Product Department Analysis
orders.combined %>% 
        group_by(department) %>% 
        summarise(tot.orders = n()) %>% 
        ggplot(aes(area = tot.orders, fill = tot.orders, label = department, subgroup = department)) +
        treemapify::geom_treemap() +
        geom_treemap_subgroup_border(color = "grey", size = 0.5) +
        geom_treemap_text(
                #fontface = "italic",
                colour = "black",
                place = "centre",
                grow = FALSE
        ) +
        labs(
                title = "Orders by Product Departments",
                caption = "The area and gradient of each department is proportional to the number of orders placed"
        ) + 
        labs(fill = "# of Orders") +
        scale_fill_gradient2(low=muted("red"), mid="white", high=muted("lightcyan2"),
                             midpoint=110, label = scales::comma) 
## Most ordered is from produce department followed by dairy & eggs, snacks and beverages.

## Treemap package: Installed from https://github.com/wilkox/treemapify


## 5. Product Aisle Analysis - Most re-ordered products
orders.combined %>%
  group_by(aisle) %>% 
  summarise(tot.orders = n()) %>% 
  left_join(
    orders.combined %>% 
      filter(reordered == 1) %>% 
      group_by(aisle, reordered) %>% 
      summarise(re.tot.orders = n()) , by = "aisle") %>% ungroup() %>% 
  mutate(pctreOrder = 100 * (re.tot.orders/tot.orders)) %>% 
  arrange(desc(tot.orders)) %>% top_n(25, wt = tot.orders) %>% 
  ggplot(aes(x = tot.orders/1000, y = pctreOrder, color = aisle)) +
  geom_point() +
  geom_text(aes(x = (tot.orders/1000), label = aisle), size = 4, hjust = 0, vjust = 1.2) +
  guides(color = FALSE) + 
  labs(title = "Top Re-ordered Products") +
  labs( x = "Number of Orders (in Thousands)", y = "Reorders as % of # Orders") +
  xlim(0, 170)
## Fresh fruits and vegetable are the among the most re-ordered items.  

## 6. Same analysis as above with word cloud
## Word Cloud
aisle.freq <- as.data.frame(orders.combined) %>%
  group_by(aisle) %>% 
  summarise(tot.orders = n()) %>% 
  rename(word = aisle, freq = tot.orders) 

## Most ordered from aisles
wordcloud2::wordcloud2(aisle.freq %>% 
                         arrange(desc(freq)) %>% top_n(50, wt = freq)
                       %>% mutate(freq=(freq)^0.7), size = 0.5, maxRotation = 0, minRotation = 0, hoverFunction = NULL)


## Least ordered from aisles
wordcloud2::wordcloud2(aisle.freq %>% 
                         arrange(desc(freq)) %>% top_n(-10, wt = freq)
                       %>% mutate(freq=(freq)^0.7), size = 0.5, maxRotation = 0, minRotation = 0, hoverFunction = NULL)
## Reference: https://cran.r-project.org/web/packages/hunspell/vignettes/intro.html


## 7. Product Analysis
## Top 25 most ordered products
wordcloud2::wordcloud2(orders.combined %>% 
                         group_by(product_name) %>% 
                         summarise(totOrders = n()) %>% 
                         rename(word = product_name, freq = totOrders) %>% 
                         arrange(desc(freq)) %>% top_n(100, wt = freq) %>% mutate(freq=(freq)^0.7), size = 0.5, maxRotation = 0, minRotation = 0, hoverFunction = NULL)
## Mostly veggies and fruits


## Least ordered products
wordcloud2::wordcloud2(orders.combined %>% 
                         group_by(product_name) %>% 
                         summarise(totOrders = n()) %>% 
                         rename(word = product_name, freq = totOrders) %>% filter(freq <= 5) %>% 
                         arrange(freq) %>% top_n(-50, wt = freq) %>% mutate(freq=(freq)^0.2), size = 0.2, maxRotation = 0, minRotation = 0, hoverFunction = NULL)
## Likely specialty products/ specific brands


## Next steps : Merge datasets at user level ( train + prior orders), trace pattens on frequency of orders, average basket size etc. 