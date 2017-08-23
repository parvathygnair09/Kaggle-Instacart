##========================================
## Project : Instacart Market Basket Analysis
## Source  : Kaggle
## Title   : Data Pre-processing
## Created : August 18, 2017
## Modified: August 22, 2017
## Authors : Parvathy & Raghuprasad
##========================================

rm(list = ls(all = TRUE))

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

## 1. Day of the week analysis
orders.combined %>% 
        mutate(orders.weekday=factor(orders.weekday,levels=rev(levels(orders.combined$orders.weekday)),ordered=T)) %>% 
        ggplot(aes(x = orders.weekday, fill = orders.weekday)) +
        geom_bar(width = 0.5) +
        labs( x = "Weekday", y = "Number of Orders") +
        labs(title = "Purchase Pattern Day of the Week") +
        guides(fill = FALSE) + 
        coord_flip() +
        theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
        theme(plot.title = element_text(size=10)) +
        scale_y_continuous(label = scales::comma) +
        scale_fill_hue()
## Maximum orders in Sunday, followed by Saturday and Monday. Lowest on Wednesday, Thursday. 

## 2. Hour of the day analysis
orders.combined %>% 
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
  mutate(pctOrder = 100 * (re.tot.orders/tot.orders)) %>% filter(pctOrder >= 1) %>% 
  ggplot(aes(x = tot.orders, y = pctOrder, color = aisle)) +
  geom_point(size = 0.1) +
  geom_text(aes(label = aisle), size = 4) +
  guides(color = FALSE) + 
  labs(title = "Top Re-ordered Products") +
  labs( x = "Number of Orders (in thousands)", y = "Reorders as % of Total Orders ") 
## Fresh fruits and vegetable are the among the most re-ordered items.  

## Same analysis as above with word cloud
## Word Cloud
aisle.freq <- as.data.frame(orders.combined) %>%
  group_by(aisle) %>% 
  summarise(tot.orders = n()) %>% 
  rename(word = aisle, freq = tot.orders)

wordcloud2::wordcloud2(aisle.freq, size = 0.6)
## Reference: https://cran.r-project.org/web/packages/hunspell/vignettes/intro.html