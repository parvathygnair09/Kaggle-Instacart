## Kaggle
## Instacart Market Basket Analysis
## Data Pre-processing
## August 18, 2017
##========================================

rm(list = ls(all = TRUE))

readFiles <- function(fileName){
        print(fileName)
        assign(fileName,read.csv(unz(paste0(fileName, ".csv.zip"), paste0(fileName, ".csv"))),envir=globalenv())
}

readFiles("aisles")
head(data)
