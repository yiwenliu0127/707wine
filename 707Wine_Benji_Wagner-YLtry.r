
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gbm)
library(randomForest)

install.packages("readr")
install.packages("tidyr")
install.packages("gbm")
install.packages("ggplot2")
install.packages("randomForest")

sample <- read_csv("Data/wine-reviews/cleanwine.csv")

sample %>% head

sample %>% group_by(country, taster_name) %>% summarize("Average_Price" = mean(price, na.rm = T), 
                                                                                 "Count" = n()) %>% 
filter(is.na(Average_Price))

sample %>% filter(is.na(price)) %>% group_by(country, taster_name) %>% summarize("Count" = n()) 

sample %>% filter(price < 500) %>% ggplot(aes(x = price)) + geom_histogram(bins = 100)

sample %>% group_by(variety) %>% summarize("Average_Price" = mean(price, na.rm = T), 
                                                                                 "Count" = n()) %>% 
filter(is.na(Average_Price)) %>% select(variety) %>% unlist() -> drop_variety

sample %>% filter(!(variety %in% drop_variety)) -> sample2

impute_mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))

sample2 %>% group_by(variety) %>% mutate(price = impute_mean(price)) %>% select(-description) -> cleanwine_price

cleanwine_price %>% head

cleanwine_price$country <- factor(cleanwine_price$country)
cleanwine_price$province <- factor(cleanwine_price$province)
cleanwine_price$taster_name <- factor(cleanwine_price$taster_name)
cleanwine_price$title <- factor(cleanwine_price$title)
cleanwine_price$variety <- factor(cleanwine_price$variety)
cleanwine_price$winery <- factor(cleanwine_price$winery)

gbmFit <- gbm(formula = price ~ ., data = cleanwine_price %>% select(-title, -winery), 
              n.trees = 1000, shrinkage = 0.01, interaction.depth = 2, cv.folds = 10, 
              distribution = "gaussian")

best_iter <- gbm.perf(gbmFit, method = "cv")

# Performance on whole dataset

diff_squared <- (data$price - 
    predict(gbmFit, newdata = cleanwine_price$price, n.trees = best_iter))^2

mean(diff_squared, na.rm = TRUE)
