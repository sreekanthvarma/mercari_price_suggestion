## Loading libraries
library(dplyr)
library(tidyr)
library(ggplot2)

## Loading datasets 
train <- read.csv("train.tsv", sep = '\t')

test <- read.csv("test.tsv", sep = '\t')

target <- train[, c("price")]
target <- data.frame(target)

## Renaming IDs on both the train and test datasets to maintain uniformity
colnames(train)[1] <- "Id"
colnames(test)[1] <- "Id"
train <- train[,-c(6)]
data <- rbind(train, test)

## Since the categories, brands and descriptions are in different cases, it is easier to analyse them by converting everything
## to either upper or lower case
data$category_name <- toupper(data$category_name)
data$brand_name <- toupper(data$brand_name)
data$item_description <- toupper(data$item_description)

## category_name column
## counting number of subcategories present and finding the maximum number of sub categories among all products
n1 <- as.character(data$category_name)

n2 <- gsub("\\/", "", n1)

# difference gives the number of / replaced by empty string. Adding 1 to that number gives the number of sub categories
num <- nchar(n1) - nchar(n2)

max(num)+1

# we have max categories of 5. so we divide the category_name to 5 separate columns

data <- data %>% separate(category_name, into = c("category1", "category2", "category3","category4","category5"), sep = '\\/')

colSums(is.na(data))/nrow(data)

## We can see that category4 and catogory5 have 99.7% and 99.8% values missing and so we can safely ignore these columns
data$category4 <- NULL
data$category5 <- NULL


## Replacing empty strings in Brand with "UNBRANDED"

data$brand_name <- sub("^$", "UNBRANDED", data$brand_name)

## Filtering out all the 3 categories by selecting few important from these categories
cat1 <- data %>% 
  group_by(category1) %>% 
  summarize(num = n()) %>%
  arrange(desc(num))

cat2 <- data %>% 
  group_by(category2) %>% 
  summarize(num = n()) %>%
  arrange(desc(num))

cat3 <- data %>% 
  group_by(category3) %>% 
  summarize(num = n()) %>%
  arrange(desc(num))


summary(target)


ggplot(target, aes(x = target)) +
  geom_bar()

train_temp <- data[1:nrow(train), ]
temp <- data.frame(target, train_temp)

## Checking out the average price per category in both category 1 and 2.
stats_cat1 <- temp %>%
  group_by(category1) %>%
  summarize(avg_price = mean(target), count = n())

stats_cat2 <- temp %>%
  group_by(category1, category2) %>%
  summarize(avg_price = mean(target), count = n()) %>%
  filter(count > 500)


### Grouping according to average price of categories
## Group1 - 0-$10
## Group2 - $10-$15
## Group3 - $15-$20
## Group4 - $20-$30
## Group5 - $30-$45
## Group6 - $45-$65
## Group7 - $65-$100


data$group1 <- ifelse(grepl("HANDMADE", data$category1) | grepl("BEAUTY", data$category1) | grepl("KIDS", data$category1) | 
                        grepl("OTHER", data$category1) |grepl("MAGAZINES", data$category2) | grepl("PAPER GOODS", data$category2) |
                        grepl("JEWELRY", data$category2) | grepl("CHILDREN", data$category1) | grepl("TOYS", data$category2) | 
                        grepl("TRADING CARDS", data$category2) | grepl("MEDIA", data$category2), 1, 0)

data$group2 <- ifelse(grepl("HANDMADE", data$category1) | grepl("BEAUTY", data$category1) | grepl("KIDS", data$category1) | 
                        grepl("OTHER", data$category1) | grepl("TRADING CARDS", data$category2) | grepl("MEDIA", data$category2) | 
                        grepl("GIRLS 0-24 MOS", data$category2) | grepl("ART", data$category2) | grepl("ARTWORK", data$category2) | 
                        grepl("BOOKS", data$category2), 1, 0)

data$group3 <- ifelse(grepl("HANDMADE", data$category1) | grepl("BEAUTY", data$category1) | grepl("KIDS", data$category1) | 
                        grepl("OTHER", data$category1) | grepl("BOOKS", data$category2) | grepl("APPAREL", data$category2) | 
                        grepl("OFFICE SUPPLIES", data$category2) | grepl("ACCESSORIES", data$category2) | 
                        grepl("BOYS 0-24 MOS", data$category2) | grepl("FAN SHOP", data$category2) | 
                        grepl("GIRLS 2T-5T", data$category2) | grepl("TOPS & BLOUSES", data$category2) | 
                        grepl("UNDERWEAR", data$category2) | grepl("GIRLS (4+)", data$category2) | grepl("MAKEUP", data$category2) |
                        grepl("BOYS 2T-5T", data$category2) | grepl("HAIR CARE", data$category2) | grepl("TOPS", data$category2) | 
                        grepl("PANTS", data$category2), 1, 0)

data$group4 <- ifelse(grepl("SPORTS & OUTDOORS", data$category1) | grepl("HOME", data$category1) | 
                        grepl("VINTAGE & COLLECTIBLES", data$category1) | grepl("WOMEN", data$category1) | 
                        grepl("TOOLS & ACCESSORIES", data$category2) | grepl("SKIN CARE", data$category2) |
                        grepl("TOYS", data$category1) | grepl("SKIRTS", data$category1) | 
                        grepl("ANTIQUE", data$category2) | grepl("ATHLETIC APPAREL", data$category2) |
                        grepl("FRAGRANCE", data$category2) | grepl("DAILY & TRAVEL ITEMS", data$category2) |
                        grepl("COLLECTIBLES", data$category2) | grepl("JEANS", data$category2) | grepl("SWEATERS", data$category2) |
                        grepl("SWEATS & HOODIES", data$category2) | grepl("BOYS (4+)", data$category2) |
                        grepl("JEWELRY", data$category2) | grepl("ATHLETIC APPAREL", data$category2) |
                        grepl("KITCHEN & DINING", data$category2) | grepl("DRESSES", data$category2) |
                        grepl("CELL PHONES & ACCESSORIES", data$category2) | grepl("EXERCISE", data$category2) |
                        grepl("HOME DÃ©COR", data$ category2) | grepl("SWIMWEAR", data$category2), 1, 0)

data$group5 <- ifelse(grepl("MEN", data$category1) | grepl("ELECTRONICS", data$category1) |
                        grepl("SPORTS & OUTDOORS", data$category1) | grepl("HOME", data$category1) | 
                        grepl("VINTAGE & COLLECTIBLES", data$category1) | grepl("WOMEN", data$category1) |
                        grepl("MEN'S ACCESSORIES", data$category2) | grepl("COATS & JACKETS", data$category2) |
                        grepl("SHOES", data$category2) | grepl("TV, AUDIO & SURVEILLANCE", data$category2) |
                        grepl("WOMEN'S ACCESSORIES", data$category2) | grepl("VIDEO GAMES & CONSOLES", data$category2), 1, 0)

data$group6 <- ifelse(grepl("MEN", data$category1) | grepl("ELECTRONICS", data$category1) |
                        grepl("SPORTS & OUTDOORS", data$category1) | grepl("HOME", data$category1) | 
                        grepl("VINTAGE & COLLECTIBLES", data$category1) | grepl("WOMEN", data$category1) |
                        grepl("WOMEN'S HANDBAGS", data$category2) | grepl("SHOES", data$category2), 1, 0)

data$group7 <- ifelse(grepl("MEN", data$category1) | grepl("ELECTRONICS", data$category1) | 
                        grepl("CAMERAS & PHOTOGRAPHY", data$category2)| grepl("COMPUTERS & TABLETS", data$category2) |
                        grepl("BAGS AND PURSES", data$category2), 1, 0)


## Calculating the average price per brand and grouping the brands based on their average price

brand <- temp %>%
  group_by(brand_name) %>%
  summarize(avg_price = mean(target), num = n()) %>%
  arrange(desc(avg_price)) %>%
  filter(num > 1000)


### Grouping according to average price of brands
## Brand1 - 0-$20
## Brand2 - $20-$30
## Brand3 - $30-$40
## Brand4 - $40-$50
## Brand5 - $50-$65
## Brand6 - $65-$100
## Brand7 - $100-$200


data$brand1 <- ifelse(grepl("NYX", data$brand_name) | grepl("OLD NAVY", data$brand_name) | grepl("FOREVER 21", data$brand_name) | 
                        grepl("HOT TOPIC", data$brand_name) | grepl("MAGAZINES", data$brand_name) | 
                        grepl("CARTER'S", data$brand_name) | grepl("H&M", data$brand_name) | 
                        grepl("BATH & BODY WORKS", data$brand_name) | grepl("HOLLISTER", data$brand_name) | 
                        grepl("AMERICAN EAGLE", data$brand_name) | grepl("DISNEY", data$brand_name), 1, 0)

data$brand2 <- ifelse(grepl("UNBRANDED", data$brand_name) | grepl("SEPHORA", data$brand_name) | grepl("TARTE", data$brand_name) | 
                        grepl("TOO FACED", data$brand_name) | grepl("URBAN DECAY", data$brand_name) | 
                        grepl("MAC", data$brand_name) | grepl("VICTORIA'S SECRET", data$brand_name) | 
                        grepl("PINK", data$brand_name), 1, 0)

data$brand3 <- ifelse(grepl("FUNKO", data$brand_name) | grepl("NIKE", data$brand_name) | grepl("XBOX", data$brand_name) | 
                        grepl("LULAROE", data$brand_name) | grepl("NINTENDO", data$brand_name) | 
                        grepl("SONY", data$brand_name) | grepl("THE NORTH FACE", data$brand_name) | 
                        grepl("INDEPENDENT", data$brand_name) | grepl("RAE DUNN", data$brand_name), 1, 0)

data$brand4 <- ifelse(grepl("COACH", data$brand_name) | grepl("LILLY PULITZER", data$brand_name) | 
                        grepl("ADIDAS", data$brand_name) | grepl("LULULEMON", data$brand_name), 1, 0)

data$brand5 <- ifelse(grepl("UGG AUSTRALIA", data$brand_name) | grepl("SAMSUNG", data$brand_name) | 
                        grepl("KATE SPADE", data$brand_name) | grepl("MICHAEL KORS", data$brand_name), 1, 0)

data$brand6 <- ifelse(grepl("KENDRA SCOTT", data$brand_name) | grepl("APPLE", data$brand_name) | 
                        grepl("TORY BURCH", data$brand_name) | grepl("AIR JORDAN", data$brand_name), 1, 0)

data$brand7 <- ifelse(grepl("LOUIS VUITTON", data$brand_name), 1, 0)


## Checkpoint1
save1 <- data
## save

## Finding the most used terms in order to group them based on specific characterstics
word_count <- data.frame(table(unlist(strsplit(data$item_description, " ")))) 

data$free_ship <- ifelse(grepl("FREE", data$item_description) | grepl("SHIPPING", data$item_description), 1, 0)
data$positive <- ifelse(grepl("GREAT", data$item_description) | grepl("GOOD", data$item_description) |
                          grepl("PERFECT", data$item_description) | grepl("EXCELLENT", data$item_description) |
                          grepl("SUPER", data$item_description) | grepl("BRAND", data$item_description), 1, 0)
data$negative <- ifelse(grepl("WORN", data$item_description) | grepl("USED", data$item_description) | grepl("NEVER", data$item_description), 1, 0)
data$size <- ifelse((grepl("SMALL", data$item_description)|grepl("MEDIUM", data$item_description)|
                       grepl("LARGE", data$item_description)|grepl("XL", data$item_description)|
                       grepl("XS", data$item_description)|grepl("SIZE", data$item_description)|
                       grepl("INCH", data$item_description)),1,0)
data$color <- ifelse((grepl("RED", data$item_description)|grepl("YELLOW", data$item_description)|
                        grepl("BLUE", data$item_description)|grepl("GREEN", data$item_description)|
                        grepl("WHITE", data$item_description)|grepl("BLACK", data$item_description)|
                        grepl("DULL", data$item_description)|grepl("GREY", data$item_description)|
                        grepl("BROWN", data$item_description)|grepl("PURPLE", data$item_description)|
                        grepl("COLOR", data$item_description)|grepl("COLOUR", data$item_description)),1,0) 
data$leather <- ifelse(grepl("LEATHER", data$item_description),1,0)


## deleting columns which are no more needed
data$name <- NULL
data$item_description <- NULL
data$category1 <- NULL
data$category2 <- NULL
data$category3 <- NULL
data$brand_name <- NULL

## Checkpoint2
save2 <- data
## save

glimpse(data)


## Dividing the complete data into test and train datasets
l <- nrow(train)

train_1 <- data[1:l,]
test_1 <- data[(l+1):nrow(data),]

summary(train_1)

target <- data.frame(target)
colnames(target) <- c("price")

train_1$Id <- NULL
test_1$Id <- NULL


### Model 1 - XGBoost

## Converting the train, test datasets to matrix form which is recommended for XGBoost model
train_xgb <- as.matrix(train_1)
test_xgb <- as.matrix(test_1)

## Standardizing the target price variable using log transformation
target_log_xgb <- as.matrix(log(target$price+1))
summary(target_log_xgb)


library(xgboost)

## After tuning the parameters with several values, these below parameters gave good results with least RMSLE value
boost <- xgboost(data = train_xgb, label = target_log_xgb, max.depth = 18, 
                 eta = 0.1, print_every_n = 20, nthread = 4, nround = 200, 
                 objective = "reg:linear")

prediction <- predict(boost, test_xgb)

imp <- xgb.importance (model = boost)

xgb.plot.importance (importance_matrix = imp[1:20])

pred_xgb <- exp(data.frame(prediction))


## Cross checking the accuracy on train set
train_xgb_pred <- exp(data.frame(prediction = predict(boost, train_xgb)))


summary(train_xgb_pred)

rmsle_xgb <- sqrt((1/nrow(train_xgb_pred))*sum((log(train_xgb_pred$prediction+1) - log(target$price+1))**2))

glimpse(pred_xgb)


##--------------------------------------------------------------
## Linear Regression

target_log_lm <- log(target$price+1)

train_temp1 <- lapply(train_1, as.factor)
train_lm <- data.frame(target_log_lm, train_temp1)
test_lm <- lapply(test_1, as.factor)

glimpse(train_lm)

model_lm <- lm(target_log_lm~., data = train_lm)
summary(model_lm)

pred_lm <- exp(data.frame(predict(model_lm, test_lm)))


## checking rmsle on train set
train_lm_pred <- exp(data.frame(prediction = predict(model_lm, train_lm)))
rmsle_lm <- sqrt((1/nrow(train_lm_pred))*sum((log(train_lm_pred$prediction+1) - log(target$price+1))**2))


##````````````````````````````````````````````````````````````````
## Random Forest

target_log_rf <- log(target$price+1)

train_temp <- lapply(train_1, as.factor)
train_rf <- data.frame(target_log_rf, train_temp)
test_rf <- lapply(test_1, as.factor)

glimpse(train_rf)

library(randomForest)

model_rf <- randomForest(target_log_rf~., data = train_rf, mtry = 7, ntree = 5, importance = TRUE)

pred_rf <- exp(data.frame(predict(model_rf, test_rf)))


## checking rmsle on train set
train_rf_pred <- exp(data.frame(prediction = predict(model_rf, train_rf)))
rmsle_rf <- sqrt((1/nrow(train_rf_pred))*sum((log(train_rf_pred$prediction+1) - log(target$price+1))**2))

glimpse(pred)


### Averaging the prediction from all the three models(XGBoost, RandomForest and Linear Regression)
### Since XGBoost gave a better output compared to others, I assigned a weight of 3 for it and 2 for RF and 1 for Regression
pred <- (3*pred_xgb + 2*pred_rf + pred_lm)/6

output <- data.frame(seq(0, nrow(test_1)-1), pred)
colnames(output) <- c("test_id", "price")

write.csv(output, "prediction_xgb_rf_lm.csv", row.names = FALSE)


## Checking rmsle for train dataset
train_pred <- (2*train_rf_pred + 3*train_xgb_pred + train_lm_pred)/6
rmsle <- sqrt((1/nrow(train_rf_pred))*sum((log(train_pred$prediction+1) - log(target$price+1))**2))


#### RMSLE for the train dataset using an ensemble of these 3 models gave a value of 0.61
#### RMSLE value for the test dataset was even lower at a value of 0.58

#### Scope for improvement 
#### 1. Text mining can be used extensively to find the bigrams/trigrams from the item description and can use it to
#### create new features which might improve the model accuracy.
#### 2. Neural networks can be applied to check the effectiveness of the NN model but since the data is huge, it takes a lot of 
#### to process on normal machines.

##--------------------------------------------------------------------
## Neural Networks

target_log_nn <- log(target$price)
target_log_nn <- ifelse(target_log_nn <= 0, 0, target_log_nn)

train_nn <- data.frame(target_log_nn, train_1)
test_nn <- test_1

glimpse(train_nn)

library(neuralnet)

form <- paste('target_log_nn ~ ', paste(colnames(train_nn)[-1], collapse = " + "))
model_nn <- neuralnet(form , data = train_nn, hidden = 1, linear.output = T)

pred_nn <- exp(data.frame(predict(model_nn, test_nn)))

output <- data.frame(seq(0, nrow(test_1)-1), pred_nn)
colnames(output) <- c("test_id", "price")

write.csv(output, "prediction_nn.csv", row.names = FALSE)

## checking rmsle on train set
train_nn_pred <- exp(data.frame(prediction = predict(model_nn, train_nn)))
rmsle_nn <- sqrt((1/nrow(train_nn_pred))*sum((log(train_nn_pred$prediction+1) - log(target$price+1))**2))


