## Loading libraries
library(dplyr)
library(tidyr)

## Loading datasets 
train <- read.csv("train.tsv", sep = '\t')

test <- read.csv("test.tsv", sep = '\t')

target <- train[, c("price")]

colnames(train)[1] <- "Id"
colnames(test)[1] <- "Id"
train <- train[,-c(6)]
data <- rbind(train, test)

## Converting text columns to upper characters
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


## Filtering out all the 3 categories by selecting top 10 of each of those categories
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


data$men_top <- ifelse(grepl("MEN", data$category1) | grepl("T-SHIRTS", data$category3) | grepl("TOPS & T-SHIRTS", data$category3) | grepl("SLIM, SKINNY", data$category3) | grepl("HOODED", data$category3), 1, 0)
data$men_bottom <- ifelse(grepl("MEN", data$category1) | grepl("JEANS", data$category2) | grepl("PANTS", data$category2) | grepl("SHORTS", data$category3), 1, 0)
data$men_inner <- ifelse(grepl("MEN", data$category1) | grepl("UNDERWEAR", data$category2), 1, 0)

data$women_top <- ifelse(grepl("WOMEN", data$category1) | grepl("TOPS & BLOUSES", data$category2) | grepl("TOPS", data$category2) | grepl("DRESSES", data$category2) | grepl("T-SHIRTS", data$category3) | grepl("WOMEN", data$category3) | grepl("TOPS & T-SHIRTS", data$category3) | grepl("SLIM, SKINNY", data$category3), 1, 0)
data$women_bottom <- ifelse(grepl("WOMEN", data$category1) | grepl("PANTS, TIGHTS, LEGGINGS", data$category3) | grepl("JEANS", data$category2) | grepl("PANTS", data$category2) | grepl("SKIRTS", data$category2) | grepl("SHORTS", data$category3) | grepl("ABOVE KNEE, MINI", data$category3) |grepl("WOMEN", data$category3), 1, 0)
data$women_inner <- ifelse(grepl("WOMEN", data$category1) | grepl("UNDERWEAR", data$category2) | grepl("BRAS", data$category3) | grepl("TANK, CAMI", data$category3)| grepl("SPORTS BRAS", data$category3) | grepl("PANTIES", data$category3), 1, 0)
data$accessories <- ifelse(grepl("WOMEN'S ACCESSORIES", data$category2) | grepl("WOMEN'S HANDBAGS", data$category2) | grepl("MEN'S ACCESSORIES", data$category2) | grepl("BAGS AND PURSES", data$category2) | grepl("WALLETS", data$category3) | grepl("BRACELETS", data$category3) | grepl("SUNGLASSES", data$category3), 1, 0)
                                  
data$kids_apparel <- ifelse(grepl("KIDS", data$category1) | grepl("GIRLS 2T-5T", data$category2) | grepl("GIRLS 0-24 MOS", data$category2) | grepl("BOYS 0-24 MOS", data$category2) | grepl("GIRLS (4+)", data$category2) | grepl("BOYS (4+)", data$category2) | grepl("BOYS 2T-5T", data$category2), 1, 0)
data$kids_toys <- ifelse(grepl("KIDS", data$category1) | grepl("TOYS", data$category2) | grepl("VIDEO GAMES & CONSOLES", data$category2) | grepl("DOLLS & ACCESSORIES", data$category3) | grepl("ACTION FIGURES & STATUES", data$category3), 1, 0)

data$winterwear <- ifelse(grepl("SWEATERS", data$category2) | grepl("COATS & JACKETS", data$category2) | grepl("SWEATS & HOODIES", data$category2) | grepl("JACKETS", data$category3) | grepl("HOODED", data$category3), 1, 0)

data$electronics <- ifelse(grepl("ELECTRONICS", data$category1) | grepl("COMPUTERS & TABLETS", data$category2), 1, 0)

data$beauty <- ifelse(grepl("BEAUTY", data$category1) | grepl("MAKEUP", data$category2) | grepl("SKIN CARE", data$category2) | grepl("FACE", data$category3) | grepl("LIPS", data$category3) | grepl("EYES", data$category3) | grepl("BATH & BODY", data$category2), 1, 0)

data$home <- ifelse(grepl("HOME", data$category1) | grepl("HOME DÃ©COR", data$category2) | grepl("FRAGRANCE", data$category2) | grepl("KITCHEN & DINING", data$category2) | grepl("SEASONAL DÃ©COR", data$category2) | grepl("BEDDING", data$category2), 1, 0)

data$vintage_collectibles <- ifelse(grepl("VINTAGE & COLLECTIBLES", data$category1), 1, 0)

data$handmade <- ifelse(grepl("HANDMADE", data$category1), 1, 0)

data$sports <- ifelse(grepl("SPORTS & OUTDOORS", data$category1) | grepl("ATHELETIC APPAREL", data$category2) | grepl("SWIMWEAR", data$category2) | grepl("ATHELETIC", data$category3) | grepl("SPORTS BRAS", data$category3), 1, 0)

data$shoes <- ifelse(grepl("SHOES", data$category2) | grepl("SHOES", data$category3) | grepl("BOOTS", data$category3) | grepl("FASHION SNEAKERS", data$category3)| grepl("SANDALS", data$category3), 1, 0)

data$jewelry <- ifelse(grepl("JEWELRY", data$category2) | grepl("NECKLACES", data$category3) | grepl("EARRINGS", data$category3), 1, 0)

data$mobile_accessories <- ifelse(grepl("CELL PHONES & ACCESSORIES", data$category2) | grepl("CASES, COVERS & SKINS", data$category3), 1, 0)

data$games <- ifelse(grepl("GAMES", data$category3) | grepl("VIDEO GAMES & CONSOLES", data$category2), 1, 0)


brand <- data %>%
  group_by(brand_name) %>%
  summarize(num = n()) %>%
  arrange(desc(num))

head(brand, 10)

data$pink <- ifelse(grepl("PINK", data$brand_name), 1, 0)
data$nike <- ifelse(grepl("NIKE", data$brand_name) | grepl("NIKE GOLF", data$brand_name), 1, 0)
data$victoria <- ifelse(grepl("VICTORIA'S SECRET", data$brand_name), 1, 0)
data$apple <- ifelse(grepl("APPLE", data$brand_name) | grepl("MAC", data$brand_name), 1, 0)
data$lularoe <- ifelse(grepl("LULAROE", data$brand_name), 1, 0)
data$nintendo <- ifelse(grepl("NINTENDO", data$brand_name), 1, 0)
data$forever21 <- ifelse(grepl("FOREVER 21", data$brand_name), 1, 0)

## Checkpoint1
temp <- data
## save


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
temp2 <- data
## save

glimpse(data)

l <- nrow(train)

train_1 <- data[1:l,]
test_1 <- data[(l+1):nrow(data),]

summary(train_1)

#train_1 <- cbind(target, train_1)
target <- data.frame(target)
colnames(target) <- c("price")


## -------------------------------------------------
#sam <- floor(0.7*nrow(train_1))
#train_ind <- sample(seq_len(nrow(train_1)), sam)

#train_final <- train_1[train_ind,]
#cross_val <- train_1[-train_ind,]

train_1$Id <- NULL
train_final <- as.matrix(train_1)

test_1$Id <- NULL
test_final <- as.matrix(test_1)


target_log <- as.matrix(log(target$price))

target_log <- ifelse(target_log <= 0, 0, target_log)


library(xgboost)


boost <- xgboost(data = train_final, label = target_log, max.depth = 18, eta = 0.01, print_every_n = 20, nthread = 4, nround = 500, objective = "reg:linear")

prediction <- predict(boost, test_final)

imp <- xgb.importance (model = boost)

xgb.plot.importance (importance_matrix = imp[1:20])

pred <- data.frame(prediction)
pred <- exp(pred)



train_pred <- data.frame(prediction = predict(boost, train_final))
train_pred <- exp(train_pred)


summary(train_pred)

rmsle <- sqrt((1/nrow(train_pred))*sum((log(train_pred$prediction+1) - log(target$price+1))**2))

glimpse(pred)

output <- data.frame(seq(0, nrow(test_final)-1), pred)
colnames(output) <- c("test_id", "price")

write.csv(output, "prediction_xgb.csv", row.names = FALSE)
