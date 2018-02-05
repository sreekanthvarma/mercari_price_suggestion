library(tidyverse)


## Loading datasets 
train <- read.csv("train.tsv", sep = '\t')

test <- read.csv("test.tsv", sep = '\t')

target <- train[c("price")]

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

head(cat1, 10)

data$women <- ifelse(grepl("WOMEN", data$category1), 1, 0)
data$beauty <- ifelse(grepl("BEAUTY", data$category1), 1, 0)
data$kids <- ifelse(grepl("KIDS", data$category1), 1, 0)
data$electronics <- ifelse(grepl("ELECTRONICS", data$category1), 1, 0)
data$men <- ifelse(grepl("MEN", data$category1), 1, 0)
data$home <- ifelse(grepl("HOME", data$category1), 1, 0)
data$vintage_collectibles <- ifelse(grepl("VINTAGE & COLLECTIBLES", data$category1), 1, 0)
data$other <- ifelse(grepl("OTHER", data$category1) | grepl("OTHER", data$category3), 1, 0)
data$handmade <- ifelse(grepl("HANDMADE", data$category1), 1, 0)
data$sports_outdoors <- ifelse(grepl("SPORTS & OUTDOORS", data$category1), 1, 0)

## Checkpoint 1
temp <- data
## save

cat2 <- data %>% 
  group_by(category2) %>% 
  summarize(num = n()) %>%
  arrange(desc(num))

head(cat2, 10)

data$atheletic_apparel <- ifelse(grepl("ATHELETIC APPAREL", data$category2) | grepl("SWIMWEAR", data$category2), 1, 0)
data$makeup <- ifelse(grepl("MAKEUP", data$category2) | grepl("SKIN CARE", data$category2), 1, 0)
data$tops_blouses <- ifelse(grepl("TOPS & BLOUSES", data$category2) | grepl("TOPS", data$category2), 1, 0)
data$shoes <- ifelse(grepl("SHOES", data$category2) | grepl("SHOES", data$category3) | grepl("BOOTS", data$category3) | grepl("FASHION SNEAKERS", data$category3), 1, 0)
data$jewelry <- ifelse(grepl("JEWELRY", data$category2) | grepl("NECKLACES", data$category3), 1, 0)
data$toys <- ifelse(grepl("TOYS", data$category2) | grepl("TOYS", data$category2), 1, 0)
data$cell_phones_accessories <- ifelse(grepl("CELL PHONES & ACCESSORIES", data$category2) | grepl("CASES, COVERS & SKINS", data$category3), 1, 0)
data$women_handbags <- ifelse(grepl("WOMEN'S HANDBAGS", data$category2), 1, 0)
data$dresses <- ifelse(grepl("DRESSES", data$category2), 1, 0)
data$womens_accessories <- ifelse(grepl("WOMEN'S ACCESSORIES", data$category2), 1, 0)


cat3 <- data %>% 
  group_by(category3) %>% 
  summarize(num = n()) %>%
  arrange(desc(num))

head(cat3, 10)

data$t_shirts <- ifelse(grepl("T-SHIRTS", data$category3), 1, 0)
data$pants_tights_leggings <- ifelse(grepl("PANTS, TIGHTS, LEGGINGS", data$category3), 1, 0)
data$face <- ifelse(grepl("FACE", data$category3), 1, 0)
data$games <- ifelse(grepl("GAMES", data$category3), 1, 0)
data$lips <- ifelse(grepl("LIPS", data$category3), 1, 0)
data$atheletic <- ifelse(grepl("ATHELETIC", data$category3), 1, 0)
data$eyes <- ifelse(grepl("EYES", data$category3), 1, 0)


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

## Checkpoint2
temp2 <- data
## save


word_count <- data.frame(table(unlist(strsplit(data$item_description, " ")))) 

data$free_ship <- ifelse(grepl("FREE", data$item_description) | grepl("SHIPPING", data$item_description), 1, 0)
data$positive <- ifelse(grepl("GREAT", data$item_description) | grepl("GOOD", data$item_description) |
                          grepl("PERFECT", data$item_description) | grepl("EXCELLENT", data$item_description) |
                          grepl("SUPER", data$item_description), 1, 0)
data$old <- ifelse(grepl("WORN", data$item_description) | grepl("USED", data$item_description), 1, 0)
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

## Checkpoint3
temp3 <- data
## save

glimpse(data)

l <- nrow(target)

train_final <- data[1:l,]
test_final <- data[(l+1):nrow(data),]

train_final <- cbind(target, train_final)


library(randomForest)

rF <- randomForest(price~., data = train_final, mtry = 7, ntree = 20, importance = TRUE)
prediction <- data.frame(predict(rF, test_final))

glimpse(train_final)

output <- data.frame(seq(0, nrow(test_final)-1), prediction)
colnames(output) <- c("test_id", "price")

write.csv(output, "prediction.csv", row.names = FALSE)