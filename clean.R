library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(Matrix)

train <- read.csv("data/train.csv", stringsAsFactors = FALSE)
test <- read.csv("data/test.csv", stringsAsFactors = FALSE)
y <- as.factor(train$OutcomeType)

names(train)[1] <- "ID"

data <- rbind(train[,-c(4,5)], test)

#NAME:
data$named <- 1*(data$Name != "")

#let's start with DATETIME:
data %>% 
    mutate(year = year(data$DateTime),
           month = month(data$DateTime),
           day = day(data$DateTime),
           hour = hour(data$DateTime),
           wday = wday(data$DateTime)) -> data


data$DateTime <- as.numeric(as.POSIXct(data$DateTime))

#make a weekend var:
data$weekend <- 1*data$wday %in% c(1, 7)

# so I should use weekend, year, month and hour - no NA's

#SEXUPONOUTCOME:
# seems important:
data$SexuponOutcome[data$SexuponOutcome == ""] <- "Unknown"

#AGEUPONOUTCOME:

data$age  <- data$AgeuponOutcome
data$age[grepl("day", data$AgeuponOutcome)] <- "< a week"
data$age[grepl("week", data$AgeuponOutcome)] <-  "< a month"
data$age[data$age == ""] <- "1 year"
data$age[data$age %in% c("13 years", "14 years", "15 years", "16 years", "17 years", "18 years",
                           "19 years", "20 years")] <- "> 12 years" 

data$AgeuponOutcome <- gsub(" years?","0000",data$AgeuponOutcome)
data$AgeuponOutcome <- gsub(" months?","00",data$AgeuponOutcome)
data$AgeuponOutcome <- gsub(" weeks?","0",data$AgeuponOutcome)
data$AgeuponOutcome <- gsub(" days?","",data$AgeuponOutcome)
data$AgeuponOutcome <- as.numeric(paste0("0",data$AgeuponOutcome))

#MIX:
data$mix <- 1*(grepl("Mix", data$Breed, fixed = TRUE))

#BREED:
# seems to matter mostly for cats & PitBull:

gsub(" Mix", "", data$Breed) -> temp

strsplit(x = temp, split = "/") %>% sapply(function(x){x[1]}) -> data$breed1



count(data, breed1) %>% arrange(desc(n)) %>% filter(n >75) -> popular

data$breed1[!(data$breed1 %in% popular$breed1)] <- "Exotic"

#COLORS:
strsplit(x = data$Color, split = "/") %>% sapply(function(x){x[1]}) -> data$color1
data %>% count(color1) %>% arrange(desc(n)) %>% filter(n > 200) -> colors
data$color1[!(data$color1 %in% colors$color1)] <- "othercolor"

#dash in the breed:
data$dash <- grepl("/", data$Breed)

#number of letter in the name:
data$namelength <- nchar(data$Name)

#
data$DateTime <- scale(data$DateTime)

#see how many levels you have
sapply(data, function(x){unique(x) %>% length()})




train <- cbind(data[1:dim(train)[1],], OutcomeType = y) 
test <- cbind(data[-(1:dim(train)[1]),])




# now we're good.

#let's also create the design_matrices.
design_matrix <- sparse.model.matrix( ~   DateTime +
                                          AnimalType +
                                          SexuponOutcome +
                                          age +
                                          AgeuponOutcome +
                                          weekend +
                                          hour +
                                          breed1 +
                                          namelength +
                                          named +
                                          wday +
                                          mix, 
                                 data = data)[,-1]


design_matrix_train <- design_matrix[1:dim(train)[1],]

design_matrix_test <- design_matrix[-(1:dim(train)[1]),]

new_y <- as.numeric(y) - 1 # for xgboost.

save.image("data.Rdata")