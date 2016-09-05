library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(Matrix)

train <- read.csv("data/train.csv", stringsAsFactors = FALSE)
test <- read.csv("data/test.csv", stringsAsFactors = FALSE)
y <- as.factor(train$OutcomeType)

names(train)[1] <- "ID" #different format vs test

data <- rbind(train[,-c(4,5)], test)


#LEAK:
data %>% count(DateTime) %>% arrange(desc(n)) -> temp
right_join(temp, data, by = "DateTime") -> data

#NAME:
data$named <- 1*(data$Name != "")
#number of letter in the name:
data$namelength <- nchar(data$Name)


#let's start with DATETIME:
data %>% 
    mutate(year = year(DateTime),
           month = month(DateTime),
           day = day(DateTime),
           hour = hour(DateTime),
           wday = wday(DateTime),
           hourz =  hour(DateTime) + minute(DateTime)/60) -> data

data$minute <- minute(data$DateTime)

data$minute0 <- 1*(data$minute == 0) 

data$DateTime <- as.numeric(as.POSIXct(data$DateTime))

data$time <- data$hour*60 + data$minute

#make a weekend var:
data$weekend <- 1*data$wday %in% c(1, 7)

# so I should use weekend, year, month and hour - no NA's

#SEXUPONOUTCOME:
# seems important:
data$SexuponOutcome[data$SexuponOutcome == ""] <- "Unknown"

strsplit(data$SexuponOutcome, " ") %>% sapply(function(x){x[1]}) -> data$neuter
strsplit(data$SexuponOutcome, " ") %>% sapply(function(x){x[2]}) -> data$sex


#AGEUPONOUTCOME:
data$less_month <- grepl("day|week", data$AgeuponOutcome)

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

data$miniature <- grepl("Miniature", data$Breed)
data$agressive <- grepl("Pit Bull|Rottweiler|Siberian Huskey|Boxer", data$Breed)


strsplit(x = temp, split = "/") %>% sapply(function(x){x[1]}) -> data$breed1
strsplit(x = temp, split = "/") %>% sapply(function(x){x[2]}) -> data$breed2

count(data, breed1) %>% arrange(desc(n)) %>% filter(n >75) -> popular
data$breed1[!(data$breed1 %in% popular$breed1)] <- "Exotic"

#COLORS:
strsplit(x = data$Color, split = "/") %>% sapply(function(x){x[1]}) -> data$color1
data %>% count(color1) %>% arrange(desc(n)) %>% filter(n > 200) -> colors
data$color1[!(data$color1 %in% colors$color1)] <- "othercolor"

#dash in the breed:
data$dash <- grepl("/", data$Breed)
#scale Date-Time:
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
                                          minute +
                                          breed1 +
                                          namelength +
                                          named +
                                          wday +
                                          mix, 
                                 data = data)[,-1]


design_matrix_train <- design_matrix[1:dim(train)[1],]


new_y <- as.numeric(y) - 1 # for xgboost.

save.image("data.Rdata")



