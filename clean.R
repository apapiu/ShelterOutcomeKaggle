library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)


train <- read.csv("train.csv", stringsAsFactors = FALSE)

#train %>% filter(AnimalType == "Cat") -> train

#sapply(train, function(x){unique(x) %>% length()})

#AnimalID        Name       DateTime    OutcomeType 
#26729           6375          22918              5 
#OutcomeSubtype     AnimalType SexuponOutcome AgeuponOutcome 
#17              2              6             45 
#Breed          Color 
#1380            366 

#[1] "AnimalID"       "Name"           "DateTime"      
#[4] "OutcomeType"    "OutcomeSubtype" "AnimalType"    
#[7] "SexuponOutcome" "AgeuponOutcome" "Breed"         
#[10] "Color" 

#plot function:

#NAME:
train$named <- 1*(train$Name != "")

#let's start with DATETIME:
train %>% 
    mutate(year = year(train$DateTime),
           month = month(train$DateTime),
           day = day(train$DateTime),
           hour = hour(train$DateTime),
           wday = wday(train$DateTime)) -> train

#make a weekend var:
train$weekend <- 1*train$wday %in% c(1, 7)

# so I should use weekend, year, month and hour - no NA's

#SEXUPONOUTCOME:
# seems important:
train$SexuponOutcome[train$SexuponOutcome == ""] <- "Unknown"

#AGEUPONOUTCOME:

train$age  <- train$AgeuponOutcome

train$age[grepl("day", train$AgeuponOutcome)] <- "< a week"
train$age[grepl("week", train$AgeuponOutcome)] <-  "< a month"

train$age[train$age == ""] <- "1 year"


train$age[train$age %in% c("13 years", "14 years", "15 years", "16 years", "17 years", "18 years",
                           "19 years", "20 years")] <- "> 12 years" 


train$mix <- 1*(grepl("Mix", train$Breed, fixed = TRUE))

#BREED:
# seems to matter mostly for cats & PitBull:

gsub(" Mix", "", train$Breed) -> temp

strsplit(x = temp, split = "/") %>% sapply(function(x){x[1]}) -> train$breed1

count(train, breed1) %>%
    arrange(desc(n)) %>%
    filter(n >75) -> popular

train$breed1[!(train$breed1 %in% popular$breed1)] <- "Exotic"

#train$breed1[!(train$breed1 %in% c("Pit Bull", 
#                                    "Domestic Shorthair", "Domestic Medium Hair",
#                               "Domestic Longhair", "Siamese"))] <- "other"

#COLORS:

strsplit(x = train$Color, split = "/") %>% sapply(function(x){x[1]}) -> train$color1

train %>% count(color1) %>% arrange(desc(n)) %>% filter(n > 200) -> colors
    
train$color1[!(train$color1 %in% colors$color1)] <- "othercolor"



#dash in the breed:

train$dash <- grepl("/", train$Breed)

#number of letter in the name:
train$namelength <- nchar(train$Name)



# now we're good.
design_matrix_in <- model.matrix(OutcomeType ~ SexuponOutcome + 
                                     age + hour + named + mix + breed1 + wday +
                                     weekend  + month + as.factor(year) + color1 +
                                     dash + namelength, 
                                 data = train)[,-1]


design_matrix_in <- model.matrix(OutcomeType ~ SexuponOutcome +AgeuponOutcome, data = train)[,-1]





#EDA:
pplot <- function(x) {
    x <- substitute(x)
    ggplot(data = train, aes_q(x = x, fill = substitute(OutcomeType))) + 
        geom_bar(stat = "count", position = "fill", width = 0.6) +
        coord_flip() +
        scale_fill_brewer(palette = "Set1")
}

pplot(year)
pplot(month)
pplot(day) #exclude this, seems noisy
pplot(hour)
pplot(wday) #more adoptions on the weekend.


