library(dplyr)
library(ggplot2)
library(lubridate)
library(ggthemes) # visualization


train <- read.csv("train.csv", stringsAsFactors = FALSE)


sapply(train, function(x){unique(x) %>% length()})

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

#let's create some dates vars:
train %>% 
    mutate(year = year(train$DateTime),
           month = month(train$DateTime),
           day = day(train$DateTime),
           hour = hour(train$DateTime)) -> train




#by animal:

train %>% count(SexuponOutcome)

#by sex
ggplot(data = train, aes(x = SexuponOutcome, fill = OutcomeType)) + 
    geom_bar(stat = "count", position = "fill", width = 0.6) +
    coord_flip() +
    scale_fill_brewer(palette = "Set1")
# whoa big difference here - intacts have a worse fate by far.
#let's make a function for the plot:

pplot <- function(x) {
    x <- substitute(x)
    ggplot(data = train, aes_q(x = x, fill = substitute(OutcomeType))) + 
        geom_bar(stat = "count", position = "fill", width = 0.6) +
        coord_flip() +
        scale_fill_brewer(palette = "Set1")
}


#too many breeds but let's look at the word Mix:
train$Mix <- 1*(grepl("Mix", train$Breed, fixed = TRUE))


ggplot(data = train, aes(x = Mix, fill = OutcomeType)) + 
    geom_bar(stat = "count", position = "fill", width = 0.6) + 
    coord_flip() +
    scale_fill_brewer(palette = "Set1")
#non mix fare a bit better.

#names:
train %>% count(Name) %>% arrange(desc(n)) %>%
    filter(n> 20) -> popular_names

#let's make a common and an unnamed names feature:
train$popular_name <- 1*(train$Name %in% popular_names$Name[-1])
train$noname <- 1*(train$Name == "")
train$unique_name <- 1*(!(train$Name %in% popular_names$Name))

#noname is important!
pplot(noname)
pplot(popular_name)
pplot(unique_name)


#let's look at date and time:
pplot(year) #meh
pplot(day) #meh
pplot(hour)

#hour seems important but also erratic:
ggplot(filter(train, hour %in% 7:19), aes(x = hour, fill = OutcomeType)) +
    geom_bar(stat = "count", position = "fill") +
    scale_fill_brewer(palette = "Set1")


#let's look at age:
train %>% count(AgeuponOutcome) -> temp
train$age = ""

train$age[grepl("day", train$AgeuponOutcome)] <- "baby"
train$age[grepl("week", train$AgeuponOutcome)] <-  "baby" #1704
train$age[grepl("month", train$AgeuponOutcome)] <- "less1yr" #8339
train$age[grepl("year", train$AgeuponOutcome)] <- train$AgeuponOutcome[grepl("year", train$AgeuponOutcome)]

sapply(train$age[grepl("year", train$AgeuponOutcome)],
       function(x) {strsplit(x," ", fixed = TRUE)[[1]][1]})-> train$age[grepl("year", train$AgeuponOutcome)]

train$age[train$age %in% c("11", "12", "13", "14", "15", "16", "17","18", "19", "20")] <- "old"

train$age <- factor(train$age, level = c("baby", "less1yr", 
                                         "1", "2", "3", "4", "5", "6" ,"7",
                                         "8", "9", "10", "old"))

#coool!
ggplot(train, aes(x = age, fill = OutcomeType)) +
    geom_bar(stat = "count", position = "fill") +
    scale_fill_brewer(palette = "Set1")

# less than 1 yr seem to be a lot of these:
ggplot(train, aes(x = age, fill = OutcomeType)) +
    geom_bar(stat = "count") +
    scale_fill_brewer(palette = "Set1")

# a look at the months:
train[grepl("month", train$AgeuponOutcome),] -> temp

ggplot(temp, aes(x = AgeuponOutcome, fill = OutcomeType)) +
    geom_bar(stat = "count", position = "fill") +
    scale_fill_brewer(palette = "Set1")
#months might be worthwhile keeping.
# it might be easier to work with the AgeuponOutcome directly,
#let's just keep it like it is right now:
ggplot(train, aes(x = AgeuponOutcome, fill = OutcomeType)) +
    geom_bar(stat = "count", position = "fill") +
    scale_fill_brewer(palette = "Set1") +
    coord_flip() +
    theme_few()


# let's make bags of words for breed and color:
library(tm)
train$color <- 1*grepl("/", train$Color)
pplot(color)


train$Color <- gsub("/", " ", train$Color, fixed = TRUE)


corpus <- Corpus(VectorSource(train$Color))

tdm <- DocumentTermMatrix(corpus, list(stopwords = TRUE,
                                       removePunctuation = TRUE,
                                       removeNumbers = TRUE,
                                       bounds = list(global = c(30,Inf))))

design_matrix_color <- as.matrix(tdm)

#breed:
train$breed <- 1*grepl("/", train$Breed)

grepl("[Cc]hihuahua", train$Breed) %>%  sum()

train$chiua <- 1*(grepl("Husky", train$Breed))
pplot(Breed)

pplot(breed) #seems vaguely important.


train$Breed <- gsub("/", " ", train$Breed, fixed = TRUE)

corpus <- Corpus(VectorSource(train$Breed))

tdm <- DocumentTermMatrix(corpus, list(stopwords = TRUE,
                                       removePunctuation = TRUE,
                                       removeNumbers = TRUE,
                                       bounds = list(global = c(30,Inf))))

design_matrix_breed <- as.matrix(tdm)
# still around 100 - lots of breeds here:
### I think we've got some decent feature enginnering here:

train %>% count(SexuponOutcome) #one guy unknown
train %>% count(AgeuponOutcome) #bunch of guys unknown
train %>% count(hour)

train$SexuponOutcom[train$SexuponOutcom == ""] <- "Unkown"
train$SexuponOutcome[train$SexuponOutcome == ""] <- "3 years"

design_matrix_gen <- model.matrix(OutcomeType ~ AnimalType + SexuponOutcome +
                                      AgeuponOutcome + hour + noname, data = train)

y = as.factor(train$OutcomeType)

pplot(Color)


