
test <- read.csv("test.csv", stringsAsFactors = FALSE)


test$named <- 1*(test$Name != "")


#let's start with DATETIME:
test %>% 
    mutate(year = year(test$DateTime),
           month = month(test$DateTime),
           day = day(test$DateTime),
           hour = hour(test$DateTime),
           wday = wday(test$DateTime)) -> test

#make a weekend var:
test$weekend <- 1*test$wday %in% c(1, 7)

test %>% count(SexuponOutcome)
test$SexuponOutcome[test$SexuponOutcome == ""] <- "Unknown"

test$age  <- test$AgeuponOutcome

test$age[grepl("day", test$AgeuponOutcome)] <- "< a week"
test$age[grepl("week", test$AgeuponOutcome)] <-  "< a month"

test$age[test$age == ""] <- "1 year"

test$age[test$age %in% c("13 years", "14 years", "15 years", "16 years", "17 years", "18 years",
                           "19 years", "20 years")] <- "> 12 years" 

test$mix <- 1*(grepl("Mix", test$Breed, fixed = TRUE))


#BREED:
# seems to matter mostly for cats & PitBull:

gsub(" Mix", "", test$Breed) -> temp

strsplit(x = temp, split = "/") %>% sapply(function(x){x[1]}) -> test$breed1

#popular breed1 is from the clean.R dataset
test$breed1[!(test$breed1 %in% popular$breed1)] <- "Exotic"


design_matrix_test <- model.matrix(~ AnimalType + SexuponOutcome + 
                                     age + hour + named + mix + breed1 +
                                     weekend + hour + month + as.factor(year) , data = test)[,-1]

df_test <- data.frame(design_matrix_test)

predict(model_ranger, df_test) -> temp

prediction <- temp$predictions

solution <- data.frame('ID' = test$ID, prediction)

write.csv(x = solution, file = "rf5_ranger_prediction.csv", row.names = FALSE)


