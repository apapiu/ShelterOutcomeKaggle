# ShelterOutcomeKaggle

Exploratory data analysis + models on the Shelter Outcome Dataset from [Kaggle](https://www.kaggle.com/c/shelter-animal-outcomes/scripts).

Using a dataset of intake information including breed, color, sex, and age, we'll need to predict the outcome for each animal. The data is from the [Austin Animal Center](http://www.austintexas.gov/department/animal-services).

**If you want to actually look at the HTML version of the exploratory data analysis** you can do so directly at Kaggle scripts,
[HERE](https://www.kaggle.com/apapiu/shelter-animal-outcomes/visualizing-breeds-and-ages-by-outcome). You can also look at some plots below.

For the models I tried logistic regression and random forests after doing some feature extraction. The gradient boosting model using `xgboost` performs the best - around .72 multiclass logloss -good enough for top 10%

Here are some plots from the rmarkdown EDA. The first one shows the distribution of outcomes by age and the second looks at euthanasia rates by animals. Perhaps not surprisingly breeds that are deemed aggresive like Pitbulls and Rottweilers have the highest euthanasia rates. 
![](/plots/img1.jpg)

![](/plots/img2.jpg)
