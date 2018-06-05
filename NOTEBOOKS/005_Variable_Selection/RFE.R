#read.csv("C:\\Users\\mmlec\\Desktop\\2018_World_Happiness_Multivariate_Analysis\\DATA\\happiness.csv")


### Feature selection for 10 best:
#Recursive Feature Elimination from caret package. It uses a random forest algo on each iteration to evaluate the model to explore all possible subsets of attributes. It comes out with a set of attributes that can be use to build an accurate model:
  
#In this case, we have 5 interesting features with a R-square of 78%

require(caret); require(mlbench); require(randomForest); require(dplyr)

best <- read.csv("C:\\Users\\mmlec\\Desktop\\2018_World_Happiness_Multivariate_Analysis\\DATA\\happiness_10happiest.csv")

best_df <- best %>% select(-X,-Country,-Region)

control <- rfeControl(functions=rfFuncs, method="cv", number=10)
results <- rfe(best_df[,2:29], best_df[,1], sizes=c(2:29), rfeControl=control)
print(results)
predictors(results)
plot(results,type=c("g","o"))

lmtest <- lm(Happiness_Score~ Family + Mental_and_Substance_Disorder_Index + 
               Air_Pollution, data= best_df)
summary(lmtest) #R_Square of 78% 
plot(lmtest)

require(MASS)
rlmtest <- rlm(Happiness_Score~ Family + Mental_and_Substance_Disorder_Index + 
                 Air_Pollution, data= best_df)

summary(rlmtest)
plot(rlmtest)

# estimate variable importance
importance <- varImp(lmtest, scale=FALSE)

# summarize importance
print(importance)

# plot importance
plot(importance)


### Feature selection for 10 worst:
#Recursive Feature Elimination from caret package. It uses a random forest algo on each iteration to evaluate the model to explore all possible subsets of attributes. It comes out with a set of attributes that can be use to build an accurate model:

#In this case, we have 5 interesting features with a R-square of 78%

require(caret); require(mlbench); require(randomForest); require(dplyr)

worst <- read.csv("C:\\Users\\mmlec\\Desktop\\2018_World_Happiness_Multivariate_Analysis\\DATA\\happiness_10leasthappy.csv")

worst_df <- worst %>% select(-X,-Country,-Region)

control <- rfeControl(functions=rfFuncs, method="cv", number=10)
results <- rfe(worst_df[,2:29], worst_df[,1], sizes=c(2:29), rfeControl=control)
print(results)
predictors(results)
plot(results,type=c("g","o"))

lmtest <- lm(Happiness_Score~ Family + Mental_and_Substance_Disorder_Index + 
               Air_Pollution, data= best_df)
summary(lmtest) #R_Square of 78% 
plot(lmtest)

require(MASS)
rlmtest <- rlm(Happiness_Score~ Family + Mental_and_Substance_Disorder_Index + 
                 Air_Pollution, data= best_df)

summary(rlmtest)
plot(rlmtest)



#----------------------------------------------------------------------------------------------------
# Variable Importance
#----------------------------------------------------------------------------------------------------

# ensure results are repeatable
set.seed(7)

# load the library
library(mlbench)
library(caret)

trainingset <- full_best_df %>% select(-X,-Country,-Region)

# prepare training scheme
control <- trainControl(method="repeatedcv", number=2, repeats=1)

# train the model
model <- train(x=trainingset[,2:29], y=trainingset[,2], method="ANFIS", trControl=control)

# estimate variable importance
importance <- varImp(model, scale=FALSE)

# summarize importance
print(importance)

# plot importance
plot(importance)

require(ggplot2)

ggplot(importance,)




