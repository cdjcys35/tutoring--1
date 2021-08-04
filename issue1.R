

library(data.table)
library(tidyverse)
library(caret)

churn_data <-fread('data/Churn_Modelling.csv')

churn_data <- subset(churn_data, select = -c(RowNumber, CustomerId, Surname))
churn_data <- churn_data %>% mutate(Gender = as.factor(Gender),
                                    Geography = as.factor(Geography),
                                    NumOfProducts = as.factor(NumOfProducts),
                                    HasCrCard = as.factor(HasCrCard),
                                    IsActiveMember = as.factor(IsActiveMember),
                                    Exited = as.factor(Exited))

levels(churn_data$Exited) <- c("No", "Yes")


churn_data %>% glimpse()


library(rsample)
splits <- initial_split(churn_data, prop = 0.7, strata = Exited)
train <- training(splits)
test <- testing(splits)



set.seed(123)
control <- trainControl(method='cv', 
                        number=5, 
                        classProbs = T,   
                        summaryFunction = twoClassSummary, 
                        savePredictions = T
)



rpart_gridsearch <- train(Exited ~ .,             
                          data = train,               
                          method = 'rpart',  
                          trControl = control,
                          tuneLength = 10,
                          metric = "ROC")

logistic_gridsearch <- train(Exited ~ .,             
                             data = train,               
                             method = 'glm', 
                             family = "binomial",
                             trControl = control,
                             metric = "ROC")






library(recipes)

churn_data$Balance %>% summary()
churn_data %>% 
    ggplot(aes(x = Balance)) + geom_histogram()


churn2 <- churn_data %>% 
    recipe(Exited~.) %>% 
    step_other(all_nominal(), threshold = 0.1) %>% 
    step_cut(Balance, breaks = c(5000, 100000))%>% 
    prep() %>% 
    juice()

churn2 %>% 
    select(Balance) %>% 
    table()

library(rsample)
splits <- initial_split(churn2, prop = 0.7, strata = Exited)
train <- training(splits)
test <- testing(splits)

set.seed(123341)
control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats = 3, 
                        classProbs = T,   
                        summaryFunction = twoClassSummary, 
                        savePredictions = T
)

rpart_gridsearch <- train(Exited ~ .,             
                          data = train,               
                          method = 'rpart',  
                          trControl = control,
                          tuneLength = 10,
                          metric = "Kappa")








