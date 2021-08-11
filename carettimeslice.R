library(caret)
library(ggplot2)
library(pls)

data(economics)
economics %>% dim()
timeSlices <- createTimeSlices(1:nrow(economics), 
                               initialWindow = 36, horizon = 12, fixedWindow = TRUE)

    
    
trainSlices <- timeSlices[[1]]
testSlices <- timeSlices[[2]]

plsFitTime <- train(unemploy ~ pce + pop + psavert,
                    data = economics[trainSlices[[1]],],
                    method = "pls",
                    preProc = c("center", "scale"))


pred <- predict(plsFitTime,economics[testSlices[[1]],])


true <- economics$unemploy[testSlices[[1]]]

plot(true, col = "red", ylab = "true (red) , pred (blue)", ylim = range(c(pred,true)))
points(pred, col = "blue") 
