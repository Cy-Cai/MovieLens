
library(dplyr)
library(tidyverse)

edx%>% group_by(movieId) %>% summarise(count=n()) %>% 
  ggplot(aes(count))+geom_histogram()+scale_x_continuous(trans='log10')

edx%>% group_by(userId) %>% summarise(count=n()) %>% 
  ggplot(aes(count))+geom_histogram()+scale_x_continuous(trans='log10')

# Creating training and testing set, 20% will assign as testing set, draw without replacement
library(caret)
set.seed(755)
test_index <-  createDataPartition(y=edx$rating,times=1,p=0.2,list=F)
train_set <- edx[-test_index,]
test_set <- edx[test_index,]

# test set should only include movies and users that are in the training set
test_set <- test_set %>% semi_join(train_set,by="movieId") %>% 
  semi_join(train_set,by="userId")


# define residual mean square error (RMSE), y_hat is the predicted rating outcome, y is the actual rating
# y<-c(1,2,3,4)
# y_hat<-c(2,1,4,3)
# N <- length(y)
# RMSE <- sqrt(sum((y_hat-y)^2)/N)
RMSD <- function(true_ratings,predicted_ratings){
  sqrt(mean((true_ratings-predicted_ratings)^2))
}

RMSE(y,y_hat)


