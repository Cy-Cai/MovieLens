
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

save(train_set,file="rda/train_set.rda")
save(test_set,file="rda/test_set.rda")

# test set should only include movies and users that are in the training set
test_set <- test_set %>% semi_join(train_set,by="movieId") %>% 
  semi_join(train_set,by="userId")


# define residual mean square error (RMSE), y_hat is the predicted rating outcome, y is the actual rating
# y<-c(1,2,3,4)
# y_hat<-c(2,1,4,3)
# N <- length(y)
# RMSE <- sqrt(sum((y_hat-y)^2)/N)
RMSE <- function(true_ratings,predicted_ratings){
  sqrt(mean((true_ratings-predicted_ratings)^2))
}

RMSE(y,y_hat)

# two path: one is k-nearest neighbors, the other one is based on matrix factorization
head(edx,5)
#Let me go down the path of matrix factorization first, if time permitted, go to the
#other path as well

# The simplest model,  y_i=mu+e_i
mu<-mean(train_set$rating)

#RMSE Performance
RMSE(mu,test_set$rating)

#Where it prediction is accurate
#by movie
temp_p <- test_set %>% mutate(residul=(rating-mu)^2) %>% group_by(movieId) %>% 
  summarise(r=sqrt(mean(residul))) %>% 
  ggplot(aes(r))+geom_histogram()
temp_p+labs(x="Residual",y="Movie Count",title="Histogram Movie Count vs Residual")

#the histogram shows that majority of the movies have residual more than 1

temp_p<-test_set %>% mutate(residul=(rating-mu)^2) %>% group_by(movieId) %>% 
  summarise(r=sqrt(mean(residul)),count=n()) %>% 
  ggplot(aes(count,r))+geom_point()
temp_p+labs(x="Rating Count by Movie",y="Residual",title="Scatter Plot Residual vs Rating Count ")

# the scatter plot shows that the residual convert to 1 as the rating count increase.
# In other words, the more rating for a movie doesn't increase the prediction accuracy with this model

#by user
temp_p <- test_set %>% mutate(residul=(rating-mu)^2) %>% group_by(userId) %>% 
  summarise(r=sqrt(mean(residul))) %>% 
  ggplot(aes(r))+geom_histogram()
temp_p+labs(x="Residual",y="User Count",title="Histogram User Count vs Residual")

#the histogram shows that majority of the users have residual more than 1

temp_p <- test_set %>% mutate(residul=(rating-mu)^2) %>% group_by(userId) %>% 
  summarise(r=sqrt(mean(residul)),count=n()) %>% 
  ggplot(aes(count,r))+geom_point()
temp_p+labs(x="Rating Count per User",y="Residual",title="Scatter Plot Residual vs Rating Count")

# the scatter plot shows that the residual convert to 1 as the rating count increase.
# In other words, the more rating from a user doesn't increase the prediction accuracy with this model


#by genres
temp_p <- test_set %>% mutate(residul=(rating-mu)^2) %>% 
separate_rows(genres, sep = "\\|") %>% 
group_by(genres) %>% 
  summarise(r=sqrt(mean(residul)),count=n()) 

temp_p%>% ggplot(aes(genres,r))+geom_bar(stat = "identity") +
  labs(x="Genres",y="Residual",title="Bar Chart Residual vs Genres")+
  geom_text(aes(label=round(r,3)),position=position_dodge(width=0.9), vjust=-0.25)

# the bar chat shows that all the genres have residual greater than 1

temp_p%>%  ggplot(aes(count,r))+geom_point() +
  labs(x="Rating Count per Genres",y="Residual",title="Scatter Plot Residual vs Rating Count")


#where it is not accurate
movie





