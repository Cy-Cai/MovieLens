
library(dplyr)
library(tidyverse)

edx%>% group_by(movieId) %>% summarise(count=n()) %>% 
  ggplot(aes(count))+geom_histogram()+scale_x_continuous(trans='log10')

edx%>% group_by(userId) %>% summarise(count=n()) %>% 
  ggplot(aes(count))+geom_histogram()+scale_x_continuous(trans='log10')

# Creating training and testing set, 20% will assign as testing set, draw without replacement
library(caret)
# set.seed(755)
# test_index <-  createDataPartition(y=edx$rating,times=1,p=0.2,list=F)
# train_set <- edx[-test_index,]
# test_set <- edx[test_index,]
# 
# save(train_set,file="rda/train_set.rda")
# save(test_set,file="rda/test_set.rda")
# 
# # test set should only include movies and users that are in the training set
# test_set <- test_set %>% semi_join(train_set,by="movieId") %>% 
#   semi_join(train_set,by="userId")


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
rmse_simplest_model <- RMSE(mu,test_set$rating)

#Where it prediction is accurate
#by movie
# temp_p <- test_set %>% mutate(residul=(rating-mu)^2) %>% group_by(movieId) %>% 
#   summarise(r=sqrt(mean(residul))) %>% 
#   ggplot(aes(r))+geom_histogram()
# temp_p+labs(x="Residual",y="Movie Count",title="Histogram Movie Count vs Residual")
# 
# #the histogram shows that majority of the movies have residual more than 1
# 
# temp_p<-test_set %>% mutate(residul=(rating-mu)^2) %>% group_by(movieId) %>% 
#   summarise(r=sqrt(mean(residul)),count=n()) %>% 
#   ggplot(aes(count,r))+geom_point()
# temp_p+labs(x="Rating Count by Movie",y="Residual",title="Scatter Plot Residual vs Rating Count ")
# 
# # the scatter plot shows that the residual convert to 1 as the rating count increase.
# # In other words, the more rating for a movie doesn't increase the prediction accuracy with this model
# 
# #by user
# temp_p <- test_set %>% mutate(residul=(rating-mu)^2) %>% group_by(userId) %>% 
#   summarise(r=sqrt(mean(residul))) %>% 
#   ggplot(aes(r))+geom_histogram()
# temp_p+labs(x="Residual",y="User Count",title="Histogram User Count vs Residual")
# 
# #the histogram shows that majority of the users have residual more than 1
# 
# temp_p <- test_set %>% mutate(residul=(rating-mu)^2) %>% group_by(userId) %>% 
#   summarise(r=sqrt(mean(residul)),count=n()) %>% 
#   ggplot(aes(count,r))+geom_point()
# temp_p+labs(x="Rating Count per User",y="Residual",title="Scatter Plot Residual vs Rating Count")
# 
# # the scatter plot shows that the residual convert to 1 as the rating count increase.
# # In other words, the more rating from a user doesn't increase the prediction accuracy with this model
# 
# 
# #by genres
# temp_p <- test_set %>% mutate(residul=(rating-mu)^2) %>% 
# separate_rows(genres, sep = "\\|") %>% 
# group_by(genres) %>% 
#   summarise(r=sqrt(mean(residul)),count=n()) 
# 
# temp_p%>% ggplot(aes(genres,r))+geom_bar(stat = "identity") +
#   labs(x="Genres",y="Residual",title="Bar Chart Residual vs Genres")+
#   geom_text(aes(label=round(r,3)),position=position_dodge(width=0.9), vjust=-0.25)
# 
# # the bar chat shows that all the genres have residual greater than 1
# 
# temp_p%>%  ggplot(aes(count,r))+geom_point() +
#   labs(x="Rating Count per Genres",y="Residual",title="Scatter Plot Residual vs Rating Count")
# 
# 
# # the scatter plot shows that the residual doesn't decrease as the rating count increase
# # In other words, the more rating for a genres doesn't increase the prediction accuracy with this model
# 

#Store the results 

rmse_results <- data.frame(method="Average",RMSE=rmse_simplest_model)

#second model
# y_i=mu+m_i+u_i+e_i
movie_avgs <- train_set %>% group_by(movieId) %>% 
  summarise(m_i=mean(rating-mu))

user_avgs <- train_set %>% left_join(movie_avgs,by="movieId") %>% 
  group_by(userId) %>% 
  summarise(u_i=mean(rating-mu-m_i))

predicted_rating <- test_set %>% left_join(movie_avgs,by="movieId") %>% 
  left_join(user_avgs,by="userId") %>% mutate(predicted_rating=mu+m_i+u_i) %>% 
  .$predicted_rating
rmse_MovieAndUserAverage <- RMSE(predicted_rating,test_set$rating)

#Store the results 

rmse_results <- rbind(rmse_results,
                      data.frame(method="Add Movie and User Average",RMSE=rmse_MovieAndUserAverage))


#by movie
# temp_p <- test_set %>% left_join(movie_avgs,by="movieId") %>% 
#   left_join(user_avgs,by="userId") %>% 
#   mutate(predicted_rating=mu+m_i+u_i) %>% 
#   mutate(residul=(rating-predicted_rating)^2) %>% 
#   group_by(movieId) %>% 
#   summarise(r=sqrt(mean(residul)),count=n()) 
# 
# temp_p%>% 
#   ggplot(aes(r))+geom_histogram()+labs(x="Residual",y="Movie Count",title="Histogram Movie Count vs Residual")
# 
# #the histogram shows that majority of the movies have residual less than 1
# 
# temp_p %>%  ggplot(aes(count,r))+geom_point()+
#   labs(x="Rating Count by Movie",y="Residual",title="Scatter Plot Residual vs Rating Count ")
# 
# # the scatter plot shows that the residual reduce quickly as the rating count increase to 500.
# # the residual reduece slowly after the count is more than 500
# 
# #by user
# temp_p <- test_set %>% left_join(movie_avgs,by="movieId") %>% 
#   left_join(user_avgs,by="userId") %>% 
#   mutate(predicted_rating=mu+m_i+u_i) %>% 
#   mutate(residul=(rating-predicted_rating)^2) %>% 
#   group_by(userId) %>% 
#   summarise(r=sqrt(mean(residul)),count=n()) 
# 
# temp_p%>% 
#   ggplot(aes(r))+geom_histogram()+labs(x="Residual",y="User Count",title="Histogram User Count vs Residual")
# 
# #the histogram shows that majority of the users have residual less than 1
# 
# temp_p %>%  ggplot(aes(count,r))+geom_point()+
#   labs(x="Rating Count by User",y="Residual",title="Scatter Plot Residual vs Rating Count ")
# 
# # the scatter plot shows that the residual reduce quickly as the rating count increase to 125.
# # the residual reduece slowly after the count is more than 125
# 
# temp_p <- test_set %>% left_join(movie_avgs,by="movieId") %>% 
#   left_join(user_avgs,by="userId") %>% 
#   mutate(predicted_rating=mu+m_i+u_i) %>% 
#   mutate(residul=(rating-predicted_rating)^2) %>% 
#   separate_rows(genres, sep = "\\|") %>% 
#   group_by(genres) %>% 
#   summarise(r=sqrt(mean(residul)),count=n()) 
# 
# temp_p%>% ggplot(aes(genres,r))+geom_bar(stat = "identity") +
#   labs(x="Genres",y="Residual",title="Bar Chart Residual vs Genres")+
#   geom_text(aes(label=round(r,3)),position=position_dodge(width=0.9), vjust=-0.25)
# 
# # the bar chat shows that all the genres have residual greater than 1
# 
# temp_p%>%  ggplot(aes(count,r))+geom_point() +
#   labs(x="Rating Count per Genres",y="Residual",title="Scatter Plot Residual vs Rating Count")
# 
# 
# # the scatter plot shows that the residual doesn't decrease as the rating count increase
# # In other words, the more rating for a genres doesn't increase the prediction accuracy with this model
# 

#Model 3 using User Profile

genres_ref <- train_set %>% distinct(genres) %>% 
  mutate(genres2=genres) %>% 
  separate_rows(genres2, sep = "\\|") 

train_set_with_genres_breakdown <- train_set %>% left_join(genres_ref,by="genres") 
test_set_with_genres_breakdown <- test_set %>% left_join(genres_ref,by="genres") 

user_profile <- train_set_with_genres_breakdown %>% group_by(userId,genres2) %>% 
  summarise(count=n(),avg_rating=mean(rating),sd_rating=sd(rating),max_rating=max(rating),min_rating=min(rating)) %>% 
  mutate(Percent=count/sum(count))

train_set_with_genres_breakdown %>% filter(userId==1,genres2=="Action") 
  
top_genres_per_users <- user_profile%>% 
  group_by(userId) %>% 
  summarise(count=max(count)) %>% 
  mutate(rank="Top 1")
  
user_profile <- user_profile %>% left_join(top_genres_per_users,by=c("userId","count"))
user_profile$rank[is.na(user_profile$rank)]<-"Non-Top 1"

user_profile <- user_profile %>% left_join(user_profile %>% mutate(sum_rating=count*avg_rating) %>% group_by(userId,rank) %>% 
  summarise(avg_rating_by_rank=sum(sum_rating)/sum(count)),by=c("userId","rank"))

user_profile_mean_by_rank <- user_profile %>% distinct(userId,rank,avg_rating_by_rank)
user_profile_rank_by_genres <- user_profile %>% select(userId,genres2,rank)

# Model 3  y=mu+u_i+e_i   ; u_i=u_i0+u_i1,  u_i0 is non-top 1, u_i1 is top 1



predicted_rating <- test_set_with_genres_breakdown %>% 
  left_join(user_profile_rank_by_genres,by=c("userId","genres2")) %>% 
  left_join(user_profile_mean_by_rank,by=c("userId","rank")) %>% select(-avg_rating_by_rank) 

predicted_rating$rank[is.na(predicted_rating$rank)]="Non-Top 1"

predicted_rating <- predicted_rating%>% 
  left_join(user_profile_mean_by_rank,by=c("userId","rank")) %>% 
  group_by(userId,movieId) %>% 
  summarise(predicted_rating=mean(avg_rating_by_rank)) %>% data.frame()


rating <- test_set %>% left_join(predicted_rating,by=c("userId","movieId"))


rmse_UserProfileAverage <- RMSE(rating$predicted_rating,rating$rating)

rmse_results <- rbind(rmse_results,
    data.frame(method="Add User Profile",RMSE=rmse_UserProfileAverage))




