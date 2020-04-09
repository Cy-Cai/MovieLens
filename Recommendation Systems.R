
#Load Package
library(dplyr)
library(tidyverse)

#Need to run the code provided by the examer first. It partion the data into edx and validation data set
# Creating training and testing set, 20% will assign as testing set, draw without replacement
library(caret)
set.seed(111)
test_index <-  createDataPartition(y=edx$rating,times=1,p=0.2,list=F)
train_set <- edx[-test_index,]
temp <- edx[test_index,]

# test set should only include movies and users that are in the training set
test_set <- temp%>% semi_join(train_set,by="movieId") %>%
  semi_join(train_set,by="userId")

# Add rows removed from test set back into train set
removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)
rm(temp)

#save the data files, it is used in the R markdownd file
save(train_set,file="rda/train_set.rda")
save(test_set,file="rda/test_set.rda")

# define residual mean square error (RMSE), y_hat is the predicted rating outcome, y is the actual rating

RMSE <- function(true_ratings,predicted_ratings){
  sqrt(mean((true_ratings-predicted_ratings)^2))
}


# two path: one is k-nearest neighbors, the other one is based on matrix factorization
head(edx,5)

#Let me go down the path of matrix factorization first, if time permitted, go to the
#other path as well

# The simplest model,  y_i=mu+e_i
mu<-mean(train_set$rating)

#RMSE Performance
rmse_simplest_model <- RMSE(mu,test_set$rating)

#observe Where it prediction is accurate
#Plot RMSE by movie
temp_p <- test_set %>% mutate(residul=(rating-mu)^2) %>% group_by(movieId) %>%
  summarise(r=sqrt(mean(residul)))

temp_p%>%  ggplot(aes(r))+geom_histogram(bins=20)+labs(x="Residual",y="Movie Count",title="Histogram Movie Count vs Residual")+
  geom_vline(xintercept=1)

mean(temp_p$r>1)

# #the histogram shows that majority of the movies have residual more than 1

temp_p<-test_set %>% mutate(residul=(rating-mu)^2) %>% group_by(movieId) %>%
  summarise(r=sqrt(mean(residul)),count=n()) %>%
  ggplot(aes(count,r))+geom_point()
temp_p+labs(x="Rating Count by Movie",y="Residual",title="Scatter Plot Residual vs Rating Count ")

# # the scatter plot shows that the residual convert to 1 as the rating count increase.
# # In other words, the more rating for a movie doesn't increase the prediction accuracy with this model
#
# observer RMSE by user
temp_p <- test_set %>% mutate(residul=(rating-mu)^2) %>% group_by(userId) %>%
  summarise(r=sqrt(mean(residul))) %>%
  ggplot(aes(r))+geom_histogram()
temp_p+labs(x="Residual",y="User Count",title="Histogram User Count vs Residual")

#the histogram shows that majority of the users have residual more than 1

temp_p <- test_set %>% mutate(residul=(rating-mu)^2) %>% group_by(userId) %>%
  summarise(r=sqrt(mean(residul)),count=n()) %>%
  ggplot(aes(count,r))+geom_point()
temp_p+labs(x="Rating Count per User",y="Residual",title="Scatter Plot Residual vs Rating Count")

# # the scatter plot shows that the residual convert to 1 as the rating count increase.
# # In other words, the more rating from a user doesn't increase the prediction accuracy with this model
#
#
# Observe RMSE by genres
temp_p <- test_set %>% mutate(residul=(rating-mu)^2) %>%
separate_rows(genres, sep = "\\|") %>%
group_by(genres) %>%
  summarise(r=sqrt(mean(residul)),count=n())

temp_p%>% ggplot(aes(genres,r))+geom_bar(stat = "identity") +
  labs(x="Genres",y="Residual",title="Bar Chart Residual vs Genres")+
  geom_text(aes(label=round(r,3)),size=3,check_overlap = TRUE,position=position_dodge(width=0.9), vjust=-0.25)+theme(axis.text.x = element_text(angle = 90, hjust = 1))

# the bar chat shows that all the genres have residual greater than 1

temp_p%>%  ggplot(aes(count,r))+geom_point() +
  labs(x="Rating Count per Genres",y="Residual",title="Scatter Plot Residual vs Rating Count")


# the scatter plot shows that the residual doesn't decrease as the rating count increase
# In other words, the more rating for a genres doesn't increase the prediction accuracy with this model
# 

#Store the results 

rmse_results <- data.frame(method="Average",RMSE=rmse_simplest_model)

#second model
# y_ij=mu+m_i+u_j+e_ij, it takes into movie and user into account. 


#Calculate m_i
movie_avgs <- train_set %>% group_by(movieId) %>% 
  summarise(m_i=mean(rating-mu))

#Caldualte u_j
user_avgs <- train_set %>% left_join(movie_avgs,by="movieId") %>% 
  group_by(userId) %>% 
  summarise(u_i=mean(rating-mu-m_i))

#Calculate the RMSE for model 2
predicted_rating <- test_set %>% left_join(movie_avgs,by="movieId") %>% 
  left_join(user_avgs,by="userId") %>% mutate(predicted_rating=mu+m_i+u_i) %>% 
  .$predicted_rating
rmse_MovieAndUserAverage <- RMSE(predicted_rating,test_set$rating)

#Store the results 
rmse_results <- rbind(rmse_results,
                      data.frame(method="Add Movie and User Average",RMSE=rmse_MovieAndUserAverage))


# observe RMSE by movie
temp_p <- test_set %>% left_join(movie_avgs,by="movieId") %>%
  left_join(user_avgs,by="userId") %>%
  mutate(predicted_rating=mu+m_i+u_i) %>%
  mutate(residul=(rating-predicted_rating)^2) %>%
  group_by(movieId) %>%
  summarise(r=sqrt(mean(residul)),count=n())

temp_p%>%
  ggplot(aes(r))+geom_histogram()+labs(x="Residual",y="Movie Count",title="Histogram Movie Count vs Residual")

#the histogram shows that majority of the movies have residual less than 1

temp_p %>%  ggplot(aes(count,r))+geom_point()+
  labs(x="Rating Count by Movie",y="Residual",title="Scatter Plot Residual vs Rating Count ")

# the scatter plot shows that the residual reduce quickly as the rating count increase to 500.
# the residual reduece slowly after the count is more than 500
# 
# Observe RMSE by user
temp_p <- test_set %>% left_join(movie_avgs,by="movieId") %>%
  left_join(user_avgs,by="userId") %>%
  mutate(predicted_rating=mu+m_i+u_i) %>%
  mutate(residul=(rating-predicted_rating)^2) %>%
  group_by(userId) %>%
  summarise(r=sqrt(mean(residul)),count=n())

temp_p%>%
  ggplot(aes(r))+geom_histogram()+labs(x="Residual",y="User Count",title="Histogram User Count vs Residual")

#the histogram shows that majority of the users have residual less than 1

temp_p %>%  ggplot(aes(count,r))+geom_point()+
  labs(x="Rating Count by User",y="Residual",title="Scatter Plot Residual vs Rating Count ")

# the scatter plot shows that the residual reduce quickly as the rating count increase to 125.
# the residual reduece slowly after the count is more than 125


#Observe RMSE by gernres
temp_p <- test_set %>% left_join(movie_avgs,by="movieId") %>%
  left_join(user_avgs,by="userId") %>%
  mutate(predicted_rating=mu+m_i+u_i) %>%
  mutate(residul=(rating-predicted_rating)^2) %>%
  separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarise(r=sqrt(mean(residul)),count=n())

temp_p%>% ggplot(aes(genres,r))+geom_bar(stat = "identity") +
  labs(x="Genres",y="Residual",title="Bar Chart Residual vs Genres")+
  geom_text(aes(label=round(r,3)),position=position_dodge(width=0.9), vjust=-0.25)

# # the bar chat shows that all the genres have residual greater than 1

temp_p%>%  ggplot(aes(count,r))+geom_point() +
  labs(x="Rating Count per Genres",y="Residual",title="Scatter Plot Residual vs Rating Count")


# # the scatter plot shows that the residual doesn't decrease as the rating count increase
# # In other words, the more rating for a genres doesn't increase the prediction accuracy with this model


#Model 3 using User Profile. It takes into users' genre preference 

#create a genres reference table to break down one-line genres into multiple lines
genres_ref <- train_set %>% distinct(genres) %>% 
  mutate(genres2=genres) %>% 
  separate_rows(genres2, sep = "\\|") 

#left join the genres reference table, which will create multiple lines with different genre for the same 
#user and movie. To be noticed, this will not cause dual counting, since later I will
# calculate weighted average to reduce one rating per user per movie
train_set_with_genres_breakdown <- train_set %>% left_join(genres_ref,by="genres") 
test_set_with_genres_breakdown <- test_set %>% left_join(genres_ref,by="genres") 

#setting up user profile: 
user_profile <- train_set_with_genres_breakdown %>% group_by(userId,genres2) %>% 
  summarise(count=n(),avg_rating=mean(rating),sd_rating=sd(rating),max_rating=max(rating),min_rating=min(rating)) %>% 
  mutate(Percent=count/sum(count))
#which genres has the highest count by user. Marked as Top 1
top_genres_per_users <- user_profile%>% 
  group_by(userId) %>% 
  summarise(count=max(count)) %>% 
  mutate(rank="Top 1")

#Mark the other as Non-Top 1
user_profile <- user_profile %>% left_join(top_genres_per_users,by=c("userId","count"))
user_profile$rank[is.na(user_profile$rank)]<-"Non-Top 1"

#user profile to add average rating for "Top 1" and "Non-Top 1"
user_profile <- user_profile %>% 
              left_join(user_profile %>% 
              mutate(sum_rating=count*avg_rating) %>% 
              group_by(userId,rank) %>% 
              summarise(avg_rating_by_rank=sum(sum_rating)/sum(count)),by=c("userId","rank"))


# Keep only userId, rank ("Top 1" or "Non-Top1") and avrage rating
user_profile_mean_by_rank <- user_profile %>% 
                            distinct(userId,rank,avg_rating_by_rank) %>% 
                            data.frame()

#A table store user's genre prefernce
user_profile_rank_by_genres <- user_profile %>% select(userId,genres2,rank)

# Model 3  y_ij=mu+m_i+u_j+e_ij   ; u_j=u_j(genres) 

#calculating m_i
movie_avgs <- train_set %>% group_by(movieId) %>% 
  summarise(m_i=mean(rating-mu))

#caculating u_j
user_profile_avgs <- train_set_with_genres_breakdown %>% 
  left_join(movie_avgs,by="movieId") %>% 
  left_join(user_profile_rank_by_genres,by=c("userId","genres2")) %>% 
  left_join(user_profile_mean_by_rank,by=c("userId","rank")) %>% 
  group_by(userId,rank) %>% 
  summarise(u_i=mean(rating-mu-m_i))

#assigning rank ("Top 1" and "Non-Top 1") to test set
predicted_rating <- test_set_with_genres_breakdown %>% 
  left_join(movie_avgs,by="movieId") %>% 
  left_join(user_profile_rank_by_genres,by=c("userId","genres2")) 

#if the movie has a genre that is not "Top 1" genre, assign it as "Non-Top 1"
predicted_rating$rank[is.na(predicted_rating$rank)]="Non-Top 1"
 
#calculating precitied rating using model 3
# the weighted rating is used as the predicted rating
predicted_rating <- predicted_rating %>% 
  left_join(user_profile_avgs,by=c("userId","rank")) %>%
  mutate(predicted_rating=mu+m_i+u_i) %>% 
  group_by(userId,movieId) %>% 
  summarise(predicted_rating=mean(predicted_rating))

#join the predcited rating to test set, however I don't want to 
#alter test set, so I use temp table
temp<-test_set %>% left_join(predicted_rating,by=c("userId","movieId"))
  
rmse_MovieAndUserPrfileAverage <- RMSE(temp$predicted_rating,temp$rating)

#Store the results 

rmse_results <- rbind(rmse_results,
                      data.frame(method="Add Movie and User Profile Average",RMSE=rmse_MovieAndUserPrfileAverage))

#Polts

#observe the RMSE by genre
temp_p <- temp %>% left_join(movie_avgs,by="movieId") %>%
  mutate(residul=(rating-predicted_rating)^2) %>%
  separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarise(r=sqrt(mean(residul)),count=n())

temp_p%>% ggplot(aes(genres,r))+geom_bar(stat = "identity") +
  labs(x="Genres",y="Residual",title="Bar Chart Residual vs Genres")+
  geom_text(aes(label=round(r,3)),size=3,check_overlap=TRUE,position=position_dodge(width=0.9), vjust=-0.25)

temp_p%>%  ggplot(aes(count,r))+geom_point() +
  labs(x="Rating Count per Genres",y="Residual",title="Scatter Plot Residual vs Rating Count")

# movies with no genres listed
train_set %>% filter(genres=="(no genres listed)")

# The RMSE by genres drops below 0.9. There are only two genres with RMSE>0.9. 
# 
# The scatter plot shows that the accuracy is stable and mostly independent to the rating counts by genre.   


# important
#Validation Set: Using it for the first time and only time in my code

#break the genres down to multiple lines to fit the User Profile Model
#the weighted predicted rating will be calculated later and won't cause
#dual counting
validation_set_with_genres_breakdown <- validation  %>% left_join(genres_ref,by="genres") 

#assign the rank ("Top 1" or "Non-Top 1")
predicted_rating <- validation_set_with_genres_breakdown %>% 
  left_join(movie_avgs,by="movieId") %>% 
  left_join(user_profile_rank_by_genres,by=c("userId","genres2")) 

#if it is missing rank, but it is not the "Top 1" genre, assign as "Non-Top 1"
predicted_rating$rank[is.na(predicted_rating$rank)]="Non-Top 1"

#Calcualte predicted rating
predicted_rating <- predicted_rating %>% 
  left_join(user_profile_avgs,by=c("userId","rank")) %>%
  mutate(predicted_rating=mu+m_i+u_i) %>% 
  group_by(userId,movieId) %>% 
  summarise(predicted_rating=mean(predicted_rating))


temp<-validation%>% left_join(predicted_rating,by=c("userId","movieId"))

#RMSE with validation data
rmse_validation <- RMSE(temp$predicted_rating,temp$rating)



# install.packages('tinytex')
# tinytex::uninstall_tinytex()
# tinytex::install_tinytex()

library(tinytex)
tinytex::is_tinytex()

