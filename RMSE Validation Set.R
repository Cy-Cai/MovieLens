#Validation Set
validation %>% head()
validation_set_with_genres_breakdown <- validation  %>% left_join(genres_ref,by="genres") 
# validation  %>% left_join(genres_ref,by="genres") %>% filter(is.na(genres2))
# 
# 
# edx %>% filter(movieId==53752)
# edx %>% filter(userId==826)
# 
# genres_ref %>% filter(genres=="Drama|Horror|Mystery|Sci-Fi|Thriller")

predicted_rating <- validation_set_with_genres_breakdown %>% 
  left_join(movie_avgs,by="movieId") %>% 
  left_join(user_profile_rank_by_genres,by=c("userId","genres2")) 


predicted_rating$rank[is.na(predicted_rating$rank)]="Non-Top 1"


predicted_rating <- predicted_rating %>% 
  left_join(user_profile_avgs,by=c("userId","rank")) %>%
  mutate(predicted_rating=mu+m_i+u_i) %>% 
  group_by(userId,movieId) %>% 
  summarise(predicted_rating=mean(predicted_rating))

temp<-validation_set_with_genres_breakdown%>% left_join(predicted_rating,by=c("userId","movieId"))




rmse_validation <- RMSE(temp$predicted_rating,temp$rating)



