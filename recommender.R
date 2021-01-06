library(dplyr)
library(stringr)
library(stringi)
library(DT)
library(recosystem)
library(shinyjs)
library(sodium)

#load the model
super_model <- readRDS("./final_model_for_app.rds")

#create two dataframes
all_data<-read.csv("ratings_group3.csv")
movies<-read.csv("movies_group3.csv")

#join both
total <- left_join(all_data,movies, by= c("movieId")) 

write.csv(total, "total.csv")

#remove the genre and x
total<-subset(total,select=-c(X,genres))

#split the year from title
movies$year<-str_sub(movies$title,start=-6)
movies$year <- sub("\\D+","",movies$year)
movies$year <- sub("\\)","",movies$year)

#converting to integer data type
movies$year<-as.integer(movies$year)

#creating separate columns of genres
movies["Action"] <- NA
movies["Adventure"] <- NA
movies["Animation"] <- NA
movies["Children"] <- NA
movies["Comedy"] <- NA
movies["Crime"] <- NA
movies["Documentary"] <- NA
movies["Drama"] <- NA
movies["Fantasy"] <- NA
movies["Film-Noir"] <- NA
movies["Horror"] <- NA
movies["Musical"] <- NA
movies["Mystery"] <- NA
movies["Romance"] <- NA
movies["Sci-Fi"] <- NA
movies["Thriller"] <- NA
movies["War"] <- NA
movies["Western"] <- NA


#filling with boolean values
movies$`Action` <- stri_detect_fixed(movies$genres,c("Action"))
movies$`Adventure` <- stri_detect_fixed(movies$genres,c("Adventure"))
movies$`Animation` <- stri_detect_fixed(movies$genres,c("Animation"))
movies$`Children` <- stri_detect_fixed(movies$genres,c("Children"))
movies$`Comedy` <- stri_detect_fixed(movies$genres,c("Comedy"))
movies$Crime <- stri_detect_fixed(movies$genres,c("Crime"))
movies$Documentary <- stri_detect_fixed(movies$genres,c("Documentary"))
movies$`Drama` <- stri_detect_fixed(movies$genres,c("Drama"))
movies$`Fantasy` <- stri_detect_fixed(movies$genres,c("Fantasy"))
movies$`Film-Noir` <- stri_detect_fixed(movies$genres,c("Film-Noir"))
movies$`Horror` <- stri_detect_fixed(movies$genres,c("Horror"))
movies$`Musical` <- stri_detect_fixed(movies$genres,c("Musical"))
movies$Mystery <- stri_detect_fixed(movies$genres,c("Mystery"))
movies$Romance <- stri_detect_fixed(movies$genres,c("Romance"))
movies$`Sci-Fi` <- stri_detect_fixed(movies$genres,c("Sci-Fi"))
movies$`Thriller` <- stri_detect_fixed(movies$genres,c("Thriller"))
movies$`War` <- stri_detect_fixed(movies$genres,c("War"))
movies$`Western` <- stri_detect_fixed(movies$genres,c("Western"))

#imputing the NA values with mean
movies$year[is.na(movies$year)]<-mean(movies$year,na.rm = TRUE)

#imputing outliers with the mean
movies$year[movies$year<10]<-mean(movies$year,na.rm = TRUE)


top<-function(film,user,number_of_movies,release_year,type1,type2,type3){
  film$userId<-user
  film$pred_rating <- super_model$predict(data_memory(film$userId,film$movieId,film$Action, film$Adventure, film$Animation, film$Children, 
                                                      film$Comedy, film$Crime,film$Documentary, film$Drama, film$Fantasy,
                                                      film$`Film-Noir`, film$Horror, film$Musical, film$Mystery,film$Romance,
                                                      film$`Sci-Fi`, film$Thriller, film$War, film$Western, rating=NULL, index1=TRUE),out_memory())
  film<-film[order(film$pred_rating,decreasing=TRUE),]
  film<-film[film$year>=release_year[1]&film$year<=release_year[2],]
  film <- film[type1 %in% colnames(film)|type2 %in% colnames(film)|type3 %in% colnames(film),]  
  film<-subset(film,select=-c(Action, Adventure, Animation, Children, 
                              Comedy, Crime,Documentary, Drama, Fantasy,
                              `Film-Noir`, Horror, Musical, Mystery,Romance,
                              `Sci-Fi`, Thriller, War, Western))
  film<-subset(film,select=-c(pred_rating,userId,movieId,year))
  topN<-film[1:number_of_movies,]
  topN
}




