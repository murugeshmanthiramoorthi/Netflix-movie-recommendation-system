library(dplyr)
library(zoo)
library(recosystem)
library(stringr)
library(stringi)

#create two dataframes
all_data<-read.csv("ratings_group3.csv")
movies<-read.csv("movies_group3.csv")

#join both
total <- left_join(all_data,movies, by= c("movieId")) 

#remove the genre and x
#total<-subset(total,select=-c(X,genres))

#TUNING THE MATRIX FACTORIZATION ALGORITHM TO FIND OUT THE BEST PARAMETER VALUE
# This is a randomized algorithm
set.seed(145)
r=Reco()

# NOTE: The  tuning is done over a set of parameter values: two settings of latent vector dimensionality, dim=5, or dim=10, three different values of the learning rate (lrate) and for 5 iterations, involving 5 fold cross validation.
opts<-r$tune(data_memory(total$userId,total$movieId, total$Action, total$Adventure, total$Animation, total$Children, 
                         total$Comedy, total$Crime,total$Documentary, total$Drama, total$Fantasy,
                         total$`Film-Noir`, total$Horror, total$Musical, total$Mystery,total$Romance,
                         total$`Sci-Fi`, total$Thriller, total$War, total$Western,rating=total$rating, index1=TRUE), opts=list(dim=c(5,10), lrate=c(0.05,0.1, 0.15),  niter=50, nfold=5, verbose=FALSE)) 

#Parameter values with minimum cross validated loss
opts$min

#use the best option to train the the training data model
r$train(data_memory(total$userId, total$movieId, total$Action, total$Adventure, total$Animation, total$Children, 
                    total$Comedy, total$Crime,total$Documentary, total$Drama, total$Fantasy,
                    total$`Film-Noir`, total$Horror, total$Musical, total$Mystery,total$Romance,
                    total$`Sci-Fi`, total$Thriller, total$War, total$Western,rating=total$rating, index1=TRUE), opts=c(opts$min, nthread=1, niter=50))

#storing the latent vectors
res <- r$output(out_memory(), out_memory()) 


res

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

str(movies)

summary(movies)

#imputing the NA values with mean
movies$year[is.na(movies$year)]<-mean(movies$year,na.rm = TRUE)

#to find any outliers
sort(unique(movies$year))

#imputing outliers with the mean
movies$year[movies$year<10]<-mean(movies$year,na.rm = TRUE)

#Save model
saveRDS(r, "./final_model_for_app.rds")

#load the saved model
super_model <- readRDS("./final_model_for_app.rds")


#create a function and pass the test data
top<-function(film,user,number_of_movies,release_year,type1,type2,type3){
  film$userId<-user
  film$pred_rating <- super_model$predict(data_memory(film$userId,film$movieId,film$Action, film$Adventure, film$Animation, film$Children, 
                                                      film$Comedy, film$Crime,film$Documentary, film$Drama, film$Fantasy,
                                                      film$`Film-Noir`, film$Horror, film$Musical, film$Mystery,film$Romance,
                                                      film$`Sci-Fi`, film$Thriller, film$War, film$Western, rating=NULL, index1=TRUE),out_memory()) 
  film<-film[order(film$pred_rating,decreasing=TRUE),]
  film<-film[film$year==release_year,]
  film <- film[type1 %in% colnames(film)|type2 %in% colnames(film)|type3 %in% colnames(film),]  
  film<-subset(film,select=-c(Action, Adventure, Animation, Children, 
                       Comedy, Crime,Documentary, Drama, Fantasy,
                       `Film-Noir`, Horror, Musical, Mystery,Romance,
                       `Sci-Fi`, Thriller, War, Western))
  film<-subset(film,select=-c(pred_rating,userId,movieId,year))
  topN<-film[1:number_of_movies,]
  topN
}

#calling function
top(movies,118267,20,2001,"Horror","Action","Thriller")