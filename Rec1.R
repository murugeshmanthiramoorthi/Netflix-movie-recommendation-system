#Matrix factorization recommendation system
#loading the packages
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

#split the year from title
total$year<-str_sub(total$title,start=-6)
total$year <- sub("\\D+","",total$year)
total$year <- sub("\\)","",total$year)

#converting to integer data type
total$year<-as.integer(total$year)

#creating separate columns of genres
total["Action"] <- NA
total["Adventure"] <- NA
total["Animation"] <- NA
total["Children"] <- NA
total["Comedy"] <- NA
total["Crime"] <- NA
total["Documentary"] <- NA
total["Drama"] <- NA
total["Fantasy"] <- NA
total["Film-Noir"] <- NA
total["Horror"] <- NA
total["Musical"] <- NA
total["Mystery"] <- NA
total["Romance"] <- NA
total["Sci-Fi"] <- NA
total["Thriller"] <- NA
total["War"] <- NA
total["Western"] <- NA


#filling with boolean values
total$`Action` <- stri_detect_fixed(total$genres,c("Action"))
total$`Adventure` <- stri_detect_fixed(total$genres,c("Adventure"))
total$`Animation` <- stri_detect_fixed(total$genres,c("Animation"))
total$`Children` <- stri_detect_fixed(total$genres,c("Children"))
total$`Comedy` <- stri_detect_fixed(total$genres,c("Comedy"))
total$Crime <- stri_detect_fixed(total$genres,c("Crime"))
total$Documentary <- stri_detect_fixed(total$genres,c("Documentary"))
total$`Drama` <- stri_detect_fixed(total$genres,c("Drama"))
total$`Fantasy` <- stri_detect_fixed(total$genres,c("Fantasy"))
total$`Film-Noir` <- stri_detect_fixed(total$genres,c("Film-Noir"))
total$`Horror` <- stri_detect_fixed(total$genres,c("Horror"))
total$`Musical` <- stri_detect_fixed(total$genres,c("Musical"))
total$Mystery <- stri_detect_fixed(total$genres,c("Mystery"))
total$Romance <- stri_detect_fixed(total$genres,c("Romance"))
total$`Sci-Fi` <- stri_detect_fixed(total$genres,c("Sci-Fi"))
total$`Thriller` <- stri_detect_fixed(total$genres,c("Thriller"))
total$`War` <- stri_detect_fixed(total$genres,c("War"))
total$`Western` <- stri_detect_fixed(total$genres,c("Western"))

str(total)

summary(total)

#imputing the NA values with mean
total$year[is.na(total$year)]<-mean(total$year,na.rm = TRUE)

#to find any outliers
sort(unique(total$year))

#imputing outliers with the mean
total$year[total$year<10]<-mean(total$year,na.rm = TRUE)



# Split the dataset into 80-20
numberOfRows = nrow(total)
bound = as.integer(numberOfRows *0.8)
train=total[1:bound ,]
test= total[(bound+1):numberOfRows ,]

train

test

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
r$train(data_memory(train$userId, train$movieId, train$Action, train$Adventure, train$Animation, train$Children, 
                    train$Comedy, train$Crime,train$Documentary, train$Drama, train$Fantasy,
                    train$`Film-Noir`, train$Horror, train$Musical, train$Mystery,train$Romance,
                    train$`Sci-Fi`, train$Thriller, train$War, train$Western,rating=train$rating, index1=TRUE), opts=c(opts$min, nthread=1, niter=50))


#save the model
saveRDS(r, "./final_model1.rds")




#storing the latent vectors
res <- r$output(out_memory(), out_memory()) 

res

typeof(res)

#predict the ratings in the testing data
test$pred_rating <- r$predict(data_memory(test$userId,test$movieId,test$Action, test$Adventure, test$Animation, test$Children, 
                                          test$Comedy, test$Crime,test$Documentary, test$Drama, test$Fantasy,
                                          test$`Film-Noir`, test$Horror, test$Musical, test$Mystery,test$Romance,
                                          test$`Sci-Fi`, test$Thriller, test$War, test$Western, rating=NULL, index1=TRUE),out_memory())

#Round the pred_rating values 
#test$pred_rating <- round(test$pred_rating,0)

pred     

#retrieving the actual rating of the test data for further comparison with the predicted rating
test <- left_join(test, data, by= c("movieId", "userId")) 


#compare the pred ratings and actual rating
test$compare <- ifelse(round(test$rating.x,0) == round(test$pred_rating,0),1,0)

mean(test$compare)
## the mean is 0.420415 -- 42.1% of the predicted ratings are accurate

# calculating the RMSE
sqrt(mean((test$pred_rating-test$rating.x)^2))
#rmse is 0.9634674

#R2 value
rsq <- function (x, y) cor(x, y) ^ 2
#r2 is 0.2169118

rsq(test$rating.x,test$pred_rating)