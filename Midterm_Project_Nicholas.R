require("psych")
library(psych)
require(lmtest)
require(plm)
library(car)
require(splines)
require(boot)
require(caTools)
library(dplyr)

x = read.csv("movie_data_new2.csv")

movies = x[-c(1590), -c(1)]
attach(movies)




#Preliminary work to put the data in the best format

par(mfrow= c(2,3))
plot(sum_total_likes, imdb_score, col = "gray")
plot(ratio_movie_cast_likes,imdb_score,  col = "gray")
plot(movie_meter_IMDB_pro,imdb_score, col = "gray")
plot( number_of_votes,imdb_score, col = "gray")
plot(director_facebook_likes, imdb_score, col = "gray")
plot(actor_1_facebook_likes, imdb_score, col = "gray")
plot(plot_summary, imdb_score, col = "gray")

regress_00 = lm(imdb_score~sum_total_likes)
regress_01 = lm(imdb_score~ratio_movie_cast_likes)
regress_02 = lm(imdb_score~movie_meter_IMDB_pro)
regress_03 = lm(imdb_score~number_of_votes)
regress_04 = lm(imdb_score~plot_summary)

summary(regress_00)
summary(regress_01)
summary(regress_02)
summary(regress_03)
summary(regress_04)



#working with categorical variables
movie_data$genres = as.factor(movie_data$genres)
levels(genres)
table(genres)

reg1 = lm(imdb_score~genres)
summary(reg1)
movie_data$plot_summary = as.factor(movie_data$plot_summary)
levels(plot_summary)


#testing for outliers, 

qqPlot(regress_00)
outlierTest(regress_00)
#removing the outliers
#sum of total likes
outlierTest(regress_00)
#rm(movie_data_00)
outlierTest(regress_00)
#number of votes
qqPlot(regress_03)
outlierTest(regress_03)

#movie_meter_IMDB_pro

qqPlot(regress_02)
outlierTest(regress_02)
movie_data_00 = movie_data[-c(655, 1341, 1355, 2479, 1516) , ]
residualPlots(regress_00)

mult_reg = lm(imdb_score~number_of_votes+sum_total_likes+movie_meter_IMDB_pro)
residualPlots(mult_reg)

#sum_total_likes is linear
#movie_meter_IMDB_pro and number_of_votes

#counting null values
sum(is.na(movie_data_00))

#removing null_values
new_data = na.omit(movie_data_00)
#verifying the count of null values
sum(is.na(new_data))

multi_reg = lm(imdb_score~sum_total_likes+ poly(number_of_votes, 1), data = new_data)
multi_reg_00 = lm(imdb_score~sum_total_likes+ poly(number_of_votes, 2), data = new_data)
multi_reg_01 = lm(imdb_score~sum_total_likes+ poly(number_of_votes, 3), data = new_data)
multi_reg_02 = lm(imdb_score~sum_total_likes+ poly(number_of_votes, 4), data = new_data)

#checking the best degree of the model
anova(multi_reg, multi_reg_00, multi_reg_01, multi_reg_02)

regression_00 = lm(imdb_score~sum_total_likes+  poly(movie_meter_IMDB_pro, 1), data = new_data)
regression_01 = lm(imdb_score~sum_total_likes+  poly(movie_meter_IMDB_pro, 2), data = new_data)
regression_02 = lm(imdb_score~sum_total_likes+  poly(movie_meter_IMDB_pro, 3), data = new_data)
regression_03 = lm(imdb_score~sum_total_likes+  poly(movie_meter_IMDB_pro, 4), data = new_data)

anova(regression_00, regression_01, regression_02, regression_03 )

#final_model = lm(imdb_score~sum_total_likes+  poly(movie_meter_IMDB_pro, 1) + poly(number_of_votes,4), data = new_data)
summary(final_model)

#testing for Heteroskedasticity

#categorical variables


test_00 = lm(imdb_score~number_news_articles)

ncvTest(test_00)
detach(movie_data_00)

attach(new_data)
#New_work__

library(dplyr)

movie_data_2nd = select(read.csv("movie_data_new2.csv"), -c(1))
#rm(movie_data_2nd)

movies$western =as.factor(movies$western)
movies$scifi =as.factor(movies$scifi)
movies$horror =as.factor(movies$horror)
movies$action =as.factor(movies$action)
movies$drama =as.factor(movies$drama)
movies$war =as.factor(movies$war)
movies$thriller =as.factor(movies$thriller)
final_model = glm(imdb_score~ actor_1_star_meter + poly(duration_mins, 5) + poly(number_news_articles, 4) + poly(director_facebook_likes, 4) + poly(movie_budget, 4) + poly(actor_2_facebook_likes, 2)  + poly(sum_total_likes, 3)+  poly(movie_meter_IMDB_pro, 4) + poly(number_of_votes,4) + western + action + war + scifi + thriller + drama + horror , data = movies)
#rm(final_model_2)
coeftest(final_model, vcov=vcovHC(final_model, type= "HC1"))

summary(final_model)
which(names(movie_data_2nd)=="imdb_score")

new = (movie_data_2nd[, -c(1)])
rm(new)

#


test_01 = lm(imdb_score~sum_total_likes)
summary(test_01)
test02 = lm(imdb_score~cast_total_facebook_likes)
summary(test02)
test_02 = lm(imdb_score~actor_1_facebook_likes)
summary(test_02)


movie_data_2nd_00 = movie_data_2nd[-c(1590),]
#rm(movie_data_2nd)


#k-fold test
cv.error = cv.glm(movies, final_model, K =30)$delta[1]
cv.error

#data = dplyr::filter(movies, !grepl("kill", plot_keywords))
#rm(data)


#female = dplyr::filter(movie_keywords, grepl("female",movie_data.plot_keywords)
#movie_keywords --> dataframe
#movie_data.plot_keywords --> column

movies$female = ifelse(grepl("female",movies$plot_keywords), 1,0)





