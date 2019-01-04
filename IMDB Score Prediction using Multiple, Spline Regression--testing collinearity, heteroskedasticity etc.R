#installing and requiring packages
require("psych")
library(psych)
require(lmtest)
require(plm)
library(car)
require(splines)
require(boot)
require(caTools)
library(dplyr)
install.packages("tm")
library("tm")
install.packages("SnowballC")
library("SnowballC")


#data_frame before removing outliers and null_values
uncleaned = read.csv("data_final.csv")
attach(uncleaned)
movie_data= na.omit(uncleaned)
detach(uncleaned)

#verifying the count of null values
sum(is.na(movie_data))
attach(movie_data)


#### Testing variables for relevancy###
##Changing datatype##
movie_data$actor_1_star_meter = as.numeric(movie_data$actor_1_star_meter)
movie_data$actor_2_star_meter = as.numeric(movie_data$actor_2_star_meter)
movie_data$actor_1_facebook_likes = as.numeric(movie_data$actor_1_facebook_likes)
movie_data$actor_3_facebook_likes = as.numeric(movie_data$actor_3_facebook_likes)




## Continuous Variables ##
par(mfrow = c(3,3))
plot(actor_1_star_meter,imdb_score, col = "gray")
plot(actor_1_facebook_likes,imdb_score, col = "gray")
plot(actor_2_facebook_likes,imdb_score, col = "gray")
plot(release_day,imdb_score, col = "gray")
plot(movie_budget, imdb_score, col = "gray")
plot(release_year, imdb_score, col = "gray")
plot(duration_mins, imdb_score, col = "gray")
plot(sum_total_likes,imdb_score, col = "gray")
plot(sum_total_likes, imdb_score, col = "gray")

par(mfrow= c(2,3))
plot(ratio_movie_cast_likes,imdb_score,  col = "gray")
plot(movie_meter_IMDB_pro,imdb_score, col = "gray")
plot( duration_mins,imdb_score, col = "gray")
plot(director_facebook_likes, imdb_score, col = "gray")
plot(actor_1_facebook_likes, imdb_score, col = "gray")



#this is not numerical value
#plot(plot_summary, imdb_score, col = "gray")

typeof(plot_summary)

##Categorical Variables ##
movie_data$release_month=as.factor(movie_data$release_month)
table(release_month)
cat1=lm(imdb_score~release_month)
summary(cat1)

#need to add it to the model
movie_data$language=as.factor(movie_data$language)
table(language)
cat2=lm(imdb_score~language)
summary(cat2)
#need to add it
movie_data$content_rating=as.factor(movie_data$content_rating)
table(content_rating)
cat3=lm(imdb_score~content_rating)
summary(cat3)

movie_data$war=as.factor(movie_data$war)
cat4=lm(imdb_score~war)
summary(cat4)

movie_data$animation=as.factor(movie_data$animation)
cat5=lm(imdb_score~animation)
summary(cat5)

movie_data$crime=as.factor(movie_data$crime)
cat6=lm(imdb_score~crime)
summary(cat6)

movie_data$drama=as.factor(movie_data$drama)
cat7=lm(imdb_score~drama)
summary(cat7)

movie_data$sport=as.factor(movie_data$sport)
cat8=lm(imdb_score~sport)
summary(cat8)

movie_data$horror=as.factor(movie_data$horror)
cat9=lm(imdb_score~horror)
summary(cat9)

movie_data$distributor=as.factor(movie_data$distributor)
cat10=lm(imdb_score~distributor)
summary(cat10)

movie_data$color=as.factor(movie_data$color)
cat11=lm(imdb_score~color)
table(color)
summary(cat11)


movie_data$genres = as.factor(movie_data$genres)
levels(genres)
table(genres)

cat12 = lm(imdb_score~genres)
summary(cat12)

#to look into plot_summary key_words
movie_data$plot_summary = as.factor(movie_data$plot_summary)
levels(plot_summary)


##Removing Outliers##
reg_actor1_meter = lm(imdb_score~actor_1_star_meter)
qqPlot(reg_actor1_meter, main = "Actor 1 Star Meter")
plot(actor_1_star_meter,imdb_score)
outlierTest(reg_actor1_meter)

reg_actor1_fb = lm(imdb_score~actor_1_facebook_likes)
qqPlot(reg_actor1_fb, main = "Actor 1 Facebook Likes")
plot(actor_1_facebook_likes,imdb_score)
outlierTest(reg_actor1_fb)

reg_actor2_fb = lm(imdb_score~actor_2_facebook_likes)
qqPlot(reg_actor2_fb, main = "Actor 2 Facebook Likes")
plot(actor_2_facebook_likes,imdb_score)
outlierTest(reg_actor2_fb)

#critics_review_number

reg_critics = lm(imdb_score~critic_reviews_number)
summary(reg_critics)
outlierTest(reg_critics)
qqPlot((reg_critics))
residualPlots(reg_critics)

#sum_total_likes
regress_00 = lm(imdb_score~sum_total_likes)
summary(regress_00)
qqPlot(regress_00)
outlierTest(regress_00)
#ratio_movie_cast_likes
regress_01 = lm(imdb_score~ratio_movie_cast_likes)
summary(regress_01)
qqPlot(regress_01)
outlierTest(regress_01)

#movie_meter_IMDB_pro
regress_02 = lm(imdb_score~movie_meter_IMDB_pro)
summary(regress_02)
qqPlot(regress_02)
outlierTest(regress_02)
#duration_mins
regress_03 = lm(imdb_score~duration_mins)
summary(regress_03)
qqPlot(regress_03)
outlierTest(regress_03)
#to look into plot_summary
regress_04 = lm(imdb_score~plot_summary)

#movie_budget
mreg=lm(imdb_score~movie_budget)
qqPlot(mreg)
outlierTest(mreg)
#release_year
mreg1=lm(imdb_score~release_year)
qqPlot(mreg1)
outlierTest(mreg1)

#duration_mins
mreg2=lm(imdb_score~duration_mins)
qqPlot(mreg2)
outlierTest(mreg2)

#cast_total_facebook_likes
mreg=lm(imdb_score~cast_total_facebook_likes)
qqPlot(mreg)
outlierTest(mreg)
#number_of_faces_in_movie_poster
mreg1=lm(imdb_score~number_of_faces_in_movie_poster)
qqPlot(mreg1)
outlierTest(mreg1)
#movie_facebook_likes
mreg2=lm(imdb_score~movie_facebook_likes)
qqPlot(mreg2)
outlierTest(mreg2)

moviedata = movie_data[-c(1440, 1596,2380, 1268,602,1282,1391,2543,513),]

#Testing for non-linearity#
library(car)

#Actor1 Star Meter
reg1 = lm(imdb_score~actor_1_star_meter)
residualPlots(reg1,xlab = 'Actor 1 Star Meter')

###reg1 = lm(imdb_score~number_news_articles))

reg2 = lm(imdb_score~number_news_articles)
residualPlots(reg2,xlab = 'number_news_articles')

residualPlots(regress_00,xlab = 'Sum Total likes')

residualPlots(regress_01,xlab = 'Ratio_movie_cast_likes')

residualPlots(regress_02,xlab = 'movie_meter_IMDB_pro')

residualPlots(regress_03,xlab = 'duration_mins')

residualPlots(regress_03,xlab = 'duration_mins')


#Making polynomial plots#
plot(number_news_articles,imdb_score, ylab = 'IMDB Score',xlab = 'Number of News Articles', col='grey')
lines(sort(number_news_articles), predict(reg1)[order(number_news_articles)], col='red')

##Tested for Heteroskedasticity ##
ncvTest(reg_critics)
coeftest(reg_critics, vcov=vcovHC(reg_critics, type="HC1"))
plot(predict(mreg), residuals(mreg), col="red")
abline(0,0,lty=2)
ncvTest(mreg) 
coeftest(mreg, vcov=vcovHC(mreg, type="HC1"))
plot(predict(mreg1), residuals(mreg1), col="red")
abline(0,0,lty=2)
ncvTest(mreg1)
coeftest(mreg1, vcov=vcovHC(mreg1, type="HC1"))
plot(predict(mreg2), residuals(mreg2), col="red")
abline(0,0,lty=2)
ncvTest(mreg2)
coeftest(mreg2, vcov=vcovHC(mreg2, type="HC1"))

reg_actor2_fb = lm(imdb_score~actor_2_facebook_likes)
plot(predict(reg_actor2_fb), residuals(reg_actor2_fb), col="red", main="Actor 2 Facebook Likes")
abline(0,0,lty=2)
ncvTest(reg_actor2_fb)
summary(reg_actor2_fb)
coeftest(reg_actor2_fb, vcov=vcovHC(reg_actor2_fb, type="HC1"))

plot(movie_data2$actor_1_star_meter,movie_data2$imdb_score)
reg_actor1_meter = lm(imdb_score~actor_1_star_meter)
plot(predict(reg_actor1_meter), residuals(reg_actor1_meter), col="red", main = "Actor 1 Star Meter")
abline(0,0,lty=2)
ncvTest(reg_actor1_meter)
summary(reg_actor1_meter)
coeftest(reg_actor1_meter, vcov=vcovHC(reg_actor1_meter, type="HC1"))

##Tested for Collinearity ##
require(psych)
corrvars = movie_new[, c(14,16,18,20,22,34,53,55,56,8,38)]
pairs.panels(corrvars)

##Tested various polynomial degrees for non-linear terms##

linreg=lm(imdb_score~release_year+poly(duration_mins, 1), data=moviedata)
linreg1=lm(imdb_score~release_year+poly(duration_mins, 2), data=moviedata)
linreg2=lm(imdb_score~release_year+poly(duration_mins, 3), data=moviedata)
linreg3=lm(imdb_score~release_year+poly(duration_mins, 4), data=moviedata)
linreg4=lm(imdb_score~release_year+poly(duration_mins, 5), data=moviedata)
linreg5 = lm(imdb_score~release_year+poly(duration_mins, 6), data=moviedata)

anova(linreg, linreg1, linreg2, linreg3, linreg4, linreg5)
bug=lm(imdb_score~release_year+poly(movie_budget, 1), data=moviedata)
bug1=lm(imdb_score~release_year+poly(movie_budget, 2), data=moviedata)
bug2=lm(imdb_score~release_year+poly(movie_budget, 3), data=moviedata)
bug3=lm(imdb_score~release_year+poly(movie_budget, 4), data=moviedata)
bug4 =lm(imdb_score~release_year+poly(movie_budget, 5), data=moviedata)
anova(bug, bug1, bug2, bug3, bug4)

linreg8=lm(imdb_score~duration_mins+movie_budget+poly(release_year, 1), data=moviedata)
linreg9=lm(imdb_score~duration_mins+movie_budget+poly(release_year, 2), data=moviedata)
linreg10=lm(imdb_score~duration_mins+movie_budget+poly(release_year, 3), data=moviedata)
linreg11=lm(imdb_score~duration_mins+movie_budget+poly(release_year, 4), data=moviedata)
anova(linreg8, linreg9, linreg10, linreg11)

###Analyzing top keywords##
library("tm")
library("SnowballC")
text = readLines("keywords.txt")
docs = Corpus(VectorSource(text))
inspect(docs)

docs = tm_map(docs, content_transformer(tolower))
docs = tm_map(docs, removeWords, stopwords("english"))
docs = tm_map(docs, removePunctuation)
docs = tm_map(docs, stripWhitespace)
docs = tm_map(docs, removeWords, c("title"))

datamatrix = TermDocumentMatrix(docs)
m = as.matrix(datamatrix)
v = sort(rowSums(m),decreasing=TRUE)
d = data.frame(word = names(v), freq=v)

View(keywords)
keywords$word=as.factor(keywords$word)
levels(keywords$word)
table(keywords$word)

##Testing the significancy of the top 20 frequent keywords
install.packages('grep')
require(grep)
female=ifelse(grepl('female',plot_keywords),1,0)
reg1=lm(imdb_score~female)
summary(reg1)

school=ifelse(grepl('school',plot_keywords),1,0)
reg2=lm(imdb_score~school)
summary(reg2)

love=ifelse(grepl('love',plot_keywords),1,0)
reg3=lm(imdb_score~love)
summary(reg3)

friend=ifelse(grepl('friend',plot_keywords),1,0)
reg4=lm(imdb_score~friend)
summary(reg4)

nudity=ifelse(grepl('nudity',plot_keywords),1,0)
reg5=lm(imdb_score~nudity)
summary(reg5)

police=ifelse(grepl('police',plot_keywords),1,0)
reg6=lm(imdb_score~police)
summary(reg6)

new=ifelse(grepl('new',plot_keywords),1,0)
reg7=lm(imdb_score~new)
summary(reg7)

death=ifelse(grepl('death',plot_keywords),1,0)
reg8=lm(imdb_score~death)
summary(reg8)

murder=ifelse(grepl('murder',plot_keywords),1,0)
reg9=lm(imdb_score~murder)
summary(reg9)

male=ifelse(grepl('male',plot_keywords),1,0)
reg10=lm(imdb_score~male)
summary(reg10)

sex=ifelse(grepl('sex',plot_keywords),1,0)
reg11=lm(imdb_score~sex)
summary(reg11)

relationship=ifelse(grepl('relationship',plot_keywords),1,0)
reg12=lm(imdb_score~relationship)
summary(reg12)

york=ifelse(grepl('york',plot_keywords),1,0)
reg13=lm(imdb_score~york)
summary(reg13)

based=ifelse(grepl('based',plot_keywords),1,0)
reg14=lm(imdb_score~based)
summary(reg14)

city=ifelse(grepl('city',plot_keywords),1,0)
reg15=lm(imdb_score~city)
summary(reg15)

high=ifelse(grepl('high',plot_keywords),1,0)
reg16=lm(imdb_score~high)
summary(reg16)

film=ifelse(grepl('film',plot_keywords),1,0)
reg17=lm(imdb_score~film)
summary(reg17)

alien=ifelse(grepl('alien',plot_keywords),1,0)
reg18=lm(imdb_score~alien)
summary(reg18)

american=ifelse(grepl('american',plot_keywords),1,0)
reg19=lm(imdb_score~american)
summary(reg19)

reference=ifelse(grepl('reference',plot_keywords),1,0)
reg20=lm(imdb_score~reference)
summary(reg20)

##Added significant keywords into a new column within our dataset##
female = dplyr::filter(moviedata, grepl("female",moviedata$plot_keywords))
moviedata$key_wordfemale = ifelse(grepl("female",moviedata$plot_keywords), 1,0)
male = dplyr::filter(moviedata, grepl("male",moviedata$plot_keywords))
moviedata$key_wordmale = ifelse(grepl("male",moviedata$plot_keywords), 1,0)
relationship = dplyr::filter(moviedata, grepl("relationship",moviedata$plot_keywords))
moviedata$key_wordrelatioship = ifelse(grepl("relationship",moviedata$plot_keywords), 1,0)
murder = dplyr::filter(moviedata, grepl("murder",moviedata$plot_keywords))
moviedata$key_wordmurder = ifelse(grepl("murder",moviedata$plot_keywords), 1,0)
love = dplyr::filter(moviedata, grepl("love",moviedata$plot_keywords))
moviedata$key_wordlove = ifelse(grepl("love",moviedata$plot_keywords), 1,0)

#Categorical Variables

moviedata$western =as.factor(moviedata$western)
moviedata$scifi =as.factor(moviedata$scifi)
moviedata$horror =as.factor(moviedata$horror)
moviedata$action =as.factor(moviedata$action)
moviedata$drama =as.factor(moviedata$drama)
moviedata$war =as.factor(moviedata$war)
moviedata$thriller =as.factor(moviedata$thriller)

#anova

dur1 = lm(imdb_score~sum_total_likes+ poly(duration_mins, 1), data = new_data)
multi_reg_00 = lm(imdb_score~sum_total_likes+ poly(duration_mins, 2), data = new_data)
multi_reg_01 = lm(imdb_score~sum_total_likes+ poly(duration_mins, 3), data = new_data)
multi_reg_02 = lm(imdb_score~sum_total_likes+ poly(duration_mins, 4), data = new_data)

#checking the best degree of the model
anova(multi_reg, multi_reg_00, multi_reg_01, multi_reg_02)

regression_00 = lm(imdb_score~sum_total_likes+  poly(movie_meter_IMDB_pro, 1), data = new_data)
regression_01 = lm(imdb_score~sum_total_likes+  poly(movie_meter_IMDB_pro, 2), data = new_data)
regression_02 = lm(imdb_score~sum_total_likes+  poly(movie_meter_IMDB_pro, 3), data = new_data)
regression_03 = lm(imdb_score~sum_total_likes+  poly(movie_meter_IMDB_pro, 4), data = new_data)

anova(regression_00, regression_01, regression_02, regression_03 )





##Final Model Code##

##K-Fold Test##
moviedata$genres =as.factor(moviedata$genres)
levels(genres)
table(genres)
#regressional_final = lm(imdb_score~ actor_1_star_meter + poly(duration_mins, 5) + poly(number_news_articles, 4) 
#                        + poly(director_facebook_likes, 4) + poly(movie_budget, 4) + poly(actor_2_facebook_likes, 2)  + poly(sum_total_likes, 3) 
#                        +  poly(movie_meter_IMDB_pro, 4) + poly(number_of_votes,4) + color + language + content_rating + western + action + war + scifi + thriller + drama + horror 
#                       + key_wordfemale + key_wordlove + key_wordmale + key_wordmurder + key_wordrelatioship, data = moviedata)
##Editing content_rating##
library(plyr)
movie_data$content_rating = mapvalues(movie_data$content_rating, from=c("Not Rated", "Passed","X"), to = c("Unrated","Approved","NC-17"))
levels(movie_data$content_rating)

#final multiple regression model
multiple_regression = lm(imdb_score~ actor_1_star_meter + poly(duration_mins, 5) + poly(number_news_articles, 4) 
                        + poly(director_facebook_likes, 4) + poly(movie_budget, 4) + poly(actor_2_facebook_likes, 2)  + poly(sum_total_likes, 3) 
                        +  poly(movie_meter_IMDB_pro, 4) + poly(number_of_votes,4) + color + content_rating 
                        + key_wordfemale + key_wordlove + key_wordmale + western + action + war + scifi + thriller + drama + horror + key_wordmurder + key_wordrelatioship, data = moviedata)
summary(multiple_regression)



final_model = glm(imdb_score~ actor_1_star_meter + poly(duration_mins, 5) + poly(number_news_articles, 4) 
   + poly(director_facebook_likes, 4) + poly(movie_budget, 4) + poly(actor_2_facebook_likes, 2)  + poly(sum_total_likes, 3) 
   +  poly(movie_meter_IMDB_pro, 4) + poly(number_of_votes,4) + color + content_rating + western + action + war + scifi + thriller + drama + horror 
   + key_wordfemale + key_wordlove + key_wordmale + key_wordmurder + key_wordrelatioship, data = moviedata)


#Validation set test to analyze out of sample performance

movie_vector= rep(0,100)
for (i in 1:100) {
  movie_sample = sample.split(moviedata$imdb_score, SplitRatio = 0.50)
  train = subset(moviedata, movie_sample==FALSE)
  test_before = subset(moviedata, movie_sample == TRUE)
  test = test_before[-c(1:8),]
  model = glm(imdb_score~ actor_1_star_meter + poly(duration_mins, 5) + poly(number_news_articles, 4) 
                    + poly(director_facebook_likes, 4) + poly(movie_budget, 4) + poly(actor_2_facebook_likes, 2)  + poly(sum_total_likes, 3) 
                    +  poly(movie_meter_IMDB_pro, 4) + poly(number_of_votes,4) + color + content_rating + western + action + war + scifi + thriller + drama + horror 
                    + key_wordfemale + key_wordlove + key_wordmale + key_wordmurder + key_wordrelatioship, data = train)
  test$pred = predict(model, data= test)
  test$residual = test$imdb_score-test$pred
  test$res_sq = (test$residual)^2
  movie_vector[i] = mean(test$res_sq)
}
avg_error = sum(movie_vector)/100
avg_error




#Fixing the model for 

#K-fold test

cv.err = cv.glm(moviedata, final_model,K=50)$delta[1]
cv.err
install.packages("qdapTools")
library(qdapTools)
cbind(movie_data[1], mtabulate(movie_data$language))
movie_data

rm(final_model)







