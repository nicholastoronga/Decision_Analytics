#olympics = read.csv("recent_final1.csv")
#olympics
#attach(olympics)

levels(Medal)
table(Medal)
require(MASS)
require(klaR)
require(tree)
require(rpart)
require(rpart.plot)
require(randomForest)
require(caTools)
install.packages("caret")
require(caret)
#sum(is.na(olympics))
#olympics$GDP/Capita = (olympics["GDP"])/(olympics["Population"])

dim(olympics)


olympics$X.1 = NULL
olympics$NOC = NULL
olympics$X = NULL
olympics$Host_country = NULL
olympics$notes = NULL
apply(olympics,2,function(x) length(unique(x)))

#Checking the number of unique values
apply(olympics,2,function(x) length(unique(x)))
olympics$Games = as.factor(olympics$Games)
olympics$Medal = as.factor(olympics$Medal)
olympics$Sex = as.factor(olympics$Sex)
olympics$Season = as.factor(olympics$Season)
olympics$City = as.factor(olympics$City)
olympics$Sport = as.factor(olympics$Sport)
olympics$Event = as.factor(olympics$Event)

rf_model = randomForest(Medal~Year+Sex+Age+Height_m+weight_kg+Games+Season+City+Sport+GDP+population, ntree = 500, importance = TRUE)
rf_model
importance(rf_model)
varImpPlot(rf_model)
table(region)
#seeing the datatypes

sapply(olympics, class)
head(olympics, 3)

colnames(olympics)

#importance
value = table(olympics$region)
new_data = olympics[olympics$region %in% names(value[value>=270]),]
#rm(new_data)


apply(new_data,2,function(x) length(unique(x)))


write.csv(new_data, "olympics_file.csv")

version1 = read.csv("olympics_file.csv")


attach(version1)
detach(version1)
rf_model = randomForest(Medal~ region + Year+Sex+Age+Height_m+weight_kg+Games+Season+City+Sport+GDP+population, ntree = 500, data = version1, importance = TRUE)
rf_model
importance(rf_model)
varImpPlot(rf_model)
table(region)
#seeing the datatypes

rf_model2 = randomForest(medal_bin~ region + Year+Sex+Age+Height_m+weight_kg+Sport+GDP+population + BMI + host_country + home_bin , mtry = 4, ntree = 500, data = version1, importance = TRUE)
rf_model2
importance(rf_model2)
varImpPlot(rf_model2)
sapply(olympics, class)
head(olympics, 3)

colnames(olympics)
values = list()
colnames(version1)

#packages
require(ggplot2)
require(ggfortify)


#Swim Data Analysis

swim_data = read.csv("final_swimming_data.csv")
attach(swim_data)
value = table(swim_data$region)
final_swim_data = swim_data[swim_data$region %in% names(value[value>=30]),]
colnames(final_swim_data)

#Checking the number of unique values
apply(final_swim_data,2,function(x) length(unique(x)))

final_swim_data$X.1 = NULL
final_swim_data$NOC = NULL
final_swim_data$X = NULL
final_swim_data$Host_country = NULL
final_swim_data$notes = NULL
apply(final_swim_data,2,function(x) length(unique(x)))


final_swim_data$Games = as.factor(final_swim_data$Games)
final_swim_data$Medal = as.factor(final_swim_data$Medal)
final_swim_data$Sex = as.factor(final_swim_data$Sex)
final_swim_data$Season = as.factor(final_swim_data$Season)
final_swim_data$City = as.factor(final_swim_data$City)
final_swim_data$Sport = as.factor(final_swim_data$Sport)
final_swim_data$Event = as.factor(final_swim_data$Event)
home_bin <- factor( home_bin , ordered = FALSE )
typeof(home_bin)
attach(final_swim_data)

# Principal Component Analysis

version1_vars = version1[, c("Year", "Age", "Height_m", "weight_kg", "population", "BMI", "GDP_capita", "home_bin", "medal_bin")]
pca =prcomp(version1_vars, scale = TRUE)
pca
autoplot(pca, data = version1_vars, loadings = TRUE, col = ifelse(version1_vars$medal_bin == 1, "cornflowerblue", "darkorange1"), loadings.label = TRUE) +  labs(title = "Principal Component Analysis", subtitle = "Medal vs No Medal") + scale_fill_manual(values = c("cornflowerblue","darkorange1"), labels = c("Medal", "No Medal"))


swim_vars = final_swim_data[, c("Year", "Age", "Height_m", "Weight", "population", "BMI", "GDP_capita", "home_bin", "medal_bin", "medal_value","Pop_density","Infant_mortality","Literacy", "Phones") ]
pca1 =prcomp(swim_vars, scale = TRUE)
pca1




autoplot(pca1, data = swim_vars, loadings = TRUE, col = ifelse(swim_vars$medal_bin == 1, "cornflowerblue", "darkorange1"), loadings.label = TRUE) +  labs(title = "Principal Component Analysis", subtitle = "Medal vs No Medal") + scale_fill_manual(values = c("cornflowerblue","darkorange1"), labels = c("Medal", "No Medal"))
rf_model2 = randomForest(medal~ region + Year + Sex+ Age+ Height_m + Weight+ Event + GDP_capita +population + BMI + host_country + home_bin + Area + Pop_density + Literacy + Phones + Infant_mortality, mtry = 4, ntree = 500, data = final_swim_data, importance = TRUE)

#################CREATING NEW VARIABLES###################
athle_events = read.csv("swimming.csv")
attach(athlete_events)
athlete_events$weight_kg = Weight * 0.453592
athlete_events$height_m = Height * 0.01
attach(athlete_events)
athlete_events$BMI = (weight_kg/(height_m*height_m))


#creating the host_country variable, post 2000#
athlete_events$host_country = ifelse(City == 'Athina','Greece',
                                     ifelse(City == 'Beijing','China',
                                            ifelse(City == 'London','UK',
                                                   ifelse(City == 'Rio de Janeiro','Brazil',
                                                          ifelse(City == 'Salt Lake City','USA',
                                                                 ifelse(City == 'Sochi','Russia',
                                                                        ifelse(City == 'Sydney','Australia',
                                                                               ifelse(City == 'Torino','Italy',
                                                                                      ifelse(City == 'Vancouver','Canada',NA)))))))))
noc_regions = subset(noc_regions, select = c('NOC','region'))
athlete_events = dplyr::left_join(athlete_events,noc_regions, by="NOC")

attach(athlete_events)
#creating home bias variable#
athlete_events$home_bin = ifelse(athlete_events$host_country==athlete_events$region,1,0)

#creating medal_bin#
athlete_events$medal_bin = ifelse(athlete_events$Medal == 'None',0,1)
athlete_events$gold = ifelse(athlete_events$Medal == 'Gold',1,0)
athlete_events$silver = ifelse(athlete_events$Medal == 'Silver',1,0)
athlete_events$bronze = ifelse(athlete_events$Medal == 'Bronze',1,0)
#Setting NA in medal bins to 0
athlete_events[ , 22:25][is.na(athlete_events[ , 21:25] ) ] = 0

##Country info
gdpdata = subset(gdpdata, select = c('Country Code','Year', 'GDP'))
athlete_events = dplyr::left_join(athlete_events, gdpdata, by = c("NOC" = "Country Code", "Year" = "Year"))

Population = subset(Population, select = c('Country Code','Year','Population'))
athlete_events = dplyr::left_join(athlete_events, Population, by = c("NOC" = "Country Code", "Year" = "Year"))

athlete_events$GDP_capita = (athlete_events$GDP)/(athlete_events$Population)

#Swimming events, all years
all_swimming = dplyr::filter(athlete_events, Sport == 'Swimming')
#Swimming events, 2000 onwards
swimming = dplyr::filter(all_swimming, Year >=2000)


write.csv(athlete_events, 'all_events.csv')
write.csv(all_swimming, 'all_swimming.csv')
write.csv(swimming, 'swimming.csv')

###########ATHLETE ANALYSIS###########
attach(swimming)
athlete_id = aggregate(data.frame(count = ID), list(value = ID), length)
athlete_id = rename(athlete_id,c('value'='ID'))
athlete_id = dplyr::left_join(athlete_id, swimming, by = "ID")
athlete_id = athlete_id[c('ID','count','Name','Sex','Weight','height_m')]
athlete_id = dplyr::distinct(athlete_id)

women = dplyr::filter(athlete_id, Sex =="F") ##1466 observations
men  = dplyr::filter(athlete_id, Sex =="M") ##1844 observations
#Weight: M vs F#
ggplot(data = athlete_id, aes(x=Weight)) + 
  geom_histogram(binwidth = 5, fill = 'seagreen', color = 'black')+
  theme_light()+
  labs(title = "Histogram: All Swimmers' Weight", x = "Weight (kg)", y = "Count")


f_weight = ggplot(data = women, aes(x=Weight)) + 
  geom_histogram(binwidth = 5, fill = 'tomato1', color = 'black')+
  theme_light()+
  labs(title = "Histogram: Female Swimmers' Weight", x = "Weight (kg)",y = "Count")

m_weight = ggplot(data = men, aes(x=Weight)) + 
  geom_histogram(binwidth = 5, fill = 'steelblue3', color = 'black')+
  theme_light()+
  labs(title = "Histogram: Male Swimmers' Weight", x = "Weight (kg)",y = "Count")

grid.arrange(f_weight, m_weight, nrow = 1, ncol=2)

#Height: M vs F#
athlete_id$height = athlete_id$height*100
ggplot(data = athlete_id, aes(x=height)) + 
  geom_histogram(binwidth = 5, fill = 'seagreen', color = 'black')+
  theme_light()+
  labs(title = "Histogram: All Swimmers' Height", x = "Height (cm)",y = "Count")

f_height = ggplot(data = women, aes(x=height)) + 
  geom_histogram(binwidth = 5, fill = 'tomato1', color = 'black')+
  theme_light()+
  labs(title = "Histogram: Female Swimmers' Height", x = "Height (cm)",y = "Count")

m_height = ggplot(data = men, aes(x=height)) + 
  geom_histogram(binwidth = 5, fill = 'steelblue3', color = 'black')+
  theme_light()+
  labs(title = "Histogram: Male Swimmers' Height", x = "Height (cm)",y = "Count")
grid.arrange(f_height, m_height, nrow = 1, ncol=2)

#####CORRELATION PLOTS#####
men_quant = na.omit(men[,c(5,6,8)])
men_quant = rename(men_quant,c('avg_age'='Average Age','height_m'='Height(m)','Weight'='Weight(kg)'))
corr_men = cor(men_quant)
corrplot_men = ggcorrplot(corr_men, title = "Male Swimmers",lab=TRUE,
                          outline.col = "white",
                          ggtheme = ggplot2::theme_light,
                          colors = c("#6D9EC1","white", "deepskyblue4"))

women_quant = na.omit(women[,c(5,6,8)])
women_quant = rename(women_quant,c('avg_age'='Average Age','height_m'='Height(m)','Weight'='Weight(kg)'))
corr_women = cor(women_quant)
corrplot_women = ggcorrplot(corr_women, title = "Female Swimmers",lab=TRUE,
                            outline.col = "white",
                            ggtheme = ggplot2::theme_light, colors = c("lightcoral","white", "firebrick1"))

all_quant = na.omit(athlete_id[,c(5,6,8)])
all_quant = rename(all_quant,c('avg_age'='Average Age','height_m'='Height(m)','Weight'='Weight(kg)'))
corr_all = cor(all_quant)
corrplot_all = ggcorrplot(corr_all, title = "All Swimmers",lab=TRUE,
                          outline.col = "white",
                          ggtheme = ggplot2::theme_light, colors = c("lightcyan2","white", "aquamarine4"))
grid.arrange(corrplot_all, corrplot_women, corrplot_men, nrow= 1, ncol=3,
             top = textGrob("Swimmer characteristics: Correlation plots",gp=gpar(fontsize=25,font=1)))

all_age = ggplot(data = athlete_id, aes(x=avg_age)) + 
  geom_histogram(binwidth = 2, fill = 'seagreen', color = 'black')+
  theme_light()+
  labs(title = "Histogram: All Swimmers' Average Age", x = "Average Age",y = "Count")+
  scale_x_continuous(breaks=c(10,15,20,25,30,35,40,45))
all_age

f_age = ggplot(data = women, aes(x=avg_age)) + 
  geom_histogram(binwidth = 2, fill = 'tomato1', color = 'black')+
  theme_light()+
  labs(title = "Histogram: Female Swimmers' Average Age", x = "Average Age",y = "Count")+
  scale_x_continuous(breaks=c(10,15,20,25,30,35,40,45))

m_age = ggplot(data = men, aes(x=avg_age)) + 
  geom_histogram(binwidth = 2, fill = 'steelblue3', color = 'black')+
  theme_light()+
  labs(title = "Histogram: Male Swimmers' Average Age", x = "Average Age",y = "Count")+
  scale_x_continuous(breaks=c(10,15,20,25,30,35,40,45))
grid.arrange(f_age, m_age, nrow = 1, ncol=2)

#####DESCRIPTIVE STATS: STARGAZER#######
stargazer(women[c("avg_age","height_m","Weight")],covariate_labels=c("Average age","Height (m)","Weight (kg)"),
          type = "text",
          title = "Descriptive statistics: Female Swimmers from 2000 to 2016",
          digits = 1, out="women.txt")
stargazer(men[c("avg_age","height_m","Weight")],covariate_labels=c("Average age","Height (m)","Weight (kg)"),
          type = "html",
          title = "Descriptive statistics: Male Swimmers from 2000 to 2016", 
          digits = 1, out="men.html")
#######SUBSETTING MEDAL WINNERS########
all_medalist_events = dplyr::filter(swimming, medal_bin==1)
all_medalist_counts = sapply(all_medalist_events, function(x) length(unique(x)))
all_medalist_counts = as.data.frame(all_medalist_counts)
attach(all_medalist_counts)
athlete_count_medal = as.data.frame(count(all_medalist_events, c("region.x","Name")))

athlete_count_medal = dplyr::inner_join(athlete_count_medal, athlete_id, by = "Name")
athlete_count_medal$win_perc = athlete_count_medal$freq/athlete_count_medal$count #Freq = how many times the athlete won a medal
mean(athlete_count_medal$win_perc)

country_count_medal = all_medalist_counts

plot(freq)
#Female winners
f_medalist_events = dplyr::filter(swimming, medal_bin==1 & Sex=="F")
f_medalist_counts = sapply(f_medalist_events, function(x) length(unique(x)))
f_medalist_counts = as.data.frame(f_medalist_counts)
country_count_f = aggregate(data.frame(count = ID), list(value = ID), length)

m_medalist_events = dplyr::filter(swimming, medal_bin==1 & Sex=="M")
m_medalist_counts = sapply(m_medalist_events, function(x) length(unique(x)))
m_medalist_counts = as.data.frame(m_medalist_counts)

#loser_events = dplyr::filter(swimming, medal_bin==0)

##Including medal information
temp = aggregate(swimming$medal_bin, by=list(ID=swimming$ID), sum)
temp_2 = sapply(swimming, function(x) length(unique(x)))
temp_2  = as.data.frame(count(swimming, c("region.x","ID")))
temp_2 = temp_2[,-c(3)]

athlete_id = dplyr::left_join(temp, athlete_id, by="ID")
athlete_id = rename(athlete_id,c('x'='medals_total','count'='events_participated'))
athlete_id$win_perc = athlete_id$medals_total/athlete_id$events_participated
athlete_id = athlete_id[,-c(2)]
View(athlete_id)
rm("athlete_Id",'medal_count','f_medalist_counts','m_medalist_counts','f_medalist_events','m_medalist_events')

View(athlete_id)
athlete_id = dplyr::left_join(athlete_id, temp_2, by="ID")
write.csv(athlete_id, 'athlete_medal_info.csv')

rm("temp_3")
temp_gold = aggregate(swimming$gold, by=list(ID=swimming$ID),sum)
temp_gold = rename(temp_gold,c('x'='gold_count'))
temp_silver = aggregate(swimming$silver, by=list(ID=swimming$ID),sum)
temp_silver = rename(temp_silver, c('x'= 'silver_count'))
temp_bronze = aggregate(swimming$bronze, by=list(ID=swimming$ID),sum)
temp_bronze = rename(temp_bronze, c('x'= 'bronze_count'))

medals = dplyr::left_join(temp_gold, temp_silver, by="ID")
medals = dplyr::left_join(medals, temp_bronze, by="ID")
athlete_id = athlete_id[,-c(12:14)]
athlete_id = dplyr::left_join(athlete_id, medals, by="ID")

total_country_count = dplyr::count(athlete_id, region.x)

women2 =dplyr::filter(athlete_id, Sex =="F")
men2 = dplyr::filter(athlete_id, Sex=="M")

#######MEDALS#####
medals2 = dplyr::filter(athlete_id, gold_count >=1|silver_count>=1|bronze_count>=1) 
##medals2 = Athletes who have won at least one medal
medals2 = athlete_id[c(2,3,12:14)]
pairs.panels(medals2)

####EVENT PARTICIPATION VS MEDALS EARNED######
values = c("Gold","Silver","Bronze",NA)
View(athlete_id)
attach(athlete_id)
plot(events_participated,medals_total)

eventvmedal = ggplot(athlete_id, aes(x=events_participated, y=medals_total, color = Sex)) + geom_point(size = 2)
eventvmedal = eventvmedal + theme_light() + scale_color_manual(values = c("tomato1","steelblue3"))+
  labs(title = "Medals won out of events participated in", x = "Total medals won", y="Total event participation")
eventvmedal = eventvmedal + geom_text(data=subset(athlete_id, ID == 94406),
                                      aes(medals_total, events_participated, label = Name))

athlete_id$win_perc = round(win_perc*100)
ggplot(data = subset(athlete_id, win_perc>0), aes(x=win_perc)) + 
  geom_histogram(binwidth = 5, fill = 'gold', color = 'black')+
  theme_light()+labs(title = "Win Percentage, excluding athletes with 0 medals", x = "Win percentage",y = "Count")+
  scale_x_continuous(breaks=c(10,20,30,40,50,60,70,80,90,100))
count(win_perc==0)

######COUNTRY ANALYSIS####
attach(swimming)
temp_5 = c("region.x", "Year", "NOC","GDP_capita")
temp_data = c("gold","silver","bronze")
temp7 = ddply(swimming, temp_5, function(x) colSums(x[temp_data]))
country_medals = temp7
#rm("df_melt", "temp7", "temp5")
attach(country_medals)
country_medals$total = country_medals$gold + country_medals$silver + country_medals$bronze
plot(GDP_capita, total)
asd = lm(total~Year+GDP_capita)
summary(asd)
write.csv(country_medals, 'country_medals.csv')

temp_8 = ddply(country_medals, ~region.x, summarise, all_years = sum(total))

country_medals$Year = as.numeric(country_medals$Year)
attach(country_medals)
total_not_zero = subset(country_medals, total>10)
graph1 = ggplot(total_not_zero, aes(x=Year, y=GDP_capita, size = total, color = region.x)) + geom_point(alpha = 0.9)
graph1 = graph1 + theme_light()
graph1

#ggplot(data=total_not_zero, aes(x=Year, y=GDP_capita))+
geom_point(aes(size=total))+
  scale_size_continuous(range = c(2,15))+
  theme_light()
#Top countries
top_over10 = ggplot(data=total_not_zero, aes(x=reorder(region.x,-all_years), y=all_years, fill=region.x))+
  geom_bar(stat="identity")
top_over10 = top_over10 + theme(axis.text.x=element_text(angle = 35,hjust=1, vjust = 1 ), legend.position="none")+
  labs(title = "Top medalist countries", x = "Countries", y = "Medals earned, 2000-2016")
#+scale_fill_brewer(palette = "Paired")

top_over10
temp = dplyr::left_join(country_medals, temp_8, by=region.x)
attach(country_medals)
plot(Year, GDP_capita)

nrow(subset(temp_8, all_years ==0))
nrow(subset(temp_8, all_years !=0))
149+38
38/187

###GDP VS MEDALS###
data_2000 = 
  attach(total_not_zero)
total_not_zero$Year = as.factor(total_not_zero$Year)
gdpvmedal = ggplot(total_not_zero, aes(x=GDP_capita, y=total, color = Year)) + geom_point(size = 3)
gdpvmedal
gdpvmedal = gdpvmedal + theme_light()+
  labs(title = "GDP per capita VS Total medals earned, 2000-2016", y = "Total medals won", x="GDP per capita")
gdpvmedal

###Box Plots###
plot(Medal,GDP_capita, main = 'GDP per Capita by Medal', col = (c('beige','sienna3', 'grey85','gold2')))

###Random Forest Importance###
forest = randomForest(medal_value~GDP+population+GDP_capita+Area+Infant_mortality+Phones, importance = TRUE)
imp_forest = importance(forest)

###Decision Tree###
library(rpart)
library(rpart.plot)

mytree = rpart(Medal~GDP+population+GDP_capita+Area+Infant_mortality+Phones, cp = 0.00001)
rpart.plot(mytree)

###QDA Model###
swim_qda = qda(Medal~Phones+Area+Infant_mortality+GDP+population)
swim_qda

###Confusion Matrix###
qda.pred <- predict(swim_qda)$class
table(final_swimming_data$Medal, qda.pred, dnn = c('Actual Medal','Predicted Medal'))

###Predictions###
predict(swim_qda, data.frame(Phones = 176.2, Area = 86600, Infant_mortality = 21, GDP = 36037519521, population = 10038188))
predict(swim_qda, data.frame(Phones = 552.2, Area = 9984670, Infant_mortality = 4.75, GDP = 1653000000000, population = 32623490))

##Visuals for numerical variables##
#install.packages("dplyr")
library(dplyr)
#install.packages("reshape")
library(reshape)
library(mtcars)
require(psych)
library(ggplot2)
#install.packages("ggcorrplot")
library(ggcorrplot)
library(gridExtra)
#install.packages('grid')
library(grid)
library(stargazer)
#install.packages("rworldmap")
#library(rworldmap)
#library(plyr)
#detach(package:plyr)
#install.packages("reshape2")
require(reshape2)

####Notes on re-doing this with the right dataset
#1. import the dataset as true_final; the column numbers used to make subset_final are ID, name, region, GDP capita, and the additional 5 
#2. summary_country shows the information for each country that participated
#3. summary_athlete shows the information for each athlete that participated, so there will be duplicate countries
#4. Make sure to screenshot everything in the same size 'frame' so that ratios are consistent
#5. Feel free to remove

subset_final = true_final[,c(3,4,19, 23, 25, 26, 27, 28, 29)]
group_country = group_by(subset_final, region)
summary_country = summarise(group_country, avg_gdpcap = mean(na.omit(GDP_capita)), area = mean(na.omit(Area)), pop_density= mean(na.omit(Pop_density)), infant_mortality = mean(na.omit(Infant_mortality)), literacy=mean(na.omit(Literacy)), phone_usage = mean(na.omit(Phones)))
group_athlete = group_by(subset_final, ID)
summary_athlete = summarise(group_athlete, avg_gdpcap = mean(na.omit(GDP_capita)), area = mean(na.omit(Area)), pop_density= mean(na.omit(Pop_density)), infant_mortality = mean(na.omit(Infant_mortality)), literacy=mean(na.omit(Literacy)), phone_usage = mean(na.omit(Phones)))
#####GDP PER CAPITA#####
#summary_country --> Info by country = 2
#summary_athlete --> Info by athlete = 1
plot_gdp1 = ggplot(data = summary_athlete, aes(x=avg_gdpcap)) + 
  geom_histogram(bins=30, fill = "gold", color='black') + theme_light()+
  labs(title = "By athlete", x = 'GDP/Capita', y= 'Count')
plot_gdp2 = ggplot(data = summary_country, aes(x=avg_gdpcap)) + 
  geom_histogram(bins=30, fill = "aquamarine", color='black') + theme_light()+
  labs(title = "By country",x = 'GDP/Capita', y= 'Count')
grid.arrange(plot_gdp1, plot_gdp2, nrow = 1, ncol=2,
             top = textGrob("GDP/capita Distribution, Average across 2000-2016", gp=gpar(fontsize=20, font=2)))

####COUNTRY AREA####
plot_area1 = ggplot(data = summary_athlete, aes(x=area)) + 
  geom_histogram(bins=20, fill = "gold", color='black') + theme_light()+
  labs(title = "By athlete", x = 'Country area', y= 'Count')
plot_area2 = ggplot(data = summary_country, aes(x=area)) + 
  geom_histogram(bins=20, fill = "aquamarine", color='black') + theme_light()+
  labs(title = "By country",x = 'Country area', y= 'Count')
grid.arrange(plot_area1, plot_area2, nrow = 1, ncol=2,
             top = textGrob("Country area distribution,", gp=gpar(fontsize=20, font=2)))

####Population density####
plot_popd1 = ggplot(data = summary_athlete, aes(x=pop_density)) + 
  geom_histogram(bins=30, fill = "gold", color='black') + theme_light()+
  labs(title = "By athlete", x = 'Population density', y= 'Count')
plot_popd2 = ggplot(data = summary_country, aes(x=pop_density)) + 
  geom_histogram(bins=30, fill = "aquamarine", color='black') + theme_light()+
  labs(title = "By country",x = 'Population density', y= 'Count')
grid.arrange(plot_area1, plot_area2, nrow = 1, ncol=2,
             top = textGrob("Population density distribution", gp=gpar(fontsize=20, font=2)))

####Infant mortality####
plot_inf1 = ggplot(data = summary_athlete, aes(x=infant_mortality)) + 
  geom_histogram(bins=30, fill = "gold", color='black') + theme_light()+
  labs(title = "By athlete", x = 'Infant mortality', y= 'Count')
plot_inf2 = ggplot(data = summary_country, aes(x=infant_mortality)) + 
  geom_histogram(bins=30, fill = "aquamarine", color='black') + theme_light()+
  labs(title = "By country",x = 'Infant mortality', y= 'Count')
grid.arrange(plot_inf1, plot_inf2, nrow = 1, ncol=2,
             top = textGrob("Infant mortality distribution", gp=gpar(fontsize=20, font=2)))

####Literacy####
plot_lit1 = ggplot(data = summary_athlete, aes(x=literacy)) + 
  geom_histogram(bins=30, fill = "gold", color='black') + theme_light()+
  labs(title = "By athlete", x = 'Literacy rates', y= 'Count')
plot_lit2 = ggplot(data = summary_country, aes(x=literacy)) + 
  geom_histogram(bins=30, fill = "aquamarine", color='black') + theme_light()+
  labs(title = "By country",x = 'Literacy rates', y= 'Count')
grid.arrange(plot_lit1, plot_lit2, nrow = 1, ncol=2,
             top = textGrob("Literacy rates distribution", gp=gpar(fontsize=20, font=2)))

####Phone usage####
plot_phone1 = ggplot(data = summary_athlete, aes(x=phone_usage)) + 
  geom_histogram(bins=30, fill = "gold", color='black') + theme_light()+
  labs(title = "By athlete", x = 'Phone usage', y= 'Count')
plot_phone2 = ggplot(data = summary_country, aes(x=phone_usage)) + 
  geom_histogram(bins=30, fill = "aquamarine", color='black') + theme_light()+
  labs(title = "By country",x = 'Phone usage', y= 'Count')
grid.arrange(plot_phone1, plot_phone2, nrow = 1, ncol=2,
             top = textGrob("Phone usage distribution", gp=gpar(fontsize=20, font=2)))

####Correlation plots####
quant1 = summary_athlete[,c(2:7)]
quant = rename(quant,c('phone_usage' = 'Phone usage','literacy'='Literacy','infant_mortality'='Infant mortality','pop_density'='Pop. Density','area'='Area','avg_gdpcap'='Avg. GDP/Capita'))
corr_vars = cor(quant)
corrplot_vars = ggcorrplot(corr_vars, title = "Country variables",lab=TRUE,
                           outline.col = "white",
                           ggtheme = ggplot2::theme_light,
                           colors = c("mediumturquoise","white", "lightseagreen"))
corrplot_vars

View(quant1)


