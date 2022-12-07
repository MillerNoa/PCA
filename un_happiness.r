# ################################################################
# A case study on happiness of countries around the globe between 2015 - 2019
# World Happiness Data is a survey of the state of global happiness released at 
# a United Nations event in 2017 using data from Gallup World Poll. 
# It reports how the new science of happiness explains personal and national variations in happiness.
# 170 countries are included in all five data sets altogether

# Best possible life: 10
# Worst possible life: 0
# Dystopia - the benchmark: a hypothetical country with lowest national averages

# Happiness score estimates the extent to which each of the measured 6 factors contribute 
# to life evaluations:

# - GDP per capita / Economy
# - Social support or Family (being replaced after year 2017)
# - Health or Life expectancy (being replaced after year 2017)
# - Freedom or Freedom to make life choices
# - Generosity
# - Trust in government or Perceptions of corruption (being replaced after year 2017)

# The columns describe the extent to which these factors contribute in evaluating 
# happiness in the respective countries.

# source: www.kaggle.com, downloaded in April 2021

# AUTHOR
# Noa Miller [nmiller2@sheffield.ac.uk]

# REFERENCES  and CREDITS--
# https://stackoverflow.com/questions/67690889/how-can-i-find-common-fields-columns-among-multiple-data-frames-of-differing-col
# https://stackoverflow.com/questions/21644848/summarizing-multiple-columns-with-dplyr
# https://cran.rstudio.com/web/packages/randomForestExplainer/vignettes/randomForestExplainer.html
# https://stackoverflow.com/questions/15215457/standardize-data-columns-in-r
# https://stackoverflow.com/questions/42676630/order-and-subset-a-multi-column-dataframe-in-r
# https://stackoverflow.com/questions/11889625/annotating-text-on-individual-facet-in-ggplot2
# MAS369/469/MAS61007 Machine Learning, Lecturer: Dr. M.A. Juarez
###############################################################################

# load libraries
library(readr)
library(readxl)
library(tidyverse)
library(patchwork)
library(ggplot2)
library(imputeTS)
library(rpart)
library(ISLR)
library(rpart.plot)
library(randomForest)
library(randomForestExplainer)

# loading the data
setwd("C:/Users/steff/Desktop/webpage_project/happiness_data/")
data <- list()
for (i in 1:5){
  print(paste(2014+i,".xlsx", sep=""))
  data[[i]] <- read_excel(paste(2014+i,".xlsx", sep=""))
}

# rewriting field names
colnames(data[[1]]) <- list("country", "region", "rank", "score", "st_error", "economy",
                          "family", "health", "freedom", "trust", "generosity",
                          "dystopia", "populaiton")
data[[1]]["year"] <-"2015"

colnames(data[[2]]) <- list("country", "region", "rank", " score", "lower_confidence",
                            "upper_confidence", "economy", "family", "health", 
                            "freedom", "trust", "generosity", "dystopia", "population")
data[[2]]["year"] <-"2016"


colnames(data[[3]]) <- list("country", "rank", "score", "whisker_high", "whisker_low",
                            "economy", "family", "health", "freedom", "generosity",
                            "trust", "dystopia", "population")
data[[3]]["year"] <-"2017"

colnames(data[[4]]) <- list("rank", "country", "score", "economy", "family", "health",
                            "freedom", "generosity", "government", "population")
data[[4]]["year"] <-"2018"

colnames(data[[5]]) <- list("rank", "country", "score", "economy", "family", "health",
                            "freedom", "generosity", "trust", "population")
data[[5]]["year"] <-"2019"

for (i in 1:5){
  print(length(data[[i]]))
}

# finding the common variable names
allDataList <- list(data[[1]],data[[2]],data[[3]],data[[4]],data[[5]])
allDataColNames <- lapply(allDataList, names)
commonCols <- Reduce(intersect, allDataColNames)

for (i in 1:5){
data[[i]] <- data[[i]]%>%select(c(commonCols))
}

# check on length3 of each data frame
for (i in 1:5){
print(length(colnames(data[[i]])))
}

# creating one data frame
allData <- rbind(data[[1]],data[[2]],data[[3]],data[[4]],data[[5]])

# imputing any missing data
allData <- imputeTS::na_kalman(allData, smooth=TRUE)
View(allData)

# checking which countries got data for all 5 years
# there are 29 countries which has less than 5 years data out of 170 countries
count(unique(allData["country"]))
toExclude <- allData%>%group_by(country)%>%
  summarize(occurence=n())%>%filter(occurence<5)

toInclude <- allData%>%group_by(country)%>%
  summarize(occurence=n())%>%filter(occurence>=5)

# excluding countries with occurrence less than 5 years
allData <- inner_join(allData,data.frame(toInclude), by="country")%>%select(-c(occurence))

View(allData)

# scaling alldata numeric columns
allData <- allData%>%mutate_at(c("economy", "family", "health", "freedom", "generosity"), 
                               ~(scale(.) %>% as.vector))

# checking mean = 0 and sd =1 after standardizing
colMeans(allData[,3:7])
apply(allData[,3:7], 2, sd)


# the higher the economy / gdp per capita, the more important is social support
ggplot(allData%>%filter(year==2019), aes(x = economy, y = family, fill=as.factor(top20)))+
  geom_point()+
  geom_smooth()+
  ggtitle("The better the economy, the more important is social support")

ggplot(allData%>%filter(year==2019), aes(x = economy, y = generosity))+
  geom_point()+
  geom_smooth()+
  ggtitle("Generosity follows a V-form as economy increases")

ggplot(allData%>%filter(year==2019), aes(x = family, y = generosity))+
  geom_point()+
  geom_smooth()+
  ggtitle("Generosity follows a V-form as economy increases")


allData$top20 <- as.factor(ifelse(allData$rank<=20,1,0))

# extracting values for annotation
allData[order(allData$economy, decreasing=T) & allData$year==2018,]

# the highest rating for economy happened in the United Arab Emirates
allData[allData$economy == max(allData$economy),]

# the highest rating for generosity happened in the Myanmar
allData[allData$generosity == max(allData$generosity),]


# setting plot annotations:
annot <- data.frame(year=c(2018, 2017), value=c(2.93, 4.96), 
                    name=c("Economy", "Generosity"),
                    lab=c("UAE","Myanmar"))

p<-ggplot(allData[,3:9]%>%pivot_longer(-c(year, top20))%>%
         mutate(top20=fct_recode(top20, 
                                 "In top 20" ="1",
                                 "Not in top 20" ="0"))%>%
         mutate(name=fct_recode(name,
                                "Economy" = "economy",
                                "Health" = "health",
                                "Family" = "family",
                                "Freedom" = "freedom",
                                "Generosity"= "generosity")),
                            aes(x=year, y=value))+
  geom_boxplot(aes(x=year, y=value, fill=top20))+
  facet_wrap(.~name)+
  scale_colour_manual(values=c("In top 20" = "#A0A0A0","Not in top 20" = "#009999"))+
  scale_fill_manual(values=c("In top 20" = "#A0A0A0","Not in top 20" = "#009999"), name=NULL)+
  labs(colour="", x="", y="Score")+
  ggtitle("Family and health gradually gained importance since 2017")+
  theme_classic()+
  theme(strip.text = element_text(face="bold", colour = "#404040"),
        axis.text.x = element_text(angle=90))+
  theme(plot.title=element_text(colour="black", size=16))+
  geom_text(data=annot, mapping=(aes(x=Inf, y=Inf, label = lab)),
            hjust   = 1,
            vjust   = 1.1)
p
#ggsave('rplot.jpeg', plot=p)

# aggregating the data to prepare for PCA
allData$rank <- as.integer(allData$rank)
allData$top20 <- as.numeric(allData$top20)
agg <- allData%>%select(-c(year))%>%group_by(country)%>%summarize(across(everything(), list(mean)))
colnames(agg) <- list("country", "rank","economy", "family", "health", "freedom", "generosity")
agg

# the happiest countries are Denmark, Norway and Iceland between 2015 - 2019
agg[order(agg$rank),]

# Principal Component Analysis
agg.pca <- princomp(agg[,3:7], cor=T)

# PC1 is 55.4%, PC2 is 23.7%
summary(agg.pca)
loadings(agg.pca)
biplot(agg.pca)

# fitting a linear model: the simple linear model seems to explain the rank fairly well
# the Median of the residuals is close to zero
# and 80.92% of the variation in the data is explained by these five variables, which is relatively high
# the only explanatory variable without significance is generosity
agg.lm <- lm(rank~economy + family + health + freedom + generosity, data=agg)
summary(agg.lm)

# fitting a Classification Tree
# we want to predict whether a line is from a happy country or not: rank <= 21.4
head(agg[order(agg$rank, decreasing=F),],20)
agg$most_happy <- as.factor(ifelse(agg$rank<=21.4, 1,0))

# splitting up into training and testing data:
ind <- sample(2, nrow(agg), replace=T, prob=c(0.8,0.2))
agg.train <- agg[ind==1,]
agg.test <- agg[ind=2,]

# Fitting a Tree based Model
# predicting based on economy, family, health, freedom and generosity, whether a country belongs to the TOP 20 or not

agg.tr <- rpart(most_happy ~., data=agg.train[,3:8], method="class")
pred<-predict(agg.tr, type="class")
table(pred, agg.train$most_happy)
(table(pred, agg.train$most_happy)[[2]] + table(pred, agg.train$most_happy)[[3]]) /(table(pred, agg.train$most_happy)[[1]] + table(pred, agg.train$most_happy)[[4]])
# 4.71% miss classification rate on the training data- there are 5 miss classifications out of 121 records

pred <- predict(agg.tr, agg.test, type="class")
table(pred, agg.test$most_happy)
# on unseen data our model performs fairly well
# 4.3% misclassified lines
prp(agg.tr)

# Fitting a Random Forest to see improvement:
agg.rf <- randomForest(most_happy~., data=agg.train[,3:8], ntree=30)
importance(agg.rf)
pred <- predict(agg.rf, type="class")
table(pred, agg.train$most_happy)[[2]]
(table(pred, agg.train$most_happy)[[2]] + table(pred, agg.train$most_happy)[[3]]) /(table(pred, agg.train$most_happy)[[1]] + table(pred, agg.train$most_happy)[[4]])
# 5.7% miss-classification rate, poorer performance than tree based model with n=30 trees

plot(agg.rf)


#distribution of minimal depth
min_depth_frame <- min_depth_distribution(agg.rf)
min_depth_frame
plot_min_depth_distribution(min_depth_frame)











