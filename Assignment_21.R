1. Use the below given data set
Data Set
2. Perform the below given activities:
a. Apply PCA to the dataset and show proportion of variance
b. Perform PCA using SVD approach
c. Show the graphs of PCA components

stringroptions(stringsAsFactors=FALSE)
options(scipen=999)
options(java.parameters = "-Xmx6044m") ## memory set to 5GB

setwd("F:/AcadGild/workings")

lib=c("bigmemory", "readr", "Hmisc", "dplyr", "MASS", "ggplot2", "lattice", "caret", "rpart", 
      "randomForest", "rpart.plot","lattice", "rattle", "data.table","RColorBrewer", "reshape2",
      "InformationValue","stringr", "VIF", "Information", "Amelia", "gdata", "party","car", 
      "lubridate","zoo", "sqldf", "fuzzyjoin", "party", "mice")
sapply(lib, require, character.only=TRUE, quietly=TRUE)


RecipData<-fread("F:/AcadGild/workings/epicurious-recipes-with-rating-and-nutrition/epi_r.csv",sep=",",header=TRUE)

# exploratory analysis 
dim(RecipData)
str(RecipData)
summary(RecipData)
describe(RecipData)
head(RecipData)
sapply(RecipData, class) 
names(RecipData)
Amelia::missmap(RecipData) # missing values %
class(RecipData)
sapply(RecipData,class)

#unic values of variables 
distinct(RecipData, protein, calories, sodium, fat)

# missing values count
sapply(RecipData,function(x) sum(is.na(x)))

# missing values imputation with median 
RecipData$calories[which(is.na(RecipData$calories))] <- median(RecipData$calories, na.rm=TRUE)
RecipData$protein[which(is.na(RecipData$protein))] <- median(RecipData$protein, na.rm=TRUE)
RecipData$fat[which(is.na(RecipData$fat))] <- median(RecipData$fat, na.rm=TRUE)
RecipData$sodium[which(is.na(RecipData$sodium))] <- median(RecipData$sodium, na.rm=TRUE)

RData<-as.data.frame(RecipData) # convert to data frame

dim(RData)
summary(RData)

# Correlation and VIF 
cor(RData[2:6])
library(psych)
cor.ci((RData[2:6]), method = "spearman")

library(usdm)
vif(RData[2:6])

# data partition 
set.seed(4444)
ind<-sample(2, nrow(RData),replace = TRUE, prob = c(0.8, 0.2))
Rtrain<-RData[ind==1,]
Rtest<-RData[ind==2,]

IV<-create_infotables(data=Rtrain, valid=Rtest, y="winter")
attributes(IV)
summary(IV)
IV$Tables

#scatterplot and correlation 
pairs.panels(Rtrain[2:6],gap=0,
             bg=c("red", "yellow", "blue" [Rtrain$protein], pch=21))


# principal component analysis
pc<-prcomp(Rtrain[2:6], center = TRUE, scale. = TRUE)
pc
attributes(pc)
pc$center
pc$scale
pc$x
pc$sdev
print(pc)
summary(pc)

# PCs plot (we can see below in plot correlation made "0" by PCA)
pairs.panels(pc$x, gap=0,
             bg=c("red","yellow","blue")[Rtrain$protein], pch=21)

# By plots
g<-ggplot(pc,
          obs.scale=1, 
          var.scale=1, 
          groups=Rtrain$protein, 
          elipse=TRUE, 
          circle=TRUE, 
          ellipse.prob=0.70)
g<-g+scale_alpha_discrete(name='')
g<-g+theme(legend.direction='horizontal', legend.position = 'top')




