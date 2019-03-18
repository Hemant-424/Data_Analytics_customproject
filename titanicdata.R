#loading data from memory
train<-read.csv('train.csv',header=TRUE)

test<-read.csv('test.csv',header=TRUE)

#add a veriable "super" to the test set to allow for combining data sets
test.Survived<-data.frame(Survived=rep("None",nrow(test)),test[,])

#combine the data set
data.combined <-rbind(train, test.Survived)

#a bit about r datatype
str(data.combined)

data.combined$Survived<-as.factor(data.combined$Survived)
data.combined$Pclass<-as.factor(data.combined$Pclass)

#look at the gross survival rates
table(data.combined$Survived)
plot(data.combined$Survived,data.combined$Sex)

hist(data.combined$Name)
#distribute across the classes
table(data.combined$Pclass)

table(data.combined$Sex)

table(data.combined$Name)

#load ggplot2 packages for the visualisation

library(ggplot2)

#hypothesis

train$Pclass<-as.factor(train$Pclass)
ggplot(train, aes(x = Pclass ,fill = factor(Survived))) +
  geom_histogram(width=0.5, stat = "count") +
xlab("pclass") +
ylab("totalcount") +
labs(fill = "Survived")
data.combined$Pclass<-as.numeric(data.combined$Pclass)
data.combined$Fare<-as.numeric(data.combined$Fare)
boxplot(Fare~Pclass, data = data.combined,col= c("gold","red"))

v<-table(data.combined$Pclass,data.combined$Sex)
v
barplot(as.matrix(v))

#Examine the first few name in the dataset

head(as.character(train$Name))

#how many different names in both the dataset train and test

length(unique(as.character(data.combined$Name)))


#two duplicate name closer look

dup.Names<-as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])

#let's take a look at the record

data.combined[which(data.combined$Name  %in% dup.Names),]

#any correlation with other variables(like:Mr,Misses)

#this is not working becoz in this version str_detect does not support
Misses<-data.combined[which(str_detect(data.combined$Name,"Miss")),]

#check out the males
males<-data.combined[which(train$Sex == "male"),]
males[1:50,]
RcmdrPlugin.IPSUR

plot(data.combined$Survived~data.combined$Cabin)
da<-as.numeric(data.combined$Survived)
data.combined$Survived<-as.numeric(data.combined$Survived)
boxplot(data.combined$Survived~data.combined$Cabin)
ls(data.combined)
ls(data.combined$Name)



#name tittle extraction

titles <- apply(data.combined,1,function(row){
  strsplit(strsplit(as.character(row['Name']),', ')[[1]][2],'\\.')[[1]][1]
})
summary(as.factor(titles))

library(ggplot2)
ggplot(data.combined, aes(titles, fill = Survived)) +
  geom_bar(position = 'fill') +
  ggtitle('tittle Impact on Survival') 

library(ggplot2)
#create the familysize
familySize <- data.combined$SibSp + data.combined$Parch + 1
ggplot(data.combined, aes(familySize, fill = Survived)) +
  geom_bar(position = 'fill') +
  ggtitle('Family Size Impact on Survival') + 
  labs(y = '%')


str(data.combined)

males<-data.combined[which(train$Sex == "male"),]
males


install.packages("mice")
library(mice)
sum(is.na(data.combined$Age))
imputes = mice(data.combined, m=5, maxit = 40)
Imputed=complete(imputes,5)
hist(data.combined$Age,  main='Histogram Before impute',col="green")
hist(Imputed$Age, main='Histogram after impute',col="black")

