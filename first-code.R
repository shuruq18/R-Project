getwd()
setwd("/Users/shuruqalghamdi/Desktop/Bioinformatics_datascience_R")
library(dplyr)

a=10
print(a)

#Function: set of instructions
#Type of Function: (1) Library (2) User-Defined
#<name of function>(<argument/s>): (1)Declared & Defined (2) Calling

a="YourName"
print(a)

a=1.2
print(a)

####Second-code####

x=5

#vector 
A=4.6 
B=4.9 
C=5.2 
D=5.5
E=6.9

gene_expression = c(4600,4.9,5.2,5.5,6.9)
print(gene_expression)

gene_name=c("TIAM1","MPK2","AVI34","GYM56","UPA65")
print(gene_name)

gene_expression[1]
gene_expression[1:4]
gene_expression[c(1,4)]
gene_expression[-1]
gene_expression[c(-1,-4)]


x=5
x

sum(gene_expression)
mean(gene_expression)
length(gene_expression)
log(gene_expression)
sd(gene_expression)

#Dataframe : 2D Data strcuture
# Combination of rows + columns
# Combination of vecors

name_vector = c("Shuruq","Noor","Ahmed","Mohammed","Mera")
degree=c("Btech","Mtech","MSc","BSc","PhD")
age=c(24,23,25,26,33)

my_first_dataframe = data.frame(name_vector,degree,age)

View(my_first_dataframe)

city=c("Riyadh","Newyork","London","Jeddah","Neom")
my_first_dataframe = cbind(my_first_dataframe,city)

new_entry = data.frame(name_vector="Alex", degree="BArch", age=29,city="Cairo")
View(new_entry)

my_first_dataframe = rbind(my_first_dataframe,new_entry)

my_first_dataframe[2,3]
my_first_dataframe[,4]
my_first_dataframe[4,]
my_first_dataframe[c(1,4),]
my_first_dataframe[-1,]
my_first_dataframe[,-1]

my_first_dataframe = my_first_dataframe[-1,]
View(my_first_dataframe)


#####third-code#####
import_DF = read.csv("social.csv")
View(import_DF)

names(import_DF)
dim(import_DF)

import_DF[,3]
import_DF$Age
max(import_DF$Age)
min(import_DF$Age)


# >, <,==,!

3>4

if(3>4){
  print("Maha")
}else{
  print("Sara")
}


# &, |
if(3<4 & 5>10){
  print("Maha")
}else{
  print("Sara")
}


if("maha" == "Maha"){
  print("I am True")
}

#Loop
# for Loop

y_vector = c(f1="apple",f2="grapes",f3="banana",f4="Apricot",f5="Orange",f6="Coconut",f7="banana")


for(x in y_vector){
  print(x)
}

count=0

for(x in y_vector){
  if(x == "banana"){
    count = count+1
  }
}
print(paste("Number of times I found Banana is: ",count))

#function

# <name of function> = function(argument){
#
#}



my_print = function(a){
  print(paste("MY NAME IS: ",a))
}

my_print("Shuruq")


Count_Banana = function(fruit_name){
  count=0
  
  for(x in fruit_name){
    if(x == "banana"){
      count = count+1
    }
  }
  print(paste("Number of times I found Banana is: ",count))
}


Count_Banana(y_vector)
x_vector = c("banana","banana","banana","banana")
Count_Banana(x_vector)

####Fourth-code######
install.packages("hflights")

library(dplyr)
library(hflights)

View(hflights)

names(hflights)

#Verb
#1. select

tempDF = hflights %>%
  select(FlightNum,AirTime,ArrDelay,DepDelay)
View(tempDF)

#2.filter
tempDF = hflights %>%
  select(FlightNum,AirTime,ArrDelay,DepDelay) %>%
  filter(ArrDelay>30 & DepDelay>30)

#3. Mutate
tempDF = hflights %>%
  select(FlightNum,AirTime,ArrDelay,DepDelay) %>%
  filter(ArrDelay>30 & DepDelay>30) %>%
  mutate(TotalDelay=ArrDelay+DepDelay)

#4. Arrange
tempDF = hflights %>%
  select(FlightNum,AirTime,ArrDelay,DepDelay) %>%
  filter(ArrDelay>30 & DepDelay>30) %>%
  mutate(TotalDelay=ArrDelay+DepDelay) %>%
  arrange(desc(TotalDelay))

tempDF = hflights %>%
  select(FlightNum,AirTime,ArrDelay,DepDelay) %>%
  filter(ArrDelay>30 & DepDelay>30) %>%
  mutate(TotalDelay=ArrDelay+DepDelay) %>%
  arrange(desc(AirTime))


#5. group_by, summarise 
tempDF = hflights %>%
  select(FlightNum,AirTime,ArrDelay,DepDelay) %>%
  filter(ArrDelay>30 & DepDelay>30) %>%
  mutate(TotalDelay=ArrDelay+DepDelay) %>%
  group_by(FlightNum) %>%
  summarise(AverageDelay=mean(TotalDelay))
View(tempDF)

data_DF=read.csv("LR.csv")
View(data_DF)

num_of_col = dim(data_DF)[2]

for (i in 5:7){
  for(p in (i+1):8){
    CORR = cor(data_DF[,i],data_DF[,p])
    if(CORR > 0.6){
      print(paste("First Column: ",i," ","Second Column: ",p, "Correlation: ",CORR))
    }
  }
}

data_DF[,-7]

######fifth-code#####
library(ggplot2)
library(dplyr)

todayData = read.csv("LR.csv")
View(todayData)

#Scatter Plots
ggplot(todayData,aes(x=sqft_living,y=price,color=Quality))+
  geom_point()+
  labs(x="Total Build Area (sqft)",y="Cost of the House")+
  theme(plot.title = element_text(size=10,face="bold"),
        axis.text.x = element_text(size=7,colour = "black",angle=90),
        axis.text.y = element_text(size=7,colour = "black"),
        axis.title.x = element_text(size=7),
        axis.title.y = element_text(size=7))


Mean=todayData %>%
  filter(bedrooms<6)%>%
  group_by(Quality)%>%
  summarise(Mean=mean(price))

View(Mean)  

ggplot(Mean, aes(x=Quality,y=Mean))+
  geom_bar(stat="identity")

Mean=todayData %>%
  filter(bedrooms>0)%>%
  filter(bedrooms<20)%>%
  group_by(bedrooms,Quality) %>%
  summarise(Average = mean(price))
#Barchart (stack)
ggplot(Mean, aes(x = bedrooms, y = Average, fill = Quality)) + 
  geom_bar(stat="identity",position="stack")+
  geom_text(aes(label=round(Average,2)),size=2,position = position_dodge(width=0.2))+
  labs(title="Bedrooms Vs Price",x="Number of Bedrooms",y="Price (USD)")+
  theme(plot.title = element_text(size=10,face="bold"),
        axis.text.x = element_text(size=7,color="black",angle=90),
        axis.text.y = element_text(size=7,color="black"),
        axis.title.x = element_text(size=7,color="black"),
        axis.title.y = element_text(size=7,color="black"))

######sixth-code#####

library(ggplot2)
library(dplyr)

data = read.csv("LR.csv",header=TRUE)
numCol = dim(data)[2]
numRow = dim(data)[1]

#Remove the blank/NA column

flag=1
col_blank=c()
C=1
for(i in 1:numCol){
  for(p in 1:numRow){
    if(is.na(data[p,i])){
      flag=0
    }
  }
  if(flag==0){
    col_blank[C]=i 
    C=C+1
    flag=1
  }
}
col_blank
data_without_blank=data[,-c(col_blank)]


numCol = dim(data_without_blank)[2]
numRow = dim(data_without_blank)[1]

#Remove Constant
col_constant=c()
C=1

for (i in  1:numCol){
  L=length(unique(data_without_blank[,i]))
  
  if(L == 1){
    #print(L)
    col_constant[C]=i 
    C=C+1    
  }
}

col_constant
data_without_blank_constant=data_without_blank[,-c(col_constant)]



numCol = dim(data_without_blank_constant)[2]
numRow = dim(data_without_blank_constant)[1]

View(data_without_blank_constant)

col_cor=c()
C=1

print(numCol)
limit=numCol-1

for(i in 4:limit){
  r=i+1
  for(p in r:numCol){
    
    CR=cor(data_without_blank_constant[,i],data_without_blank_constant[,p])
    
    if(CR*CR > 0.5){
      print(paste("I value: ",i,"Correlation between ",names(data_without_blank_constant[i])," and ",names(data_without_blank_constant[p]), " = ",C))
      col_cor[C]=i
      print(col_cor[C])
      C=C+1 
    }
  }
}
col_cor
data_without_blank_constant_cor=data_without_blank_constant[,-c(col_cor)]

View(data_without_blank_constant_cor)

data_without_blank_constant_cor$id=NULL

nRow = dim(data_without_blank_constant_cor)[1]
print(nRow)

train = sample(nRow,0.7*nRow)
length(train)


test = nRow %>%
  seq_len() %>%
  setdiff(train)


length(test)

trainSet = data_without_blank_constant_cor[train,]
testSet  = data_without_blank_constant_cor[test,]

View(testSet)

library(rpart)

fit = rpart(Quality~price+bedrooms+condition+zipcode+sqft_living15,method="class",data=trainSet)

prediction = predict(fit,testSet,type='class')


table(testSet$Quality,prediction)


fit = rpart(Quality~bedrooms+price+zipcode+sqft_living15+condition,method="class",data=trainSet)

prediction = predict(fit,testSet,type='class')


table(testSet$Quality,prediction)


library(randomForest)

fit = randomForest(price~bedrooms+sqft_living15,data=trainSet)

prediction = predict(fit,testSet)
prediction

cor(testSet$price, prediction)
plot(testSet$price, prediction)

######seventh-code#####
library(dplyr)
library(rpart)
library(randomForest)

library(caret)

View(iris)


data=iris
unique(iris$Species)

obs=nrow(data)

0.7*obs
train = sample(obs,0.7*obs)
test = obs %>%
  seq_len() %>%
  setdiff(train)

testset = data[test,]
trainset = data[train,]

model = rpart(Species~.,data=trainset,method='class')
prediction_DT = predict(model,testset,type='class')

prediction_DT


confusionMatrix(prediction_DT,testset$Species)


model = randomForest(Species~.,data=trainset)
prediction_RF = predict(model,testset)

#prediction_DT


confusionMatrix(prediction_RF,testset$Species)

dataset = read.csv("social.csv")
View(dataset)

dataset$User.ID=NULL
dataset$Gender = NULL

obs=nrow(dataset)

0.7*obs
train = sample(obs,0.7*obs)
test = obs %>%
  seq_len() %>%
  setdiff(train)
testset = dataset[test,]
trainset = dataset[train,]


library(e1071) 
model_svm = svm(Purchased~.,data=trainset,type='C-classification',kernal='linear')
p = predict(model_svm, testset)

confusionMatrix(table(p,testset$Purchased))


model_dt = rpart(Purchased~.,data=trainset,method='class')
p = predict(model_dt, testset)

length(p)
length(testset$Purchased)  


######8th-code####
#data_cleaning_and_lm.R

copy_of_aq = airquality
all_na_location = which (is.na(copy_of_aq), arr.ind = T)

na_replaced = copy_of_aq
# Decision 1. - replace NA with pre-decided value : could be zero or could be mean of the column
na_replaced [all_na_location] = 0
copy_of_aq
na_replaced
head (airquality)

# Decision 2. - delete all the rows with NA

removed_na = airquality[-all_na_location[,1],]
dim(removed_na)
dim (airquality) 

# Decision 3. Remove the columns that are constant
names(copy_of_aq)
length(unique(copy_of_aq$Month))

unique(copy_of_aq)
# introduce constant column

constant_added = copy_of_aq
constant_added$const = 20
names(constant_added)
unique(constant_added$const)
names(constant_added) [apply(constant_added,2,function(x){length(unique(x))}) == 1]

# writing dataframe in file

write.table(copy_of_aq, file = "copy_of_aq.tsv", sep = "\t",
            row.names = F, col.names = T, quote = T)

retrieved_from_file = read.table("ex.tsv", header = F, sep = "\t") #fix this

########################## Assignment ####################  

# Decision 4. Remove the columns that are almost constant - quasi constant
# This is a practice assignment. Use the file `data_with_const_and_quasi_constant_value.tsv`. 
# The final dataframe should match `airquality` dataframe. 

aq = read.table("data_with_const_and_quasi_constant_value.tsv", sep = "\t", header = T)

threshold = 0.8
# The idea is to first calculate the frequency of the values, and extract the maximum. If it is greater than the threshold then remove it. 

columns_to_remove = names(aq)[(apply(aq,2,function(x){max(table(x)/length(x))}) > threshold)]

columns_to_remove

aq_after_cols_removed = aq[, !(names(aq) %in% columns_to_remove)]
aq_after_cols_removed == airquality

# Replacing NA with mean of the corresponding column - use the `airquality` dataframe.
# Write out your approach first and then write the code.

apply(aq_after_cols_removed, 2, mean) # when not removing NA, you will see mean of Ozone and Solar.R is NA

mean_of_columns = apply(aq_after_cols_removed, 2, mean, na.rm = T) # this is how you can pass the arguments of the function

# Now get mean values for all locations of NA 

all_na_location = which (is.na(aq_after_cols_removed), arr.ind = T)

# the 2nd column in the all_na_location tells you the column id where NA is located, 
# and that has relation with mean_of_columns. So I just access them to create a vector of values to fill NA

vals = mean_of_columns[all_na_location[,2]]

aq_after_cols_removed[all_na_location] = vals

# Now check the Ozone (5,1), and Solar.R (5,2; 6,2). Match it with the mean value


# Reading a file with both column and row-names
# Use the file `airquality_rt_ct.tsv` for reading.


df_from_file_with_row_and_col_id = read.table("airquality_rt_ct.tsv", header = T)
df_from_file_with_row_and_col_id == airquality


######9th-code#####
library(ggplot2)
library(dplyr)

data = read.csv("out-smiles.csv",header=TRUE)

data$SMILES=NULL
View(data)

numCol = dim(data)[2]
numRow = dim(data)[1]

#Remove the blank/NA column

flag=1
col_blank=c()
C=1
for(i in 1:numCol){
  for(p in 1:numRow){
    if(is.na(data[p,i])){
      flag=0
    }
  }
  if(flag==0){
    col_blank[C]=i 
    C=C+1
    flag=1
  }
}
col_blank
data_without_blank=data[,-c(col_blank)]



numCol = dim(data_without_blank)[2]
numRow = dim(data_without_blank)[1]

#Remove Constant 
col_constant=c()
C=1

for (i in  1:numCol){
  L=length(unique(data_without_blank[,i]))
  
  if(L == 1){
    #print(L)
    col_constant[C]=i 
    C=C+1    
  }
}

col_constant
data_without_blank_constant=data_without_blank[,-c(col_constant)]



numCol = dim(data_without_blank_constant)[2]
numRow = dim(data_without_blank_constant)[1]

View(data_without_blank_constant)

col_cor=c()
C=1

print(numCol)
limit=numCol-1

for(i in 4:limit){
  r=i+1
  for(p in r:numCol){
    
    CR=cor(data_without_blank_constant[,i],data_without_blank_constant[,p])
    
    if(CR*CR > 0.5){
      print(paste("I value: ",i,"Correlation between ",names(data_without_blank_constant[i])," and ",names(data_without_blank_constant[p]), " = ",C))
      col_cor[C]=i
      print(col_cor[C])
      C=C+1 
    }
  }
}
col_cor
data_without_blank_constant_cor=data_without_blank_constant[,-c(col_cor)]

View(data_without_blank_constant_cor)

#data_without_blank_constant_cor$id=NULL

nRow = dim(data_without_blank_constant_cor)[1]
print(nRow)

train = sample(nRow,0.7*nRow)
length(train)


test = nRow %>%
  seq_len() %>%
  setdiff(train)


length(test)

trainSet = data_without_blank_constant_cor[train,]
testSet  = data_without_blank_constant_cor[test,]

View(testSet)

library(randomForest)

fit = randomForest(pIC50~.,data=trainSet)

prediction = predict(fit,testSet)
prediction

cor(testSet$pIC50, prediction)
plot(testSet$pIC50, prediction)



#####10thcode######







