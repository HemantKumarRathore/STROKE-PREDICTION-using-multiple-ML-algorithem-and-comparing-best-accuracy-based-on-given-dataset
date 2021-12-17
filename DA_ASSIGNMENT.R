library("dplyr")
library("stringr")

#Reading data.frame from csv file 
df=read.csv("C:/Users/ratho/Documents/DA PROJECT/StrokePredictionDataset.csv",header=T)
View(head(df,5))
str(df)
summary(df)

# checking and removing unwanted spaces in column entry.
for(i in 1:ncol(df)){
  df[,i]=as.character(df[,i]);
}
df=df %>% mutate_if(is.character, str_trim)
df[,1]=as.character(df[,1]);
df[,2]=as.numeric(df[,2]);
df[,3]=as.numeric(df[,3]);
df[,4]=as.numeric(df[,4]);
df[,5]=as.numeric(df[,5]);
df[,6]=as.character(df[,6]);
df[,7]=as.character(df[,7]);
df[,8]=as.numeric(df[,8]);
df[,9]=as.numeric(df[,9]);
df[,10]=as.numeric(df[,10]);
View(df)

# creating empty numeric vector to store column which is having 'NA' values
na.col=c();
# Checking weather 'NA' value is present in data.frame or not, if it is having 
# then storing column number in na.col vector
if(any(is.na(df))){
  for(i in 1:ncol(df)){
    if(any(is.na(df[,i]))){
      na.col=c(na.col,i)
    }
  }
  cat("Columns which are having 'NA' values: ",na.col,"\n")
}else{
  print("There is no 'NA' Values.")
}
# if na.col vector is not empty, then removing 'NA' values 
# by replacing with column mean
if(length(na.col)!=0){
  for(i in na.col){
    df[,i][is.na(df[,i])]=mean(df[,i],na.rm = T);
  }
}
# printing first 5 entry after removing NA values
View(head(df,5))

#Normalizing numerical column value between 0 and 1 using min-max Normalization
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

for(i in 1:ncol(df)){
  if(class(df[,i])=='numeric'){
    df[,i]=min_max_norm(df[,i])
  }
}
View(df)

pie(table(df[,3][df$stroke==0]),
    main = "Hyper-Tension (for stroke=0)",
    col = rainbow(length(table(df[,3][df$stroke==0]))))
pie(table(df[,3][df$stroke==1]),
    main = "Hyper-Tension (for stroke=1)",
    col = rainbow(length(table(df[,3][df$stroke==1]))))

pie(table(df[,4][df$stroke==0]),
    main = "Heart Disease (for stroke=0)",
    col = rainbow(length(table(df[,4][df$stroke==0]))))
pie(table(df[,4][df$stroke==1]),
    main = "Heart Disease (for stroke=1)",
    col = rainbow(length(table(df[,4][df$stroke==1]))))

pie(table(df[,5][df$stroke==0]),
    main = "Ever Married (for stroke=0)",
    col = rainbow(length(table(df[,5][df$stroke==0]))))
pie(table(df[,5][df$stroke==1]),
    main = "Ever Maried (for stroke=1)",
    col = rainbow(length(table(df[,5][df$stroke==1]))))

pie(table(df[,6][df$stroke==0]),
    main = "Work Type (for stroke=0)",
    col = rainbow(length(table(df[,6][df$stroke==0]))))
pie(table(df[,6][df$stroke==1]),
    main = "Work Type (for stroke=1)",
    col = rainbow(length(table(df[,6][df$stroke==1]))))

pie(table(df[,7][df$stroke==0]),
    main = "Residence Type (for stroke=0)",
    col = rainbow(length(table(df[,7][df$stroke==0]))))
pie(table(df[,7][df$stroke==1]),
    main = "Residence Type (for stroke=1)",
    col = rainbow(length(table(df[,7][df$stroke==1]))))

boxplot(df[,c("age","avg_glucose_level","bmi")],
        names = c("AGE","AVG GLUCOSE LEVEL","BMI"), 
        varwidth = TRUE,
        col = c("green","yellow","purple"))
#remove outlying


View(df)

library(e1071)
library(caTools)
library(class)

# Splitting data into train
# and test data
split <- sample.split(df, SplitRatio = 0.7)
train_cl <- subset(df, split == "TRUE")
test_cl <- subset(df, split == "FALSE")

# Feature Scaling
train_scale <- (train_cl[, c(2:5,8,9)])
test_scale <- (test_cl[, c(2:5,8,9)])


classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$stroke,
                      k = 59)
classifier_knn

# Confusion Matrix
cm <- table(test_cl$stroke, classifier_knn)
cm

