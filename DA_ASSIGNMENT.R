#importing some important library to be used
library("dplyr")
library("stringr")
library("e1071")
library("caTools")
library("class")
library("GGally")
library("ggplot2")
library("caret")

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
df[,10]=as.factor(df[,10]);
View(head(df,5))

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
View(head(df,5))

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

#converting Character discrete value to a suitable discrete value
for(i in 1:ncol(df)){
  if(class(df[,i])=='character'){
    for(j in 1:nrow(df)){
      if(df[j,i]=="Male"){
        df[j,i]="1"
      }else if(df[j,i]=="Female"){
        df[j,i]="2"
      }else if(df[j,i]=="Other"){
        df[j,i]="3"
      }else if(df[j,i]=="children"){
        df[j,i]="1"
      }else if(df[j,i]=="Govt_job"){
        df[j,i]="2"
      }else if(df[j,i]=="Never_worked"){
        df[j,i]="3"
      }else if(df[j,i]=="Self-employed"){
        df[j,i]="4"
      }else if(df[j,i]=="Private"){
        df[j,i]="5"
      }else if(df[j,i]=="Urban"){
        df[j,i]="1"
      }else if(df[j,i]=="Rural"){
        df[j,i]="2"
      }
    }
    df[,i]=as.factor(df[,i])
  }
}

View(head(df,5))


# Splitting data into train and test data
split <- sample.split(df$stroke, SplitRatio = 0.8)
train_cl <- subset(df, split == "TRUE")
test_cl <- subset(df, split == "FALSE")

# Implementing KNN:

# As we Know from ML(machine learning) concept, K can be assumed as 
#sqrt(nrow(data.frame_train)) and also it has to be odd to remove 
#draw case in decision. 
K=as.integer(sqrt(nrow(train_cl)))
if(K%%2==0){
  K=K+1
}
# Fitting KNN Model to training data-set
classifier_knn <- knn(train = train_cl,
                      test = test_cl,
                      cl = train_cl$stroke,
                      k = K)
print(classifier_knn)
# Confusion Matrix
cm <- table(test_cl$stroke, classifier_knn)
paste("confusion Marix for KNN: ")
print(cm)
# Accuracy
misClassError.KNN <- mean(classifier_knn != test_cl$stroke)
KNN.Accu=1-misClassError
print(paste('Accuracy for KNN is ', KNN.Accu))


# Implementing SVM:
# Fitting SVM Model to training data-set
svm_model <- svm(stroke ~ ., data=train_cl,
                 kernel="sigmoid")
# ploting data
ggpairs(df, ggplot2::aes(colour = stroke, alpha = 0.4))
# predicting test data and drawing Confusion matrix
pred.svm = predict(svm_model,test_cl)
tab.svm = table(Predicted=pred.svm, Actual = test_cl$stroke)
print("Confusion Matrix: ")
print(tab.svm)
# Accuracy
SVM.Accu=sum(diag(tab.svm)/sum(tab.svm))
print(paste("Accuracy for SVM is ",SVM.Accu))


# Implementing Naive Bayes:
# Fitting Naive Bayes Model to training data-set
model.nb = train(train_cl[,1:9],train_cl$stroke,'nb',trControl=trainControl(method='cv',number=10))
print(model.nb)
# predicting test data and drawing Confusion matrix
con.matrix.nb=table(predict(model.nb$finalModel,test_cl)$class,test_cl$stroke)
print("Confusion Matrix: ")
print(con.matrix.nb)
# Accuracy
NB.Accu=sum(diag(con.matrix.nb)/sum(con.matrix.nb))
print(paste("Accuracy: ",NB.Accu))

#comparing accuracy of different model and suggesting best model which best
#fits training data-set
if(KNN.Accu>=SVM.Accu && KNN.Accu>NB.Accu){
  print(paste("Accuracy of KNN is heigher then that of other model with Accuacy of",KNN.Accu))
}else if(SVM.Accu>=KNN.Accu && SVM.Accu>NB.Accu){
  print(paste("Accuracy of SVM is heigher then that of other model with Accuacy of",SVM.Accu))
}else{
  print(paste("Accuracy of Naive Bayes is heigher then that of other model with Accuacy of",NB.Accu))
}

