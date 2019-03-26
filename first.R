library(stringr)
library(tidyr)
library(ggplot2)
library(xgboost)
library(Matrix)
library(magrittr)
library(plyr)
library()

train=read.csv("train 2.csv")
test=read.csv("test 2.csv")
attach(train)
attach(test)

# summary(train)
# summary(test)
train=subset(train, !Product_Category_1 %in% c(19,20))
# colSums(is.na(train))
# colSums(is.na(test))

View(train)
# View(test)

#removing + sign from column 
train$Stay_In_Current_City_Years=str_replace_all(train$Stay_In_Current_City_Years,"[+]","")
test$Stay_In_Current_City_Years=str_replace_all(test$Stay_In_Current_City_Years,"[+]","")

#cleaning age column in train
train$Age=str_replace_all(train$Age,"0-17",15)
train$Age=str_replace_all(train$Age,"18-25",22)
train$Age=str_replace_all(train$Age,"26-35",30)
train$Age=str_replace_all(train$Age,"36-45",40)
train$Age=str_replace_all(train$Age,"46-50",48)
train$Age=str_replace_all(train$Age,"51-55",53)
train$Age=str_replace_all(train$Age,"55+",60)
train$Age=str_replace_all(train$Age,"[+]","")

#cleaning age column in test
test$Age=str_replace_all(test$Age,"0-17",15)
test$Age=str_replace_all(test$Age,"18-25",22)
test$Age=str_replace_all(test$Age,"26-35",30)
test$Age=str_replace_all(test$Age,"36-45",40)
test$Age=str_replace_all(test$Age,"46-50",48)
test$Age=str_replace_all(test$Age,"51-55",53)
test$Age=str_replace_all(test$Age,"55+",60)
test$Age=str_replace_all(test$Age,"[+]","")

#converting Gender to binary
train$Gender=str_replace_all(train$Gender,"M",1)
train$Gender=str_replace_all(train$Gender,"F",0)

test$Gender=str_replace_all(test$Gender,"M",1)
test$Gender=str_replace_all(test$Gender,"F",0)

# One Hot Encoding City category
library(dummies)

train=dummy.data.frame(train, names = 'City_Category')
test=dummy.data.frame(test, names = "City_Category")


###     FEATURE ENGINEERING     ###

# number of times distinct user visited
usercount=ddply(train, .(User_ID), nrow)
names(usercount)[2]= "USER_Count"
train=merge(train, usercount, by="User_ID")
test=merge(test,usercount, by="User_ID")

#expenditure per user
totalpurchase=ddply(train, .(User_ID), summarize, tot=sum(Purchase))
names(totalpurchase)[2]="Total_User_Purchase"
train=merge(train, totalpurchase, by="User_ID")
test=merge(test, totalpurchase,all.x = T ,by="User_ID")

# revenue per product
totalproduct=ddply(train, .(Product_ID), summarize, tota=sum(Purchase))
names(totalproduct)[2]="Product_Revenue"
train=merge(train, totalproduct, by="Product_ID")
test=merge(test, totalproduct, all.x = T ,by="Product_ID")

# no. of times distinct product was taken
productcount=ddply(train, .(Product_ID), nrow)
names(productcount)[2]= "Product_Count"
train=merge(train, productcount, by="Product_ID")
test=merge(test, productcount,all.x = T ,by="Product_ID")

# Revenue by occupation
occupatu=ddply(train, .(Occupation),summarize, totalo=sum(Purchase))
names(occupatu)[2]="Occupation Revenue"
train=merge(train, occupatu, by="Occupation")
test=merge(test, occupatu,all.x = T ,by="Occupation")

# Revenue by age
whiage=ddply(train, .(Age),summarize ,total=sum(Purchase))
names(whiage)[2]="Age_group_Revenue"
train=merge(train, whiage, by="Age")
test=merge(test, whiage,all.x = T ,by="Age")

# revenue by current stay 
currenntstay=ddply(train, .(Stay_In_Current_City_Years), summarize, sum(Purchase))
names(currenntstay)[2]="Revenue-by-Stay_in_city"
train=merge(train, currenntstay, by="Stay_In_Current_City_Years")
test=merge(test, currenntstay,all.x = T ,by="Stay_In_Current_City_Years")

# Mean purchase of product
meanpurchase=ddply(train, .(Product_ID), summarize, mea=mean(Purchase))
names(meanpurchase)[2]="Mean_Purchase_of_Product"
train=merge(train, meanpurchase, by="Product_ID")
test=merge(test, meanpurchase,all.x = T ,by="Product_ID")

#Mean Purchase of User
meanuser=ddply(train, .(User_ID), summarize, mea=mean(Purchase))
names(meanuser)[2]="Mean_Purchase_of_Product"
train=merge(train, meanuser, by="User_ID")
test=merge(test, meanuser,all.x = T ,by="User_ID")


# str(train)
# str(test)

train$Stay_In_Current_City_Years=as.integer(train$Stay_In_Current_City_Years)
train$Age=as.integer(train$Age)
train$Gender=as.integer(train$Gender)

test$Stay_In_Current_City_Years=as.integer(test$Stay_In_Current_City_Years)
test$Age=as.integer(test$Age)
test$Gender=as.integer(test$Gender)

submission= test[,c("User_ID","Product_ID")]
op=train$Purchase

train=subset(train, select = -c(Product_ID, Purchase))
test=subset(test, select = c(colnames(train)) )

train[]=lapply(train, as.numeric)
test[]=lapply(test, as.numeric)


# mod=xgboost(data = as.matrix(train),
#             label = op,
#             objective="reg:linear",nrounds=5000,max.depth=10,eta=0.01,colsample_bytree=0.5,subsample=1,
#             seed=235,metric="rmse",importance=1,missing='NA')

mod=xgboost(data = as.matrix(train),
            label = op,
            objective="reg:linear",nrounds=50000,max.depth=10,eta=0.001,colsample_bytree=0.5,subsample=1,
            seed=235,metric="rmse",importance=1,missing='NA')

pred=predict(mod, as.matrix(test), outputmargin=TRUE,missing='NA')

View(submission)
submission$Purchase=pred

write.csv(submission, "su101.csv", row.names=F)
#233599

