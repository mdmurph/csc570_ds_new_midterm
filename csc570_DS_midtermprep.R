#csc570 DS midtermprep

#print(getwd())
setwd("//ad.uillinois.edu/engr/USERS/mdmurph/WORKING FILES/OL_ED/UIS MCS/2016 Fall/csc570 data science essentials/new midterm")

#1 load the dataset
train <- read.csv("midterm_train.csv", stringsAsFactors = + FALSE)

#2 overall structure of the dataset using str()
str(train)

#4 delete the rows with missing values in horsepower
train<-na.omit(train)
str(train)

#7 select list of variables to drop from the model
drop.cols <- c("x0", "x37",
               "x1",
               "x10",
               "x11",
               "x13",
               "x14",
               "x15",
               "x16",
               "x17",
               "x18",
               "x19",
               "x21",
               "x22",
               "x24",
               "x25",
               "x26",
               "x27",
               "x28",
               "x29",
               "x3",
               "x30",
               "x31",
               "x32",
               "x33",
               "x34",
               "x35",
               "x36",
               "x39",
               "x4",
               "x43",
               "x44",
               "x45",
               "x47",
               "x48",
               "x5",
               "x7",
               "x8",
               "x9") #to remove multiple columns from model

train<-train[, !names(train) %in% drop.cols, drop = F]

str(train)

#use random forest
library(randomForest)
set.seed(300)

rf <- randomForest(y ~ x2+x6+x12+x20+x23+x38+x40+x41+x42+x46+x49, data = train)
rf

#install.packages("caret")  #one time install
library(caret)
ctrl <- trainControl(method = "repeatedcv",
                     number = 10, repeats = 10)

grid_rf <- expand.grid(.mtry = c(2, 4, 8, 16))

set.seed(300)
m_rf <- train(data = train, method = "rf",
              metric = "Kappa", trControl = ctrl,
              tuneGrid = grid_rf)

m_rf

getTree(rf,500, labelVar=TRUE)
importance(rf)
