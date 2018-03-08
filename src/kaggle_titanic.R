# Titanic ML Practice
# Date: 02/19/2018
# Author: Hongfei Yang

rm(list=ls(all=TRUE))  # Clear workspace

library("tidyverse")  # Package for data wrangling 
library("ggplot2")    # Package for data visualization
library("stringr")    # Package for string manipulation
library('caret')      # Package for CV and Tuning

# Import data
setwd("/Users/Derek/Kaggle-Titanic-ML")
train.dat <- read.csv("./input/train.csv", stringsAsFactors = FALSE)
test.dat <- read.csv("./input/test.csv", stringsAsFactors = FALSE)


################################################################################


#===============#
# Data cleaning #
#===============#

# Create an aggregated data set which contains the training set without the 
# response variable and the testing set
train.y <- train.dat$Survived
all.dat <- rbind(train.dat[, -2], test.dat)

# Convert '' to NA for Cabin and Embarked
all.dat$Cabin <- ifelse(all.dat$Cabin == '', NA, all.dat$Cabin)
all.dat$Embarked <- ifelse(all.dat$Embarked == '', NA, all.dat$Embarked)

# Convert chr to factor for Pclass, Sex, Cabin and Embarked
for (i in c(2, 4, 10, 11)) {
  all.dat[, i] <- as.factor(all.dat[, i])
}

# Check the number of missing values for all rows
for (i in c(1:ncol(all.dat))) {
  na.count = sum(is.na(all.dat[, i]))
  out = paste(names(all.dat[i]), na.count)
  print(out)
}

# Sex vs. Embarked
all.dat %>% 
  select(Embarked, Sex) %>% 
  group_by(Embarked, Sex) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(x = Sex, y = n, fill = Embarked)) + geom_bar(stat="identity")

# Pclass vs. Embarked
all.dat %>% 
  select(Embarked, Pclass) %>% 
  group_by(Embarked, Pclass) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(x = Pclass, y = n, fill = Embarked)) + geom_bar(stat="identity")

# Fare vs. Pclass
all.dat %>% 
  select(Fare, Pclass) %>% 
  ggplot(aes(x = Fare, color = Pclass)) + geom_density()

# Fill missing values for Embarked
all.dat %>% filter(is.na(Embarked))
all.dat$Embarked <- as.character(all.dat$Embarked)
all.dat$Embarked <- as.factor(ifelse(is.na(all.dat$Embarked), 
                                     'S', 
                                     all.dat$Embarked))
# Fill missing values for Fare
all.dat %>% filter(is.na(Fare))
all.dat$Fare <- ifelse(is.na(all.dat$Fare), 
                       median(all.dat[all.dat$Pclass == '3', ]$Fare, na.rm = TRUE),
                       all.dat$Fare)

# Age vs. Sex
all.dat %>% 
  ggplot(aes(x = Age, color = Sex)) + geom_density()

# Age vs. Pclass
all.dat %>% 
  ggplot(aes(x = Age, color = Pclass)) + geom_density() + facet_grid(. ~ Sex)

# Age vs. Embarked
all.dat %>% 
  ggplot(aes(x = Age, color = Embarked)) + geom_density()

# Fill missing values for Age based on Sex, Pclass and Embarked
all.dat$Age_known <- as.factor(ifelse(is.na(all.dat$Age), 1, 0))
for (i in 1:nrow(all.dat)) {
  if(is.na(all.dat$Age[i])) {
    
    pclass <- all.dat$Pclass[i]
    sex <- all.dat$Sex[i]
    embarked <- all.dat$Embarked[i]
    filtered.dat <- all.dat %>% filter(Pclass == pclass &
                                       Sex == sex &
                                       Embarked == embarked &
                                       !is.na(Age))
    
    all.dat$Age[i] <- mean(filtered.dat$Age)
  }
}


################################################################################


#=====================#
# Feature Engineering #
#=====================#

# Retrieve the first letter of Cabin, name it as Deck. If there is no value
# for Cabin, set Deck as 'Unknown'
all.dat$Cabin <- as.character(all.dat$Cabin)
all.dat$Deck <- sapply(all.dat$Cabin, function(x) {strsplit(x, NULL)[[1]][1]})
all.dat$Deck <- as.factor(ifelse(is.na(all.dat$Deck), 'Unknown', all.dat$Deck))

# Retrieve the titles from the passengers' names, then regroup titles into
# Occupation, Noble and Other.
all.dat$Title <- sapply(all.dat$Name, function(x) {strsplit(strsplit(x, ', ')[[1]][2], '\\. ')[[1]][1]})

occup.titles <- c('Capt', 'Col', 'Dr', 'Major', 'Rev')
noble.titles <- c('Don', 'Dona', 'Jonkheer', 'Lady', 'Sir', 'the Countess')

all.dat$Title <- ifelse(all.dat$Title %in% occup.titles, 'occupation', all.dat$Title)
all.dat$Title <- ifelse(all.dat$Title %in% noble.titles, 'noble', all.dat$Title)
all.dat$Title <- ifelse(!all.dat$Title %in% c('occupation', 'noble'), 'other',all.dat$Title)

all.dat$Title <- as.factor(all.dat$Title)

# Create a family size variable - Fsize
all.dat$Fsize <- all.dat$SibSp + all.dat$Parch

# Normalize Fare
hist(all.dat$Fare)
min_Fare <- min(all.dat$Fare)
max_Fare <- max(all.dat$Fare)
all.dat$Fare_norm <- (all.dat$Fare - min_Fare) / (max_Fare - min_Fare)

# Normalize Age
min_Age <- min(all.dat$Age, na.rm = TRUE)
max_Age <- max(all.dat$Age, na.rm = TRUE)
all.dat$Age_norm <- (all.dat$Age - min_Age) / (max_Age - min_Age)

levels(all.dat$Pclass) <- c('First', 'Second', 'Third')

all.dat$First_class <- ifelse(all.dat$Pclass == 'First', 1, 0)
all.dat$Second_class <- ifelse(all.dat$Pclass == 'Second', 1, 0)
all.dat$Sex_male <- ifelse(all.dat$Sex == 'male', 1, 0)

# Split the data back into training and testing set
train.dat <- cbind(all.dat[c(1:891), ], train.y)
names(train.dat)[21] <- 'Survived'
train.dat$Survived <- as.factor(train.dat$Survived)
test.dat <- all.dat[-c(1:891), ]

levels(train.dat$Survived) <- c('No', 'Yes')



################################################################################


#===============#
# Data Analysis #
#===============#

# Fare vs. Survived
train.dat %>% ggplot(aes(x = Fare_norm, color = Survived)) + geom_density()

# Pclass vs. Survived
train.dat %>% ggplot(aes(x = Pclass, fill = Survived)) + geom_bar(position = 'dodge')

# Sex vs. Survived
train.dat %>% ggplot(aes(x = Sex, fill = Survived)) + geom_bar(position = 'dodge')

# Pclass vs. Survived (Separated by Sex)
train.dat %>% 
  ggplot(aes(x = Pclass, fill = Survived)) +
  geom_bar(position = 'dodge') +
  facet_grid(. ~ Sex)

# Deck vs. Survived
train.dat %>% 
  ggplot(aes(x = Deck, fill = Survived)) + 
  geom_bar(position = 'dodge')

# Deck vs. Survived (Separated by Pclass)
train.dat %>% 
  ggplot(aes(x = Deck, fill = Survived)) + 
  geom_bar(position = 'dodge') +
  facet_grid(Pclass ~ .)

# Recode Deck variable into B-E, Other, Unkonown
all.dat$Deck <- as.character(all.dat$Deck)
all.dat$Deck <- ifelse(all.dat$Deck %in% c('B', 'C', 'D', 'E'),
                       'B_to_E',
                       all.dat$Deck)
all.dat$Deck <- as.factor(ifelse(all.dat$Deck %in% c('A', 'F', 'G', 'T'),
                       'Other',
                       all.dat$Deck))

train.dat$Deck <- all.dat$Deck[1:891]
test.dat$Deck <- all.dat$Deck[892:1309]

# Title vs. Survived
train.dat %>% 
  ggplot(aes(x = Title, fill = Survived)) +
  geom_bar(position = 'dodge')

# Age_known vs. Survived
train.dat %>% 
  ggplot(aes(x = Age_known, fill = Survived)) +
  geom_bar(position = 'dodge')

# Age vs. Survived
train.dat %>% 
  ggplot(aes(x = Age_norm, color = Survived)) + 
  geom_density()

# Age vs. Fsize
train.dat %>% ggplot(aes(x = Age_norm, y = Fsize)) + geom_point()

# Age vs. fare
train.dat %>% ggplot(aes(x = Age_norm, y = Fare_norm)) + geom_point()


################################################################################


#==================#
# Developing Model #
#==================#
set.seed(5)

features <- c('Pclass',
              'Sex',
              'Embarked',
              'Deck',
              'Age_norm',
              'Age_known',
              'Title',
              'Fsize',
              'Fare_norm')

train.Control <- trainControl(method = 'repeatedcv', number = 10, repeats = 3)

# Random Forest
# Feature Selection 
control <- rfeControl(functions = rfFuncs, method = 'cv', repeats = 5)
rf.pred.profile <- rfe(train.dat[, features], 
                       train.dat$Survived,
                       sizes = c(4, 5, 6, 7, 8),
                       rfeControl = control)

# Plot the Learning Curve for the Random Forest Model
learning.curve.df <- learing_curve_dat(dat = train.dat[, c('Survived', 
                                                           'Pclass', 
                                                           'Sex', 
                                                           'Fare_norm', 
                                                           'Fsize', 
                                                           'Age_norm')],
                                       outcome = 'Survived',
                                       test_prop = 1/4,
                                       proportion = seq(0.05, 1, by = 0.05),
                                       method = 'rf',
                                       trControl = train.Control)

learning.curve.df %>% 
  filter(Data %in% c('Training', 'Testing')) %>% 
  select(Accuracy, Training_Size, Data) %>% 
  ggplot(aes(x = Training_Size, y = 1 - Accuracy, color = Data)) +
  ylim(0, 1) +
  geom_smooth(span = 0.7, se = FALSE)

# Fit the RF model
rf.mod <- train(x = train.dat[, c('Pclass', 
                                  'Sex',
                                  'Fare_norm', 
                                  'Fsize',
                                  'Age_norm')],
                y = train.dat$Survived,
                data = train.dat,
                method = 'rf',
                trControl = train.Control)


# GBDT
# Fit the RF model
gbm.mod <- train(Survived ~ .,
                 data = train.dat[, c('Survived', 'Pclass', 'Sex','Fare_norm', 'Fsize','Age_norm')],
                 method = 'gbm',
                 trControl = train.Control)


# SVM
# Feature Selection 
control <- rfeControl(functions = svmFuncs, method = 'cv', repeats = 5)
svm.pred.profile <- rfe(train.dat[, features], 
                        train.dat$Survived,
                        sizes = c(4, 5, 6, 7, 8),
                        rfeControl = control)

# Plot the Learning Curve for the SVM Model with Linear Kernal
learning.curve.df <- learing_curve_dat(dat = train.dat[, c('Survived', 
                                                           'Sex_male',
                                                           'First_class',
                                                           'Second_class',
                                                           'Fare_norm', 
                                                           'Age_norm',
                                                           'Fsize')],
                                       outcome = 'Survived',
                                       test_prop = 1/4,
                                       proportion = seq(0.05, 1, by = 0.05),
                                       method = 'svmRadial',
                                       trControl = train.Control)

learning.curve.df %>% 
  filter(Data %in% c('Training', 'Testing')) %>% 
  select(Accuracy, Training_Size, Data) %>% 
  ggplot(aes(x = Training_Size, y = 1 - Accuracy, color = Data)) +
  ylim(0, 1) +
  geom_smooth(span = 0.7, se = FALSE)

# Fit the SVM model
svm.mod <- train(Survived ~ .,
                 data = train.dat[, c('Survived', 
                                      'First_class', 
                                      'Second_class',
                                      'Sex_male',
                                      'Fare_norm',
                                      'Age_norm',
                                      'Fsize')],
                 method = 'svmRadial',
                 trControl = train.Control)


################################################################################


#=============#
# Prediction  #
#=============#

rf.pred <- as.integer(predict(rf.mod, test.dat)) - 1    # RF predictions
gbm.pred <- as.integer(predict(gbm.mod, test.dat)) - 1  # GBM predictions
svm.pred <- as.integer(predict(svm.mod, test.dat)) - 1  # SVM predictions

# Caculate correlation between predictions from different models
pred.mat <- matrix(c(rf.pred, gbm.pred, svm.pred), nrow = 418, ncol = 3)

final.pred <- round((rf.pred + gbm.pred + svm.pred) / 3)

pred.df <- data.frame(PassengerId = test.dat$PassengerId,
                     Survived = final.pred)

write.csv(x = pred.df,
          file = 'Titanic_ML_Output.csv',
          row.names = FALSE)
