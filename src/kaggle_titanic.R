# Titanic ML Practice
# Date: 02/19/2018
# Author: Hongfei Yang

rm(list=ls(all=TRUE))  # Clear Workspace

# Load Libraries
library("tidyverse")

# Import Data
setwd("Users/Derek/Kaggle-Titanic-ML")
train.dat <- read.csv("./input/train.csv", stringsAsFactors = FALSE)
test.dat <- read.csv("./input/test.csv", stringsAsFactors = FALSE)