library(tidyverse)
library(lubridate)
library(janitor)
library(skimr)
library(psych)
library(ggplot2)
library(reshape2)
library(DataExplorer)
library(scales)

features<-read.csv("Features_data_set.csv", header = TRUE)
sales<-read.csv("sales_data_set.csv", header = TRUE)
stores<-read.csv("stores_data_set.csv", header = TRUE)

head(stores)
head(features)
head(sales)

glimpse(stores)
glimpse(features)
glimpse(sales)

create_report(features)
