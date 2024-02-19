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

#create_report(features)

n_distinct(stores$Store) #checking store Ids 

n_distinct(features$Date) #checking before merging with sales

n_distinct(sales$Date)
n_distinct(sales$Store)
n_distinct(sales$Dept)

sum(is.na(sales))
sum(is.na(features))
sum(is.na(sales))

# merge features and sales data frames sorted by Dates
features$Date <- as.Date(features$Date, "%d/%m/%Y")
sales$Date <- as.Date(sales$Date, "%d/%m/%Y")

features_date1 <- aggregate(
  list("Temperature" = features$Temperature, 
       "Fuel_Price" = features$Fuel_Price, 
       "CPI" = features$CPI, 
       "Unemployment" = features$Unemployment),
  by = list("Date" = features$Date),
  FUN = mean, 
  na.rm = TRUE
)
 
features_date2 <- aggregate(features[, c("Temperature", "Fuel_Price", "CPI", "Unemployment")],  list(features$Date),  mean, na.rm = TRUE)

df1_unique <- anti_join(features_date1, features_date2)

features_date1 <- features_date1 %>% 
  mutate(aggregate(list("IsHoliday" = features$IsHoliday), by = list("Date" = features$Date), FUN = sum, na.rm = TRUE))

sales_date <- aggregate(list("Weekly_Sales" = sales$Weekly_Sales), by= list("Date" = sales$Date), FUN=sum, na.rm = TRUE)

# Converting sales into Millions
sales_date$Weekly_Sales <- as.integer(sales_date$Weekly_Sales / 1000000)

# Merging the features_date and sales_date datasets
sales_complete <- merge(sales_date, features_date1, by = "Date", all.sales_date = TRUE)
head(sales_complete)
sum(is.na(sales_complete))

df2_unique <- anti_join(features_date1, sales_date, by = "Date")
df3_unique <- anti_join(sales_date, features_date1, by = "Date")

create_report(sales_complete, output_file = "sales_complete_report.html")

# build a density plot
sales_complete %>% plot_density(ncol = 3, ggtheme = theme_minimal())
# build correlation plot
corPlot(sales_complete[,2:7], upper = FALSE, scale = FALSE, main = "Correlation in Sales Attributes")

plot_sales <- gather(sales_complete, key = "attribs", value = "Value", -Date, -IsHoliday) #Gather columns into key-value pairs

ggplot(plot_sales, aes(Date, Value)) + geom_line(aes(color = Value), linewidth = 1) + facet_grid(attribs~., scales = "free_y", #adjust scales
                                                                                                 switch = "y") + #switch y-axis labels to left
  ylab(NULL) + #remove the label "Value"
  theme(strip.background = element_blank(), #remove the background
        strip.placement = "outside", strip.text.y.left = element_text(angle = 0), legend.position = "none") + scale_x_date(date_breaks = "5 months", date_labels = '%Y-%m')
