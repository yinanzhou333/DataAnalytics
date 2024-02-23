library(tidyverse)
library(lubridate)
library(janitor)
library(skimr)
library(psych)
library(ggplot2)
library(reshape2)
library(DataExplorer)
library(scales)
library(dplyr)

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

# convert a column named Date to the Date class. 
features$Date <- as.Date(features$Date, "%d/%m/%Y")
sales$Date <- as.Date(sales$Date, "%d/%m/%Y")

# merge features and sales data frames sorted by Dates
features_date1 <- aggregate(
  list("Temperature" = features$Temperature, 
       "Fuel_Price" = features$Fuel_Price, 
       "CPI" = features$CPI, 
       "Unemployment" = features$Unemployment), # creates a list of vectors
  by = list("Date" = features$Date), # specifies the grouping variable(The data will be grouped by the "Date" variable).
  FUN = mean, # indicates the function to be applied to each group
  na.rm = TRUE
)
#OR
features_date2 <- features %>%
  group_by(Date) %>%
  summarize(
    Temperature = mean(Temperature, na.rm = TRUE),
    Fuel_Price = mean(Fuel_Price, na.rm = TRUE),
    CPI = mean(CPI, na.rm = TRUE),
    Unemployment = mean(Unemployment, na.rm = TRUE)
  )
#OR
features_date3 <- aggregate(features[, c("Temperature", "Fuel_Price", "CPI", "Unemployment")],  list("Date" = features$Date),  mean, na.rm = TRUE)

df1_unique <- anti_join(features_date1, features_date2)

features_date1 <- features_date1 %>% 
  mutate(aggregate(list("IsHoliday" = features$IsHoliday), by = list("Date" = features$Date), FUN = sum, na.rm = TRUE)) #OR: FUN=mean

sales_date <- aggregate(list("Weekly_Sales" = sales$Weekly_Sales), by= list("Date" = sales$Date), FUN=sum, na.rm = TRUE)

# Converting sales into Millions
sales_date$Weekly_Sales <- as.integer(sales_date$Weekly_Sales / 1000000)

# Merging the features_date and sales_date datasets
sales_complete <- merge(sales_date, features_date1, by = "Date")
head(sales_complete)
sum(is.na(sales_complete))

df2_unique <- anti_join(features_date1, sales_date, by = "Date")
df3_unique <- anti_join(sales_date, features_date1, by = "Date")

#create_report(sales_complete, output_file = "sales_complete_report.html")

# build a density plot
sales_complete %>% plot_density(ncol = 3, ggtheme = theme_minimal())

# build correlation plot
corPlot(sales_complete[,2:7], upper = FALSE, scale = FALSE, main = "Correlation in Sales Attributes")

plot_sales <- gather(sales_complete, key = "attribs", value = "Value", -Date, -IsHoliday) #Gather columns into key-value pairs

ggplot(plot_sales, aes(Date, Value)) + 
  geom_line(aes(color = Value), linewidth = 1) + # Maps the color aesthetic to the "Value" variable
  facet_grid(attribs~., scales = "free_y", switch = "y") + # Adds facets to the plot, creating a grid of subplots
  # specifies the formula for facets, where "attribs" is on the y-axis
  # adjust scales of the y-axis independently for each facet 
  # switch y-axis labels to left
  ylab(NULL) + #remove the label "Value"
  theme(strip.background = element_blank(), # remove the background of facet labels(CPI, Fuel_price, etc.)
        strip.placement = "outside", # Places the facet labels outside the plotting area.
        strip.text.y.left = element_text(angle = 0), # Sets the angle of y-axis facet labels to 0 degrees (horizontal)
        legend.position = "none") + # Removes the legend.
  scale_x_date(date_breaks = "5 months", date_labels = '%Y-%m') # Sets breaks in the x-axis at intervals of 5 months. # Formats the date labels to display the year and month.

# further check if there is any specific months when the weekly sales are generally higher
sales_monthly <- sales_complete %>%
  group_by(month = lubridate::floor_date(Date, "month")) %>% # or: group_by(month = month(Date)) 
  # the :: is the double-colon operator in R, used to access functions or variables from a specific package without having to load the entire package into the global namespace.
  # the floor_date function from the lubridate package to round down the Date variable to the beginning of the month.
  summarize("Monthly_Sales" = sum(Weekly_Sales))

sales_monthly <- sales_monthly %>% 
  mutate(Month = as.integer(lubridate::month(month)), Year = lubridate::year(month)) %>% 
  group_by(Month) %>%
  summarize("Sales" = sum(Monthly_Sales))

ggplot(sales_monthly, aes(x = Month, y = Sales, fill = as.factor(Sales))) + 
  geom_col() +
  guides(fill="none") + # equivalent to: theme(legend.position = "none"), which also removes the legend.
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12)) + 
  scale_fill_discrete() + 
  labs(title = "Monthly Sales")+ 
  theme(plot.title = element_text(hjust = 0.5))
  
sales_yearly <- sales_complete %>%
  group_by(Year = lubridate::floor_date(Date, "year")) %>% #or: group_by(Year=year(Date)) 
  summarise(Sales= sum(Weekly_Sales))

ggplot(sales_yearly, aes(x = Year, y = Sales, fill=as.factor(Sales))) + 
  geom_col() + 
  guides(fill="none") + 
  scale_fill_discrete() + 
  labs(title = "Yearly Sales")+ 
  theme(plot.title = element_text(hjust = 0.5))

# further investigate if there is any trend or correlation with different store types.
stores_agg <- aggregate(list("Tempreture"=features$Temperature, "Fuel_Price"= features$Fuel_Price),
                        by= list("Store"=features$Store), FUN=mean, na.rm=TRUE)
stores_agg <- stores_agg %>% 
  mutate(aggregate(list("IsHoliday" = features$IsHoliday), 
                   by = list("Store" = features$Store), FUN=sum, na.rm = TRUE))

temp_store <- aggregate(list("Weekly_Sales" = sales$Weekly_Sales), 
                        by = list("Store" = sales$Store), FUN=sum, na.rm = TRUE)

stores <- stores %>% mutate("Temp" = stores_agg$Temperature) 
stores <- stores %>% mutate("Fuel_Price" = stores_agg$Fuel_Price) 
stores <- stores %>% mutate("Holiday" = stores_agg$IsHoliday) 
stores <- stores %>% mutate("Weekly_Sales" = temp_store$Weekly_Sales)

str(stores)
stores$Type2 <- as.numeric(factor(stores$Type),levels = c('A', 'B', 'C'))
corPlot(stores[,3:7], upper = FALSE, scale = FALSE, main = "Correlation in Stores Attributes")

ggplot(stores, aes(x=Type, y=Size, fill=Type)) +
  geom_col() + 
  guides(fill="none") +
  scale_y_continuous(labels=comma) # format the y-axis tick labels using a comma as a thousand separator

ggplot(stores, aes(x = Type, y = Weekly_Sales, fill = Type))+ 
  geom_boxplot() + 
  guides(fill="none") +
  scale_y_continuous(labels=comma) 

# Checking number of unique departments
n_distinct(sales$Dept)

#Grouping departments by sales
dept_data <- aggregate(list("Weekly_Sales" = sales$Weekly_Sales), 
                       by = list("Dept" = sales$Dept), FUN=sum, na.rm = TRUE)

dept_data$Weekly_Sales <- as.integer(dept_data$Weekly_Sales / 1000000)

dept_data[order(dept_data$Weekly_Sales),]   

ggplot(dept_data, aes(x = Dept, y = Weekly_Sales, fill = "lightpink")) + 
  geom_col() + 
  labs(title = "Department vs Weekly Sales") + 
  theme(plot.title = element_text(hjust = 0.5),axis.title.y = element_text(angle = 0,vjust = 0.5))  +  #set the y-axis label horizontal
  guides(fill="none") 

# Aggregate weekly sales by Date and Store
sales_date_store <- aggregate(list("Weekly_Sales" = sales$Weekly_Sales), 
                              by = list("Date" = sales$Date, "Store" = sales$Store), 
                              FUN=sum, na.rm = TRUE)
# Converting weekly sales into millions
sales_date_store$Weekly_Sales <- as.integer(sales_date_store$Weekly_Sales / 1000000)

# Merging features data set with sales_date_store
sales_stores_combined <- merge(features, sales_date_store, by = c("Date", "Store"), all = TRUE)

diff1<-anti_join(features,sales_date_store)
diff2<-anti_join(sales_date_store,features)

# Adding type column in the sales_stores_combined dataset 
sales_stores_combined <- merge(sales_stores_combined, stores[c("Store", "Type")], by = "Store", all = TRUE)

# Omitting all NA values
clean_sales_stores <- na.omit(sales_stores_combined)

# Aggregating average markdown values by Date
markdowns <- aggregate(list("Markdown1" = clean_sales_stores$MarkDown1, "Markdown2" = clean_sales_stores$MarkDown2, "Markdown3" = clean_sales_stores$MarkDown3, "Markdown4" = clean_sales_stores$MarkDown4), 
                       by = list("Date" = clean_sales_stores$Date), FUN=mean, na.rm = TRUE)

# Plotting the markdowns as trending lines

ggplot(markdowns, aes(x = Date, y = Markdowns)) + 
  geom_line(aes(y = Markdown1, colour = "Markdown1"),  linewidth = 1) + 
  geom_line(aes(y = Markdown2, colour = "Markdown2"),linewidth = 1) + 
  geom_line(aes(y = Markdown3, colour = "Markdown3"),linewidth = 1) + 
  geom_line(aes(y = Markdown4, colour = "Markdown4"),linewidth = 1) +
  theme(axis.title.y = element_text(angle = 0,vjust = 0.5)) 

corPlot(markdowns[,2:5], upper = FALSE, scale = FALSE, main = "Correlation in markdowns")
corPlot(sales_stores_combined[,5:9], upper = FALSE, scale = FALSE, main = "Correlation in markdowns")
