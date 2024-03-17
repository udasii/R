#' Suraj Udasi_Assignment 1
#' Purpose: EDA case a1 
#' Suraj Udasi
#' Jan 23, 2024

# WD
setwd("~/Visualizing_Analyzing_Data_with_R/personalFiles")

# Libraries
library(data.table)
library(DataExplorer)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(plotly)
library(readr)
library(maps)
library(ggmap)
library(lubridate)
library(scales)
library(tidyr)
library(viridis)

# read csv
Data <- read.csv("~/Visualizing_Analyzing_Data_with_R/personalFiles/2024-01-23_complete_data_a1_EDA_case.csv")

# clean columns names
names(Data) <- make.names(names(Data))

# Correct column names
names(Data) <- gsub("\\.", "_", names(Data))

# check missing values
colSums(is.na(Data))

caseData <- na.omit(subset(Data, !is.na(Zip_Code)))
dim(caseData)

# Classes for each column
sapply(caseData, class)

# Correcting Date Format
caseData$Date <- as.Date(caseData$Date)

# Descriptive Analysis

#Summary Statistics:
summary(caseData)
str(caseData)

# Identifying categorical variables
caseData$Store_Number <- as.factor(caseData$Store_Number)
caseData$Zip_Code <- as.factor(caseData$Zip_Code)
caseData$Month <- as.factor(caseData$Month)

str(caseData)

# To get unique vendors
summary <- caseData %>%
  summarize(Unique_Products = n_distinct(Item_Description),
            Unique_Vendors = n_distinct(Vendor_Name),
            Unique_Cities = n_distinct(City),
            Total_Sales = sum(Sale__Dollars_),
            Total_Bottles = sum(Bottles_Sold),
            Total_Liters = sum(Bottles_Sold * Bottle_Volume__ml_ / 1000))

# Print summary
print(summary)




# DataExplorer
plot_str(caseData)
plot_missing(caseData)
#plot_histogram(caseData) 


################# ANALYSING DATA #######################

# Aggregating sales data by year and month
caseData <- caseData %>%
  mutate(Year = year(Date),
         DayOfWeek = weekdays(Date),
         YearMonth = floor_date(Date, "month"),
         Bottle_Volume_Category = cut(Bottle_Volume__ml_ / 1000,  # Convert ml to L
                                      breaks = c(0, 0.1, 0.2, 0.5, 1, 1.75, Inf),
                                      labels = c("0 - 0.1 L", "0.1 - 0.2 L", "0.2 - 0.5 L", 
                                                 "0.5 - 1 L", "1 - 1.75 L", "1.75 L +"),
                                      include.lowest = TRUE),
         Price_Segment = cut(State_Bottle_Retail,
                             breaks = c(-Inf, 10, 20, 50, 100, Inf),
                             labels = c("Under $10", "$10-20", "$20-50", "$50-100", "Over $100")),
         Vendor_Type = ifelse(Vendor_Name == "DIAGEO AMERICAS", "Diageo Americas", "Other Vendors"))

sales_trends <- caseData %>%
  group_by(Year, Month) %>%
  summarize(Total_Sales = sum(Sale__Dollars_),
            Total_Bottles = sum(Bottles_Sold))

# Calculate year-over-year growth rate
yearly_sales <- sales_trends %>%
  group_by(Year) %>% 
  summarize(Total_Sales = sum(Total_Sales)) %>% 
  arrange(Year) %>% 
  mutate(YoY_Growth_Rate = (Total_Sales / lag(Total_Sales) - 1) * 100)

monthly_sales <- sales_trends %>%
  group_by(Month) %>% 
  summarize(Total_Sales = sum(Total_Sales)) %>% 
  arrange(Month)

highest_month <- monthly_sales[which.max(monthly_sales$Total_Sales), ]
lowest_month <- monthly_sales[which.min(monthly_sales$Total_Sales), ]
monthly_avgsales <- mean(monthly_sales$Total_Sales)
monthly_medsales <- median(monthly_sales$Total_Sales)


daily_sales <- caseData %>%
  group_by(DayOfWeek) %>%
  summarize(Total_Sales = sum(Sale__Dollars_)) %>% 
  arrange(Total_Sales)

highest_day <- daily_sales[which.max(daily_sales$Total_Sales), ]
lowest_day <- daily_sales[which.min(daily_sales$Total_Sales), ]
weekly_avgsales <- mean(daily_sales$Total_Sales)
weekly_medsales <- median(daily_sales$Total_Sales)



# Create a bar chart to visualize yearly sales trends for the specified years
sales_trends$Year <- as.numeric(as.character(sales_trends$Year))  # Convert Year to numeric

yearly_sales_plot <- ggplot(yearly_sales, aes(x = Year, y = Total_Sales, fill = Year)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = ifelse(Year != 2019, paste0(round(YoY_Growth_Rate, 1), "%"), ""),
                y = Total_Sales + 0.02 * max(Total_Sales)), # Adjust the y position for 'YoY %'
            position = position_stack(vjust = 1.02), color = "black", size = 3) +
  scale_y_continuous(labels = dollar_format(scale = 1e-6, suffix = "M")) +
  labs(title = "Yearly Sales Trends and Y-o-Y Growth (2019 to 2023 (excl. Dec 23))",
       x = "Year",
       y = "Total Sales ($ million)") +
  theme_minimal() +
  scale_fill_gradient(low = "orange", high = "darkred") +
  theme(legend.position = "none")

# Display the plot
yearly_sales_plot

sales_ts <- caseData %>%
  group_by(YearMonth, Vendor_Name) %>%
  summarise(Total_Sales = sum(Sale__Dollars_)) %>% 
  group_by(YearMonth) %>%
  arrange(desc(Total_Sales)) %>%
  mutate(rank = row_number()) %>% 
  filter(rank <= 10)



# Create the Time Series Line Chart with markers and notes
ggplot(sales_ts, aes(x = YearMonth, y = Total_Sales)) +
  geom_line() +
  geom_point(data = sales_ts %>% filter(Total_Sales == max(Total_Sales)), color = "red", size = 3) +  # Mark peak
  geom_text(data = sales_ts %>% filter(Total_Sales == max(Total_Sales)),
            aes(label = paste0("Peak: ", scales::dollar(max(Total_Sales) / 1e6, 1), "M")),
            vjust = -0.5, hjust = -0.1, color = "red", size = 4) +
  geom_point(data = sales_ts %>% filter(Total_Sales == min(Total_Sales)), color = "blue", size = 3) +  # Mark valley
  geom_text(data = sales_ts %>% filter(Total_Sales == min(Total_Sales)),
            aes(label = paste0("Valley: ", scales::dollar(min(Total_Sales) / 1e6, 1), "M")),
            vjust = 1, hjust = -0.1, color = "blue", size = 4) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
  labs(title = "Time Series of Sales",
       x = "Time",
       y = "Total Sales") +
  theme_minimal()




# Create a new variable 'Season' based on the date
caseData <- caseData %>%
  mutate(Season = case_when(
    month(Date) %in% c(3, 4, 5) ~ "Spring (Mar - May)",
    month(Date) %in% c(6, 7, 8) ~ "Summer (Jun - Aug)",
    month(Date) %in% c(9, 10, 11) ~ "Fall (Oct - Nov)",
    TRUE ~ "Winter (Dec - Feb)"
  ))

# Aggregate sales data by season
seasonal_sales <- caseData %>%
  group_by(Season) %>%
  summarize(Total_Sales = sum(Sale__Dollars_))


# Create a plot to visualize sales trends across seasons
ggplot(seasonal_sales, aes(x = Season, y = Total_Sales, fill = Season)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
  labs(title = "Sales Trends Across Seasons",
       x = "Season",
       y = "Total Sales ($ million)") +
  theme_minimal() +
  scale_fill_brewer(palette = "YlGnBu") +
  theme(legend.position = "none",
        axis.text.y = element_blank()) +
  geom_text(aes(label = scales::dollar(Total_Sales, scale = 1e-6, suffix = "M")), 
            vjust = -0.5, size = 4)

# Create a plot to visualize sales trends across months
monthly_sales$Month <- as.numeric(as.character(monthly_sales$Month))

ggplot(monthly_sales, aes(x = as.factor(Month), y = Total_Sales, fill = Month)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
  scale_fill_gradient(low = "lightblue", high = "steelblue") +
  labs(title = "Monthly Sales Over Years",
       x = "Month",
       y = "Total Sales ($ million)",
       fill = "Month") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_blank()) + 
  geom_text(aes(label = paste0("$", round(Total_Sales / 1e6, 1), "M")), 
            vjust = -0.5, size = 2.5) +
  geom_hline(yintercept = monthly_avgsales, linetype = "dashed", color = "red") +
  geom_text(aes(x = as.factor(2), y = monthly_avgsales, label = paste0("Avg: $", round(monthly_avgsales / 1e6, 1), "M")), 
            vjust = -1.2, size = 3, color = "black") +
  geom_text(aes(x = as.factor(highest_month$Month), y = highest_month$Total_Sales, 
                label = paste0("High")), 
            vjust = -1.8, size = 3, color = "blue") +
  geom_text(aes(x = as.factor(lowest_month$Month), y = lowest_month$Total_Sales, 
                label = paste0("Low")), 
            vjust = -1.8, size = 3, color = "red")


# Create a bar chart for sales trends across days of the week
ggplot(daily_sales, aes(x = reorder(as.factor(DayOfWeek), -Total_Sales), y = Total_Sales, fill = as.factor(DayOfWeek))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
  labs(title = "Weekly Sales Over Days of the Week",
       x = "Day of the Week",
       y = "Total Sales ($ million)",
       fill = "Day of the Week") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_blank()) + 
  geom_text(aes(label = paste0("$", round(Total_Sales / 1e6, 1), "M")), 
            vjust = -0.5, size = 2.5) +
  geom_hline(yintercept = weekly_avgsales, linetype = "dashed", color = "red") +
  geom_text(aes(x = as.factor(0), y = weekly_avgsales, 
                label = paste0("Avg: $", round(weekly_avgsales / 1e6, 1), "M")), 
            vjust = -1.2, size = 3, color = "black") +
  geom_text(aes(x = as.factor(highest_day$DayOfWeek), y = highest_day$Total_Sales, 
                label = paste0("High")), 
            vjust = -1.8, size = 3, color = "blue") +
  geom_text(aes(x = as.factor(lowest_day$DayOfWeek), y = lowest_day$Total_Sales, 
                label = paste0("Low")), 
            vjust = -1.8, size = 3, color = "red")


# Market Analysis
# Calculate Total Sales for Diageo for each year
diageo_sales <- caseData %>%
  filter(Vendor_Name == "DIAGEO AMERICAS") %>%
  group_by(Year) %>%
  summarize(Diageo_Sales = sum(Sale__Dollars_)) %>%
  arrange(Year) %>% 
  mutate(YoY_Growth_Diageo = ((Diageo_Sales / lag(Diageo_Sales)) - 1) * 100)

# Merge the two dataframes
diageo_v_industry <- left_join(yearly_sales, diageo_sales, by = "Year") %>% 
  mutate(Industry_Sales = Total_Sales - Diageo_Sales,
         YoY_Industry = ((Industry_Sales / lag(Industry_Sales) - 1) * 100),
         Diageo_Share = (Diageo_Sales / Total_Sales) * 100)


# Reshape for stacked bar chart
DvI_stacked_bar <- diageo_v_industry %>%
  select(Year, Diageo_Sales, Industry_Sales) %>%
  pivot_longer(cols = -Year, names_to = "Category", values_to = "Sales")  

ggplot(DvI_stacked_bar, aes(x = Year, y = Sales, fill = Category)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, prefix = "$", suffix = "M")) +
  scale_fill_manual(values = c("Diageo_Sales" = "lightblue", "Industry_Sales" = "steelblue")) +
  labs(title = "Diageo Consistently Maintaining ~20% of Total Market Share",
       x = "Year",
       y = "Total Sales (Millions $)",
       fill = "Category") +
  theme_minimal() +
  theme(legend.position = "top")



# Vendor Performance:
# Summarize sales by vendor and create a bar chart for top vendors.
top_vendors <- caseData %>%
  group_by(Vendor_Name) %>%
  summarize(Total_Sales = sum(Sale__Dollars_)) %>%
  arrange(desc(Total_Sales)) %>%
  top_n(10)

ggplot(top_vendors, aes(x = reorder(Vendor_Name, Total_Sales), y = Total_Sales, fill = Total_Sales)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen") +  # Blue gradient
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
  labs(title = "Top 10 Vendors by Sales",
       x = "Vendor Name",
       y = "Total Sales ($)") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip()

# Identifying unique vendors for each year
unique_vendors <- caseData %>%
  group_by(Year) %>%
  summarise(Unique_Vendors = n_distinct(Vendor_Name),
            Total_Sales = sum(Sale__Dollars_)) %>% 
  arrange(Year)


# Calculate Total Sales for the top 10 vendors for each year
top_10_sales <- caseData %>%
  group_by(Year, Vendor_Name) %>%
  summarise(Total_Sales = sum(Sale__Dollars_)) %>%
  group_by(Year) %>%
  arrange(Year, desc(Total_Sales)) %>%
  slice_head(n = 10) %>%
  summarise(Total_Sales_Top_10 = sum(Total_Sales))

# Merge the two dataframes
unique_vendors <- left_join(unique_vendors, top_10_sales, by = "Year") %>% 
  mutate(Total_Sales_Others = Total_Sales - Total_Sales_Top_10,
         Top_10_Share = Total_Sales_Top_10/Total_Sales * 100)

# Reshape for stacked bar chart
data_for_stacked_bar <- unique_vendors %>%
  select(Year, Total_Sales_Top_10, Total_Sales_Others) %>%
  pivot_longer(cols = -Year, names_to = "Category", values_to = "Sales")  

ggplot(data_for_stacked_bar, aes(x = Year, y = Sales, fill = Category)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, prefix = "$", suffix = "M")) +
  scale_fill_manual(values = c("Total_Sales_Top_10" = "lightblue", "Total_Sales_Others" = "steelblue")) +
  labs(title = "Top 10 Vendors Account for Over 70% of Total Market Share",
       x = "Year",
       y = "Total Sales (Millions $)",
       fill = "Category") +
  theme_minimal() +
  theme(legend.position = "top")


# Group Sales by Vendor
vendor_performance <- caseData %>%
  group_by(Vendor_Name) %>%
  summarise(
    Total_Sales = sum(Sale__Dollars_),
    Total_Liters = sum(Bottles_Sold * Bottle_Volume__ml_ / 1000),
    .groups = 'drop'
  )
# Sorting to see top vendors based on sales or liters
top_vendors_by_sales <- vendor_performance %>%
  arrange(desc(Total_Sales))
top_vendors_by_liters <- vendor_performance %>%
  arrange(desc(Total_Liters))

# Summarize sales by vendor and year
vendor_sales_over_time <- caseData %>%
  group_by(Vendor_Name, Year) %>%
  summarise(
    Total_Sales = sum(Sale__Dollars_),
    .groups = 'drop'
  )

# Plotting sales trends for top vendors
top_vendors <- head(top_vendors_by_sales, 5)$Vendor_Name  # Top 5 vendors
vendor_sales_over_time_filtered <- filter(vendor_sales_over_time, Vendor_Name %in% top_vendors)

ggplot(vendor_sales_over_time_filtered, aes(x = Year, y = Total_Sales, color = Vendor_Name)) +
  geom_line() +
  labs(
    title = "Top 5 Vendors - Sales Trends",
    x = "Year",
    y = "Total Sales (Million $)",
    color = 'Vendor'
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
  theme(legend.position = "top", legend.title = element_blank())

# Summarize liters by vendor and year
vendor_liters_over_time <- caseData %>%
  group_by(Vendor_Name, Year) %>%
  summarise(Total_Liters = sum(Bottles_Sold * Bottle_Volume__ml_ / 1000), .groups = 'drop')

vendor_liters_over_time_filtered <- filter(vendor_liters_over_time, Vendor_Name %in% top_vendors)

# Plot Vendors by Liters Over Time
ggplot(vendor_liters_over_time_filtered, aes(x = Year, y = Total_Liters, color = Vendor_Name)) +
  geom_line() +
  labs(
    title = "Top 5 Vendors - Liters Sold",
    x = "Year",
    y = "Total Liters (Million)",
    color = 'Vendor'
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::unit_format(scale = 1e-6, suffix = "M")) +
  theme(legend.position = "top", legend.title = element_blank())



# Group Sales and Liters by Category 
Customer_category <- caseData %>%
  group_by(Category_Name) %>%
  summarise( 
    Total_Sales = sum(Sale__Dollars_),
    Total_Liters = sum(Bottles_Sold * Bottle_Volume__ml_ / 1000),
    Stores = n_distinct(Store_Number),
    Bottles_Sold = sum(Bottles_Sold),
    AvgPrice_per_bottle = Total_Sales / Bottles_Sold,
    AvgSales_per_store = Total_Sales / Stores, .groups = 'drop') 

# Sorting top categories based on sales and liters
top_category_by_sales <- Customer_category %>%
  arrange(desc(Total_Sales))
top_category_by_liters <- Customer_category %>%
  arrange(desc(Total_Liters))

# Summarize sales by category and year
category_sales_over_time <- caseData %>%
  group_by(Category_Name, Year) %>%
  summarise(
    Total_Sales = sum(Sale__Dollars_),
    .groups = 'drop'
  )

# Summarize liters by category and year
category_liters_over_time <- caseData %>%
  group_by(Category_Name, Year) %>%
  summarise(Total_Liters = sum(Bottles_Sold * Bottle_Volume__ml_ / 1000), .groups = 'drop')

# Top 10 by sales
top_10_categories_sales <- Customer_category %>%
  arrange(desc(Total_Sales)) %>%
  slice_head(n = 10)

ggplot(top_10_categories_sales, aes(x = reorder(Category_Name, Total_Sales), y = Total_Sales, fill = Total_Sales)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "orange", high = "darkred") +  # Blue gradient
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
  labs(title = "Top 10 Best Selling Categories by Revenue",
       x = "Item Name",
       y = "Total Sales ($)") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip()

# Top 10 by liters
top_10_categories_liters <- Customer_category %>%
  arrange(desc(Total_Liters)) %>%
  slice_head(n = 10)

ggplot(top_10_categories_liters, aes(x = reorder(Category_Name, Total_Sales), y = Total_Sales, fill = Total_Sales)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightblue", high = "steelblue") +  # Blue gradient
  scale_y_continuous(labels = scales::unit_format(scale = 1e-6, suffix = "M")) +
  labs(title = "Top 10 Best Selling Categories by Liters",
       x = "Item Name",
       y = "Total Liters") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip()



# Plotting sales trends for top 5 categories
top_categories <- head(top_category_by_sales, 5)$Category_Name  # Top 5 categories

category_sales_over_time_filtered <- filter(category_sales_over_time, 
                                            Category_Name %in% top_categories)

ggplot(category_sales_over_time_filtered, aes(x = Year, y = Total_Sales, 
                                              color = Category_Name)) +
  geom_line() +
  labs(
    title = "Top 5 Categories - Sales Trends",
    x = "Year",
    y = "Total Sales (Million $)",
    color = 'Category'
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
  theme(legend.position = "top", legend.title = element_blank())


# Plot categories by Liters Over Time
category_liters_over_time_filtered <- filter(category_liters_over_time, 
                                             Category_Name %in% top_categories)

ggplot(category_liters_over_time_filtered, aes(x = Year, y = Total_Liters, color = Category_Name)) +
  geom_line() +
  labs(
    title = "Top 5 Categories - Liters Sold",
    x = "Year",
    y = "Total Liters (Million)",
    color = 'Category'
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::unit_format(scale = 1e-6, suffix = "M")) +
  theme(legend.position = "top", legend.title = element_blank())


# Group Sales and Liters by Items 
Customer_items <- caseData %>%
  group_by(Item_Description) %>%
  summarise( 
    Total_Sales = sum(Sale__Dollars_),
    Total_Liters = sum(Bottles_Sold * Bottle_Volume__ml_ / 1000),
    Stores = n_distinct(Store_Number),
    Bottles_Sold = sum(Bottles_Sold),
    AvgPrice_per_bottle = Total_Sales / Bottles_Sold,
    AvgSales_per_store = Total_Sales / Stores, .groups = 'drop') 

# Sorting top items based on sales and liters
top_items_by_sales <- Customer_items %>%
  arrange(desc(Total_Sales))
top_items_by_liters <- Customer_items %>%
  arrange(desc(Total_Liters))

# Summarize sales by items and year
item_sales_over_time <- caseData %>%
  group_by(Item_Description, Year) %>%
  summarise(
    Total_Sales = sum(Sale__Dollars_),
    .groups = 'drop'
  )

# Summarize liters by items and year
item_liters_over_time <- caseData %>%
  group_by(Item_Description, Year) %>%
  summarise(Total_Liters = sum(Bottles_Sold * Bottle_Volume__ml_ / 1000), .groups = 'drop')

# Top 10 Items by sales
top_10_items_sales <- Customer_items %>%
  arrange(desc(Total_Sales)) %>%
  slice_head(n = 10)

ggplot(top_10_items_sales, aes(x = reorder(Item_Description, Total_Sales), y = Total_Sales, fill = Total_Sales)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "#E6E6FA", high = "#800080") +  # Blue gradient
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
  labs(title = "Top 10 Best Selling Items by Revenue",
       x = "Item Name",
       y = "Total Sales ($)") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip()

# Top 10 Items by liters
top_10_item_liters <- Customer_items %>%
  arrange(desc(Total_Liters)) %>%
  slice_head(n = 10)

ggplot(top_10_item_liters, aes(x = reorder(Item_Description, Total_Sales), y = Total_Sales, fill = Total_Sales)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "#E0FFFF", high = "#008080") +  # Blue gradient
  scale_y_continuous(labels = scales::unit_format(scale = 1e-6, suffix = "M")) +
  labs(title = "Top 10 Best Selling Items by Liters",
       x = "Item Name",
       y = "Total Liters") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip()


# Plotting sales trends for top 5 items
top_items <- head(top_items_by_sales, 5)$Item_Description  # Top 5 items

item_sales_over_time_filtered <- filter(item_sales_over_time, 
                                        Item_Description %in% top_items)

ggplot(item_sales_over_time_filtered, aes(x = Year, y = Total_Sales, 
                                          color = Item_Description)) +
  geom_line() +
  labs(
    title = "Top 5 Items - Sales Trends",
    x = "Year",
    y = "Total Sales (Million $)",
    color = 'Category'
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
  theme(legend.position = "top", legend.title = element_blank())


# Plot items by Liters Over Time
item_liters_over_time_filtered <- filter(item_liters_over_time, 
                                         Item_Description %in% top_items)

ggplot(item_liters_over_time_filtered, aes(x = Year, y = Total_Liters, color = Item_Description)) +
  geom_line() +
  labs(
    title = "Top 5 Items - Liters Sold",
    x = "Year",
    y = "Total Liters (Million)",
    color = 'Category'
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::unit_format(scale = 1e-6, suffix = "M")) +
  theme(legend.position = "top", legend.title = element_blank())

# Group by Bottle Volume Category and summarize
bottle_volume_analysis <- caseData %>%
  group_by(Bottle_Volume_Category) %>%
  summarize(Total_Sales = sum(Sale__Dollars_), 
            Total_Bottles_Sold = sum(Bottles_Sold)) %>%
  arrange(desc(Total_Sales))

# Group by Bottle Volume Category and summarize
bottle_volume_diageo <- caseData %>%
  filter(Vendor_Name == "DIAGEO AMERICAS") %>%
  group_by(Bottle_Volume_Category) %>%
  summarize(Total_Sales = sum(Sale__Dollars_), 
            Total_Bottles_Sold = sum(Bottles_Sold)) %>%
  arrange(desc(Total_Sales))

# Plot Total
ggplot(bottle_volume_analysis, aes(x = Bottle_Volume_Category, y = Total_Bottles_Sold, fill = Bottle_Volume_Category)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::unit_format(scale = 1e-6, suffix = "M")) +
  labs(title = "Sales by Bottle Volume Category",
       x = "Bottle Volume Category",
       y = "Total Bottles Sold") +
  theme_minimal() +
  scale_fill_brewer(palette = "Spectral") +  # Using a discrete color palette
  theme(legend.position = "none")

# Plot Diageo
ggplot(bottle_volume_diageo, aes(x = Bottle_Volume_Category, y = Total_Bottles_Sold, fill = Bottle_Volume_Category)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::unit_format(scale = 1e-6, suffix = "M")) +
  labs(title = "Diageo Sales by Bottle Volume Category",
       x = "Bottle Volume Category",
       y = "Total Bottles Sold") +
  theme_minimal() +
  scale_fill_brewer(palette = "Spectral") +  # Using a discrete color palette
  theme(legend.position = "none")


# Group by Price Segment and summarize
price_segment_analysis <- caseData %>%
  group_by(Price_Segment) %>%
  summarize(Total_Sales = sum(Sale__Dollars_), 
            Total_Bottles_Sold = sum(Bottles_Sold)) %>%
  arrange(desc(Total_Sales))

# Group by Price Segment and summarize for Diageo
price_segment_diageo <- caseData %>%
  filter(Vendor_Name == "DIAGEO AMERICAS") %>%
  group_by(Price_Segment) %>%
  summarize(Total_Sales = sum(Sale__Dollars_), 
            Total_Bottles_Sold = sum(Bottles_Sold)) %>%
  arrange(desc(Total_Sales))

# Plot Total
ggplot(price_segment_analysis, aes(x = Price_Segment, y = Total_Bottles_Sold, fill = Price_Segment)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::unit_format(scale = 1e-6, suffix = "M")) +
  labs(title = "Bottles Sold by Price Segment",
       x = "Price Segment",
       y = "Total Bottles Sold") +
  theme_minimal() +
  scale_fill_brewer(palette = "Paired") +  # Using a discrete color palette
  theme(legend.position = "none")

# Plot Diageo
ggplot(price_segment_diageo, aes(x = Price_Segment, y = Total_Bottles_Sold, fill = Price_Segment)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::unit_format(scale = 1e-6, suffix = "M")) +
  labs(title = "Diageo Bottles Sold by Price Segment",
       x = "Price Segment",
       y = "Total Bottles Sold") +
  theme_minimal() +
  scale_fill_brewer(palette = "Paired") +  # Using a discrete color palette
  theme(legend.position = "none")


# Price Comparison
# Adjust the data to include Diageo, Sazerac and Others
boxplot_data <- caseData %>%
  select(Vendor_Name, State_Bottle_Retail) %>%
  mutate(Vendor_Type = case_when(
    Vendor_Name == "DIAGEO AMERICAS" ~ "Diageo Americas",
    Vendor_Name == "SAZERAC COMPANY  INC" ~ "Sazerac Company Inc",
    TRUE ~ "Other Vendors")) %>%
  filter(Vendor_Type %in% c("Diageo Americas", "Sazerac Company Inc", "Other Vendors")) %>%
  mutate(State_Bottle_Retail = as.numeric(State_Bottle_Retail))

boxplot_data <- boxplot_data %>%
  group_by(Vendor_Type) %>%
  mutate(Q1 = quantile(State_Bottle_Retail, 0.25, na.rm = TRUE),
         Q3 = quantile(State_Bottle_Retail, 0.75, na.rm = TRUE),
         IQR = Q3 - Q1) %>%
  filter(State_Bottle_Retail >= (Q1 - 1.5 * IQR) & State_Bottle_Retail <= (Q3 + 1.5 * IQR))

# Box plot for price comparison without outliers
ggplot(boxplot_data, aes(x = Vendor_Type, y = State_Bottle_Retail, fill = Vendor_Type)) +
  geom_boxplot() +
  stat_summary(fun = min, geom = "text", aes(label = after_stat(sprintf("Min: %.1f", y))), vjust = -1, color = "black", size = 3) +
  stat_summary(fun = max, geom = "text", aes(label = after_stat(sprintf("Max: %.1f", y))), vjust = 2, color = "black", size = 3) +
  stat_summary(fun = median, geom = "text", aes(label = after_stat(sprintf("Median: %.1f", y))), vjust = 0, hjust = -0.2, color = "black", size = 3) +
  stat_summary(fun = mean, geom = "text", aes(label = after_stat(sprintf("Mean: %.1f", y))), vjust = 0, hjust = 1.2, color = "black", size = 3) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Comparison of Bottle Retail Prices (Without Outliers)",
       x = "Vendor Type",
       y = "State Bottle Retail Price ($)") +
  theme_minimal() +
  theme(legend.position = "top")





# Closest comp analysis, sales, bottle sales, store reach
# Top 2 Vendor Comp analysis

top2_vendors <- caseData %>%
  group_by(Vendor_Name) %>%
  summarize(Total_Sales = sum(Sale__Dollars_)) %>%
  arrange(desc(Total_Sales)) %>%
  slice_head(n = 2)

yearly_sales_top2 <- caseData %>%
  filter(Vendor_Name %in% top2_vendors$Vendor_Name) %>%
  group_by(Year, Vendor_Name) %>%
  summarize(Yearly_Sales = sum(Sale__Dollars_))

bottle_sales_top2 <- caseData %>%
  filter(Vendor_Name %in% top2_vendors$Vendor_Name) %>%
  group_by(Year, Vendor_Name) %>%
  summarize(Bottles_Sold = sum(Bottles_Sold))

stores_top2 <- caseData %>%
  filter(Vendor_Name %in% top2_vendors$Vendor_Name) %>%
  group_by(Year, Vendor_Name) %>%
  summarize(Stores = n_distinct(Store_Number))

total_stores <- caseData %>%
  summarize(Stores = n_distinct(Store_Number))
total_stores

comp_analysis <- merge(merge(yearly_sales_top2, bottle_sales_top2, by = c("Year", "Vendor_Name")), stores_top2, by = c("Year", "Vendor_Name"))


## Compare top 2 vendors by top 10 categories (how much is top 10 categories of total)


# Most preferred product - Diagio
# Filter the data for "DIAGEO AMERICAS"
diageo_items <- caseData %>%
  filter(Vendor_Name == "DIAGEO AMERICAS") %>%
  group_by(Category_Name) %>%
  summarise(
    Items = n_distinct(Item_Description), 
    Total_Liters = sum(Bottles_Sold * Bottle_Volume__ml_ / 1000),
    Total_Sales = sum(Sale__Dollars_),
    Stores = n_distinct(Store_Number),
    Bottles_Sold = sum(Bottles_Sold),
    AvgPrice_per_bottle = Total_Sales / Bottles_Sold,
    AvgSales_per_store = Total_Sales / Stores
  ) %>%
  arrange(desc(Total_Sales)) 

diageo_items_summary <- diageo_items %>% 
  summarise(
    Items = sum(Items), 
    Total_Sales = sum(Total_Sales),
    Bottles_Sold = sum(Bottles_Sold),
    AvgPrice_per_bottle = Total_Sales / sum(Bottles_Sold))

min_max_price_diageo <- diageo_items %>% 
  summarise(AvgPrice_per_bottle_min = min(AvgPrice_per_bottle),
            AvgPrice_per_bottle_max = max(AvgPrice_per_bottle))

# Diageo best seller
# Diageo best seller category
diageo_usp_cat <- caseData %>%
  filter(Vendor_Name == "DIAGEO AMERICAS") %>%
  group_by(Category_Name) %>%
  summarise( 
    Total_Sales = sum(Sale__Dollars_),
    Total_Liters = sum(Bottles_Sold * Bottle_Volume__ml_ / 1000),
    Stores = n_distinct(Store_Number),
    Bottles_Sold = sum(Bottles_Sold),
    AvgPrice_per_bottle = Total_Sales / Bottles_Sold,
    AvgSales_per_store = Total_Sales / Stores, .groups = 'drop') 

# Diageo best seller items
diageo_usp_items <- caseData %>%
  filter(Vendor_Name == "DIAGEO AMERICAS") %>%
  group_by(Item_Description) %>%
  summarise( 
    Total_Sales = sum(Sale__Dollars_),
    Total_Liters = sum(Bottles_Sold * Bottle_Volume__ml_ / 1000),
    Stores = n_distinct(Store_Number),
    Bottles_Sold = sum(Bottles_Sold),
    AvgPrice_per_bottle = Total_Sales / Bottles_Sold,
    AvgSales_per_store = Total_Sales / Stores, .groups = 'drop') 

# Sorting diageo top category based on sales and liters
top_d_cat_by_sales <- diageo_usp_cat %>%
  arrange(desc(Total_Sales))
top_d_cat_by_liters <- diageo_usp_cat %>%
  arrange(desc(Total_Liters))

# Sorting diageo top items based on sales and liters
top_d_items_by_sales <- diageo_usp_items %>%
  arrange(desc(Total_Sales))
top_d_items_by_liters <- diageo_usp_items %>%
  arrange(desc(Total_Liters))


# Summarize sales by category and year
d_cat_sales_over_time <- caseData %>%
  filter(Vendor_Name == "DIAGEO AMERICAS") %>%
  group_by(Category_Name, Year) %>%
  summarise(
    Total_Sales = sum(Sale__Dollars_),
    .groups = 'drop'
  )

# Summarize sales by items and year
d_item_sales_over_time <- caseData %>%
  filter(Vendor_Name == "DIAGEO AMERICAS") %>%
  group_by(Item_Description, Year) %>%
  summarise(
    Total_Sales = sum(Sale__Dollars_),
    .groups = 'drop'
  )


# Summarize Diageo liters by category and year
d_cat_liters_over_time <- caseData %>%
  group_by(Category_Name, Year) %>%
  summarise(Total_Liters = sum(Bottles_Sold * Bottle_Volume__ml_ / 1000), .groups = 'drop')

# Summarize Diageo liters by items and year
d_item_liters_over_time <- caseData %>%
  group_by(Item_Description, Year) %>%
  summarise(Total_Liters = sum(Bottles_Sold * Bottle_Volume__ml_ / 1000), .groups = 'drop')

# Top 10 Diageo Category by sales
d_top_10_cat_sales <- diageo_usp_cat %>%
  arrange(desc(Total_Sales)) %>%
  slice_head(n = 10)

# Top 10 Diageo Items by sales
d_top_10_items_sales <- diageo_usp_items %>%
  arrange(desc(Total_Sales)) %>%
  slice_head(n = 10)

# Top 10 Diageo Category by liters
d_top_10_cat_liters <- diageo_usp_cat %>%
  arrange(desc(Total_Liters)) %>%
  slice_head(n = 10)

# Top 10 Diageo Items by liters
d_top_10_items_liters <- diageo_usp_items %>%
  arrange(desc(Total_Liters)) %>%
  slice_head(n = 10)


# Plot Diageo top 10 Categories 
ggplot(d_top_10_cat_sales, aes(x = reorder(Category_Name, Total_Sales), y = Total_Sales, fill = Total_Sales)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "#C0C0C0", high = "#27408B") +  # Blue gradient
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
  labs(title = "Diageo's Top 10 Best Selling Categories by Revenue",
       x = "Category",
       y = "Total Sales ($)") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip()

# Plot Diageo top 10 Items 
ggplot(d_top_10_items_sales, aes(x = reorder(Item_Description, Total_Sales), y = Total_Sales, fill = Total_Sales)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "#C0C0C0", high = "#27408B") +  # Blue gradient
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
  labs(title = "Diageo's Top 10 Best Selling Items by Revenue",
       x = "Item Name",
       y = "Total Sales ($)") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip()

# Plot Diageo top 10 Category Liters
ggplot(d_top_10_cat_liters, aes(x = reorder(Category_Name, Total_Sales), y = Total_Sales, fill = Total_Sales)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "#f6e8c3", high = "#8c510a") +  # Blue gradient
  scale_y_continuous(labels = scales::unit_format(scale = 1e-6, suffix = "M")) +
  labs(title = "Diageo's Top 10 Best Selling Categories by Liters",
       x = "Category",
       y = "Total Liters") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip()


# Plot Diageo top 10 Items Liters
ggplot(d_top_10_items_liters, aes(x = reorder(Item_Description, Total_Sales), y = Total_Sales, fill = Total_Sales)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "#f6e8c3", high = "#8c510a") +  # Blue gradient
  scale_y_continuous(labels = scales::unit_format(scale = 1e-6, suffix = "M")) +
  labs(title = "Diageo's Top 10 Best Selling Items by Liters",
       x = "Item Name",
       y = "Total Liters") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip()


# Plotting Diageo sales trends for top 5 Categories
d_top_cat <- head(top_d_cat_by_sales, 5)$Category_Name  # Top 5 items

d_cat_sales_over_time_filtered <- filter(d_cat_sales_over_time, 
                                         Category_Name %in% d_top_cat)

ggplot(d_cat_sales_over_time_filtered, aes(x = Year, y = Total_Sales, 
                                           color = Category_Name)) +
  geom_line() +
  labs(
    title = "Top 5 Categories - Sales Trends",
    x = "Year",
    y = "Total Sales (Million $)",
    color = 'Category'
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
  theme(legend.position = "top", legend.title = element_blank())




# Plotting Diageo sales trends for top 5 items
d_top_items <- head(top_d_items_by_sales, 5)$Item_Description  # Top 5 items

d_item_sales_over_time_filtered <- filter(d_item_sales_over_time, 
                                          Item_Description %in% d_top_items)

ggplot(d_item_sales_over_time_filtered, aes(x = Year, y = Total_Sales, 
                                            color = Item_Description)) +
  geom_line() +
  labs(
    title = "Top 5 Items - Sales Trends",
    x = "Year",
    y = "Total Sales (Million $)",
    color = 'Items'
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
  theme(legend.position = "top", legend.title = element_blank())

# Plot Diageo category by Liters Over Time
d_cat_liters_over_time_filtered <- filter(d_cat_liters_over_time, 
                                          Category_Name %in% d_top_cat)

ggplot(d_cat_liters_over_time_filtered, aes(x = Year, y = Total_Liters, color = Category_Name)) +
  geom_line() +
  labs(
    title = "Top 5 Categories - Liters Sold",
    x = "Year",
    y = "Total Liters (Million)",
    color = 'Category'
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::unit_format(scale = 1e-6, suffix = "M")) +
  theme(legend.position = "top", legend.title = element_blank())

# Plot Diageo items by Liters Over Time
d_items_liters_over_time_filtered <- filter(d_item_liters_over_time, 
                                            Item_Description %in% d_top_items)

ggplot(d_items_liters_over_time_filtered, aes(x = Year, y = Total_Liters, color = Item_Description)) +
  geom_line() +
  labs(
    title = "Top 5 Items - Liters Sold",
    x = "Year",
    y = "Total Liters (Million)",
    color = 'Items'
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::unit_format(scale = 1e-6, suffix = "M")) +
  theme(legend.position = "top", legend.title = element_blank())



# Exploring American Vodkas Category
American_vodkas <- caseData %>%
  filter(Category_Name == "AMERICAN VODKAS") %>%
  group_by(Vendor_Name) %>%
  summarise(Total_Sales = sum(Sale__Dollars_),
            Bottles_sold = sum(Bottles_Sold)) %>%
  mutate(Avg_price = Total_Sales / Bottles_sold) %>%
  arrange(desc(Total_Sales))

vodkas_top5 <- caseData %>%
  filter(Category_Name == "AMERICAN VODKAS") %>%
  group_by(Item_Description) %>%
  summarise(Total_Sales = sum(Sale__Dollars_),
            Bottles_sold = sum(Bottles_Sold)) %>%
  mutate(Avg_price = Total_Sales / Bottles_sold) %>%
  arrange(desc(Total_Sales)) %>% 
  slice_head(n = 5)

American_vodkas_top5 <- caseData %>%
  filter(Category_Name == "AMERICAN VODKAS") %>%
  group_by(Vendor_Name) %>%
  summarise(Total_Sales = sum(Sale__Dollars_),
            Bottles_sold = sum(Bottles_Sold)) %>%
  mutate(Avg_price = Total_Sales / Bottles_sold) %>%
  arrange(desc(Total_Sales)) %>%
  slice_head(n = 5)

American_vodkas_summary <- American_vodkas %>%
  summarise(Total_Sales = sum(Total_Sales),
            Bottles_sold = sum(Bottles_sold)) %>%
  mutate(Avg_price = Total_Sales / Bottles_sold)


# Dominant Store Locations and Performance
# Aggregate sales by store and create a bar chart to show top-performing stores.

# Finding total number of stores by year
unique_store_count <- caseData %>%
  group_by(Year) %>%
  summarise(Unique_Store_Count = n_distinct(Store_Name)) %>% 
  arrange(Year) %>% 
  mutate(YoY_Growth = ifelse(Year != min(Year), ((Unique_Store_Count / lag(Unique_Store_Count)) - 1) * 100, NA))

print(unique_store_count)

yearly_store_count <- ggplot(unique_store_count, aes(x = Year, y = Unique_Store_Count, fill = Year)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = ifelse(!is.na(YoY_Growth), paste0(round(YoY_Growth, 1), "%"), ""),
                y = Unique_Store_Count + 0.02 * max(Unique_Store_Count)),
            position = position_stack(vjust = 1.02), color = "black", size = 3) +
  geom_text(aes(label = comma(Unique_Store_Count), 
                y = Unique_Store_Count + 0.02 * max(Unique_Store_Count)),
            position = position_stack(vjust = 0.8), color = "black", size = 3) + # Add store count labels
  labs(title = "Yearly Store Counts and Y-o-Y Growth (2019 to 2023 (excl. Dec 23))",
       x = "Year",
       y = "Total Stores",
       fill = "Year") +
  theme_minimal() +
  scale_fill_gradient(low = "lightgreen", high = "lightblue") +
  theme(legend.position = "none")

# Display the plot
yearly_store_count


# How many of these stores sell diageo products?

unique_store_count <- caseData %>%
  group_by(Year) %>%
  summarise(Unique_Store_Count = n_distinct(Store_Name)) %>% 
  arrange(Year) %>% 
  mutate(YoY_Growth = ifelse(Year != min(Year), ((Unique_Store_Count / lag(Unique_Store_Count)) - 1) * 100, NA))


# Calculate Total Stores with Diageo products
Diageo_stores <- caseData %>%
  filter(Vendor_Name == "DIAGEO AMERICAS") %>%
  group_by(Year) %>%
  summarise(Diageo_store_count = n_distinct(Store_Name)) %>%
  arrange(Year) %>%
  mutate(YoY_Growth = ifelse(Year != min(Year), ((Diageo_store_count / lag(Diageo_store_count)) - 1) * 100, NA))

# Merge the two dataframes
Diageo_penetration <- left_join(unique_store_count, Diageo_stores, by = "Year") %>% 
  mutate(Untapped_stores = Unique_Store_Count - Diageo_store_count,
         Diageo_pen = Diageo_store_count/Unique_Store_Count * 100)

# Reshape for stacked bar chart
stacked_geo_data <- Diageo_penetration %>%
  select(Year, Untapped_stores, Diageo_store_count) %>%
  pivot_longer(cols = -Year, names_to = "Category", values_to = "Stores") 

ggplot(stacked_geo_data, aes(x = Year, y = Stores, fill = Category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Diageo_store_count" = "lightblue", "Untapped_stores" = "lightgreen")) +
  labs(title = "Diageo Americas Maintains ~99% Store Penetration",
       x = "Year",
       y = "Number of Stores (Thousands)",
       fill = "Category") +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "K"))

# Get a list of all unique store names
all_stores <- unique(caseData$Store_Name)

# Get a list of stores where Diageo Americas is present
diageo_stores <- caseData %>%
  filter(Vendor_Name == "DIAGEO AMERICAS") %>%
  distinct(Store_Name)

# Find stores where Diageo Americas is not present
stores_without_diageo <- setdiff(all_stores, diageo_stores$Store_Name)

# Filter the caseData dataframe for stores without Diageo Americas
stores_data_without_diageo <- caseData %>%
  filter(Store_Name %in% stores_without_diageo) %>% 
  group_by(City) %>% 
  summarise(No_of_stores = n_distinct(Store_Name),
            Total_sales = sum(Sale__Dollars_),
            Bottles_sold = sum(Bottles_Sold)) %>% 
  arrange(desc(Total_sales))


stores_data_without_diageo_summary <- stores_data_without_diageo %>%
  summarise(Total_sales = sum(Total_sales),
            Bottles_sold = sum(Bottles_sold))


# Finding number of stores and sales by city

city_store_count <- caseData %>%
  group_by(County) %>%
  summarise(Unique_Store_Count = n_distinct(Store_Name),
            Total_Sales = sum(Sale__Dollars_),
            Total_Bottles = sum(Bottles_Sold)) %>% 
  mutate(Avgsale = Total_Sales/Unique_Store_Count,
         Avgbottle = Total_Bottles/Unique_Store_Count) %>% 
  arrange(desc(Avgsale))

top_stores <- caseData %>%
  group_by(Store_Name) %>%
  summarize(Total_Sales = sum(Sale__Dollars_)) %>%
  arrange(desc(Total_Sales)) %>%
  top_n(10)

ggplot(top_stores, aes(x = reorder(Store_Name, Total_Sales), y = Total_Sales, fill = Store_Name)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
  labs(title = "Top 10 Stores in Iowa by Total Sales",
       x = "Store Name",
       y = "Total Sales ($)") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip()

# Geographical Influence on Sales:
# Group sales by city and visualize using a bar chart.

# Summarize and arrange the data
city_sales <- caseData %>%
  group_by(City) %>%
  summarize(Total_Sales = sum(Sale__Dollars_)) %>%
  arrange(desc(Total_Sales)) %>%
  slice_max(order_by = Total_Sales, n = 10)  # Get top 10 cities

# Create the plot
ggplot(city_sales, aes(x = reorder(City, Total_Sales), y = Total_Sales, fill = Total_Sales)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +  # Blue gradient
  scale_y_continuous(labels = scales::unit_format(scale = 1e-6, suffix = "M")) +
  labs(title = "Top 10 Cities by Sales",
       x = "City",
       y = "Total Sales") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip()


#

# Calculate the total sales for Diageo Americas in the top 10 cities in Iowa
diageo_sales_in_top_cities <- caseData %>%
  filter(City %in% city_sales$City, Vendor_Name == "DIAGEO AMERICAS") %>%
  group_by(City) %>%
  summarize(Diageo_Sales = sum(Sale__Dollars_)) %>%
  ungroup()

# Merge the total Diageo sales with the top cities data
city_sales <- city_sales %>%
  left_join(diageo_sales_in_top_cities, by = "City") %>%
  mutate(Diageo_Share = Diageo_Sales / Total_Sales * 100)

# Create the ggplot chart with Diageo share
ggplot(city_sales, aes(x = reorder(City, Total_Sales), y = Total_Sales, fill = City)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Diageo_Share, 1), "%")), vjust = -0.5, size = 3) +  # Add Diageo share labels
  scale_y_continuous(labels = scales::unit_format(scale = 1e-6, suffix = "M")) +
  labs(title = "Top 10 Cities in Iowa by Sales (Diageo Americas Share)",
       x = "City",
       y = "Total Sales ($)") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip()

# Filter for Diageo Americas and calculate total sales per store
diageo_sales <- caseData %>%
  filter(Vendor_Name == "DIAGEO AMERICAS") %>%
  group_by(Store_Name, City, County) %>%
  summarize(Total_Sales = sum(Sale__Dollars_)) %>%
  arrange(desc(Total_Sales))

# Get the top 10 stores for Diageo Americas
top_10_diageo_stores <- head(diageo_sales, 10)

# Create the plot
ggplot(top_10_diageo_stores, aes(x = reorder(Store_Name, Total_Sales), y = Total_Sales, fill = Store_Name)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
  labs(title = "Diageo's Top 10 Stores by Total Sales",
       x = "Store Name",
       y = "Total Sales ($)") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip()

# Calculate the total sales for Diageo Americas in the top 10 stores in Iowa
diageo_sales_in_top_stores <- caseData %>%
  filter(Store_Name %in% top_stores$Store_Name, Vendor_Name == "DIAGEO AMERICAS") %>%
  group_by(Store_Name) %>%
  summarize(Diageo_Sales = sum(Sale__Dollars_)) %>%
  ungroup()

# Merge the total Diageo sales with the top stores data
top_stores <- top_stores %>%
  left_join(diageo_sales_in_top_stores, by = "Store_Name") %>%
  mutate(Diageo_Share = Diageo_Sales / Total_Sales * 100)

# Create the ggplot chart with Diageo share
ggplot(top_stores, aes(x = reorder(Store_Name, Total_Sales), y = Total_Sales, fill = Store_Name)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Diageo_Share, 1), "%")), vjust = -0.5, size = 3) +  # Add Diageo share labels
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
  labs(title = "Top 10 Stores in Iowa by Total Sales (Diageo Americas Share)",
       x = "Store Name",
       y = "Total Sales ($)") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip()







