Real world data science tasks
================
Jason Cox
9/12/2021

### Introduction

This is an R exercise to work through the real world data science
Python/Pandas analysis on Keith Galli’s YouTube video: [Real Life Data
Science Tasks in
Python/Pandas](https://www.youtube.com/watch?v=eMOA1pPVUc4&t=2803s "Test")

``` r
# Setting up my environment
# Importing libraries and loading 12 months of data

library(tidyverse)
library(janitor)
library(kableExtra)  
library(RColorBrewer)  
library(ggplot2)
library(scales)
library(lubridate)

df_m1 <- read.csv('Sales_January_2019.csv')
df_m2 <- read.csv('Sales_February_2019.csv')
df_m3 <- read.csv('Sales_March_2019.csv')
df_m4 <- read.csv('Sales_April_2019.csv')
df_m5 <- read.csv('Sales_May_2019.csv')
df_m6 <- read.csv('Sales_June_2019.csv')
df_m7 <- read.csv('Sales_July_2019.csv')
df_m8 <- read.csv('Sales_August_2019.csv')
df_m9 <- read.csv('Sales_September_2019.csv')
df_m10 <- read.csv('Sales_October_2019.csv')
df_m11 <- read.csv('Sales_November_2019.csv')
df_m12 <- read.csv('Sales_December_2019.csv')

# Merging data into one data frame 

sales_df <- rbind(df_m1,df_m2,df_m3,df_m4,df_m5,df_m6,df_m7,df_m8,df_m9,
                  df_m10,df_m11,df_m12)

head(sales_df)
```

    ##   Order.ID                  Product Quantity.Ordered Price.Each
    ## 1   141234                   iPhone                1        700
    ## 2   141235 Lightning Charging Cable                1      14.95
    ## 3   141236         Wired Headphones                2      11.99
    ## 4   141237         27in FHD Monitor                1     149.99
    ## 5   141238         Wired Headphones                1      11.99
    ## 6   141239   AAA Batteries (4-pack)                1       2.99
    ##       Order.Date                       Purchase.Address
    ## 1 01/22/19 21:25        944 Walnut St, Boston, MA 02215
    ## 2 01/28/19 14:15       185 Maple St, Portland, OR 97035
    ## 3 01/17/19 13:33  538 Adams St, San Francisco, CA 94016
    ## 4 01/05/19 20:33     738 10th St, Los Angeles, CA 90001
    ## 5 01/25/19 11:59          387 10th St, Austin, TX 73301
    ## 6 01/29/19 20:22 775 Willow St, San Francisco, CA 94016

``` r
dim(df_m1)
```

    ## [1] 9723    6

``` r
dim(sales_df)
```

    ## [1] 186850      6

### Q1.

What was the best month of sales for the data and how much was earned in
that month?<br>

``` r
# Add date and month column 

sales_df$Sales.Date <- as.Date(sales_df$Order.Date,"%m/%d/%y")
class(sales_df$Sales.Date)
```

    ## [1] "Date"

``` r
sales_df$Sales.Month <- format(sales_df$Sales.Date,"%m")

# Change data types for Price.Each and Quantity.Ordered so we can add Sales.Total column 

sales_df$Price.Each <- as.numeric(sales_df$Price.Each)
sales_df$Quantity.Ordered <- as.numeric(sales_df$Quantity.Ordered)

sales_df <- sales_df %>% 
  mutate(Sales.Total = Price.Each * Quantity.Ordered)

# For use in Excel or SQL 
# write.csv(sales_df,"sales_df.csv", row.names = FALSE) 


# Find total missing values in data frame 

sum(is.na(sales_df))
```

    ## [1] 1800

``` r
# Find percentage of missing values by column 

ncol <- ncol(sales_df)
for (i in 1:ncol) {
  percentage = mean(is.na(sales_df[i])) * 100
  
  print(paste0('percentage of missing values for ',colnames(sales_df[i])))
  print(percentage)
  cat("\n")
}
```

    ## [1] "percentage of missing values for Order.ID"
    ## [1] 0
    ## 
    ## [1] "percentage of missing values for Product"
    ## [1] 0
    ## 
    ## [1] "percentage of missing values for Quantity.Ordered"
    ## [1] 0
    ## 
    ## [1] "percentage of missing values for Price.Each"
    ## [1] 0
    ## 
    ## [1] "percentage of missing values for Order.Date"
    ## [1] 0
    ## 
    ## [1] "percentage of missing values for Purchase.Address"
    ## [1] 0
    ## 
    ## [1] "percentage of missing values for Sales.Date"
    ## [1] 0.4816698
    ## 
    ## [1] "percentage of missing values for Sales.Month"
    ## [1] 0.4816698
    ## 
    ## [1] "percentage of missing values for Sales.Total"
    ## [1] 0

``` r
# Drop rows with missing values

dim(sales_df)
```

    ## [1] 186850      9

``` r
sales_df_v2 <- sales_df %>% 
  na.omit(sales_df)

dim(sales_df_v2)
```

    ## [1] 185950      9

``` r
# Removing redundant Order.Date2 column 

#sales_df_v2 <- sales_df_v2 %>% 
#  select(-Order.Date2)

str(sales_df_v2)
```

    ## 'data.frame':    185950 obs. of  9 variables:
    ##  $ Order.ID        : Factor w/ 178439 levels "","141234","141235",..: 2 3 4 5 6 7 8 9 10 11 ...
    ##  $ Product         : Factor w/ 21 levels "","20in Monitor",..: 12 15 21 4 21 7 3 19 9 8 ...
    ##  $ Quantity.Ordered: num  2 2 3 2 2 2 2 2 2 2 ...
    ##  $ Price.Each      : num  17 5 4 6 4 9 13 3 18 7 ...
    ##  $ Order.Date      : Factor w/ 142397 levels "","01/01/19 03:07",..: 5702 7157 4250 1244 6312 7521 6585 1110 48 5700 ...
    ##  $ Purchase.Address: Factor w/ 140789 levels "","1 4th St, Los Angeles, CA 90001",..: 8656 899 4537 6539 2983 6929 8973 854 7862 5757 ...
    ##  $ Sales.Date      : Date, format: "2019-01-22" "2019-01-28" ...
    ##  $ Sales.Month     : chr  "01" "01" "01" "01" ...
    ##  $ Sales.Total     : num  34 10 12 12 8 18 26 6 36 14 ...
    ##  - attr(*, "na.action")= 'omit' Named int  665 679 798 877 1074 1103 1195 1300 1492 1700 ...
    ##   ..- attr(*, "names")= chr  "665" "679" "798" "877" ...

``` r
summary(sales_df_v2)
```

    ##     Order.ID                          Product      Quantity.Ordered
    ##  160873 :     5   USB-C Charging Cable    :21903   Min.   : 2.000  
    ##  165665 :     4   Lightning Charging Cable:21658   1st Qu.: 2.000  
    ##  178158 :     4   AAA Batteries (4-pack)  :20641   Median : 2.000  
    ##  193511 :     4   AA Batteries (4-pack)   :20577   Mean   : 2.124  
    ##  194253 :     4   Wired Headphones        :18882   3rd Qu.: 2.000  
    ##  196615 :     4   Apple Airpods Headphones:15549   Max.   :11.000  
    ##  (Other):185925   (Other)                 :66740                   
    ##    Price.Each              Order.Date    
    ##  Min.   : 2.000   12/15/19 20:16:     8  
    ##  1st Qu.: 5.000   04/02/19 13:24:     7  
    ##  Median : 8.000   10/30/19 21:28:     7  
    ##  Mean   : 8.704   12/11/19 13:24:     7  
    ##  3rd Qu.:11.000   01/17/19 21:54:     6  
    ##  Max.   :25.000   02/06/19 11:14:     6  
    ##                   (Other)       :185909  
    ##                                Purchase.Address    Sales.Date        
    ##  193 Forest St, San Francisco, CA 94016:     9   Min.   :2019-01-01  
    ##  279 Sunset St, San Francisco, CA 94016:     8   1st Qu.:2019-04-16  
    ##  223 Elm St, Los Angeles, CA 90001     :     8   Median :2019-07-17  
    ##  550 Cherry St, San Francisco, CA 94016:     7   Mean   :2019-07-18  
    ##  284 Walnut St, San Francisco, CA 94016:     7   3rd Qu.:2019-10-26  
    ##  716 5th St, San Francisco, CA 94016   :     7   Max.   :2020-01-01  
    ##  (Other)                               :185904                       
    ##  Sales.Month         Sales.Total   
    ##  Length:185950      Min.   : 4.00  
    ##  Class :character   1st Qu.:10.00  
    ##  Mode  :character   Median :16.00  
    ##                     Mean   :18.43  
    ##                     3rd Qu.:26.00  
    ##                     Max.   :99.00  
    ## 

``` r
# Calculate sales total by month 

sales_total_sum <- aggregate(Sales.Total~Sales.Month, data = sales_df_v2, sum)
sales_total_sum
```

    ##    Sales.Month Sales.Total
    ## 1           01      178480
    ## 2           02      219736
    ## 3           03      280634
    ## 4           04      336770
    ## 5           05      305972
    ## 6           06      251960
    ## 7           07      262530
    ## 8           08      223732
    ## 9           09      213034
    ## 10          10      371743
    ## 11          11      322160
    ## 12          12      459631

``` r
# Which month has max Sales.Total?

sales_total_sum[which.max(sales_total_sum$Sales.Total),]
```

    ##    Sales.Month Sales.Total
    ## 12          12      459631

``` r
# data visualization for sales total by month
sales_df_v2 %>% 
  group_by(Sales.Month) %>% 
  summarize(monthly_total = sum(Sales.Total)) %>% 
  #arrange(member_casual, week  day) %>% 
  ggplot(aes(x = Sales.Month, y = monthly_total)) +
  geom_col(position = "dodge", fill = '#004c6d') +
  theme(plot.title = element_text(size=18, margin=margin(15,0,15,0))) + 
  scale_y_continuous(labels = comma_format()) +
  labs(title = "Monthly sales in USD ($)") + 
  ylab("Sales total") +
  xlab("Month")
```

![](sales_data_analysis_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

### A1.

The best month for sales was December, with $459,631 in total sales.
<br>

### Q2.

What U.S. City had the highest number of sales?<br>

``` r
# sales_df_v2[str_split(sales_df_v2$Purchase.Address,", ")[[1]][2]

sales_df_v2$City <- sapply(str_split(sales_df_v2$Purchase.Address,", "),"[", 2)
sales_df_v2$StateZip <- sapply(str_split(sales_df_v2$Purchase.Address,", "),"[", 3)
sales_df_v2$State <- sapply(strsplit(sales_df_v2$StateZip,' '),"[", 1)
sales_df_v2$City.State <- str_c(sales_df_v2$City,', ',sales_df_v2$State)
  
# Which cities are in the dataset? 

unique(sales_df_v2$City)
```

    ## [1] "Boston"        "Portland"      "San Francisco" "Los Angeles"  
    ## [5] "Austin"        "Atlanta"       "Seattle"       "New York City"
    ## [9] "Dallas"

``` r
unique(sales_df_v2$StateZip)
```

    ##  [1] "MA 02215" "OR 97035" "CA 94016" "CA 90001" "TX 73301" "GA 30301"
    ##  [7] "WA 98101" "NY 10001" "TX 75001" "ME 04101"

``` r
unique(sales_df_v2$State)
```

    ## [1] "MA" "OR" "CA" "TX" "GA" "WA" "NY" "ME"

``` r
unique(sales_df_v2$City.State)
```

    ##  [1] "Boston, MA"        "Portland, OR"      "San Francisco, CA"
    ##  [4] "Los Angeles, CA"   "Austin, TX"        "Atlanta, GA"      
    ##  [7] "Seattle, WA"       "New York City, NY" "Dallas, TX"       
    ## [10] "Portland, ME"

``` r
city_sales <- aggregate(Sales.Total~City.State, data = sales_df_v2, sum)
city_sales
```

    ##           City.State Sales.Total
    ## 1        Atlanta, GA      274116
    ## 2         Austin, TX      181455
    ## 3         Boston, MA      369135
    ## 4         Dallas, TX      274505
    ## 5    Los Angeles, CA      544442
    ## 6  New York City, NY      457145
    ## 7       Portland, ME       44668
    ## 8       Portland, OR      184828
    ## 9  San Francisco, CA      822978
    ## 10       Seattle, WA      273110

``` r
sales_df_v2 %>% 
  group_by(City.State) %>% 
  summarize(monthly_total = sum(Sales.Total)) %>% 
  #arrange(member_casual, weekday) %>% 
  ggplot(aes(x = City.State, y = monthly_total)) +
  geom_col(fill = '#004c6d') +
  scale_y_continuous(labels = comma_format()) +
  theme(axis.text.x = element_text(angle = 25), 
        plot.title = element_text(size=18, margin=margin(15,0,15,0))) + 
  labs(title = "Sales in USD ($) by City") + 
  ylab("Sales total") +
  xlab("City")
```

![](sales_data_analysis_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

### A2.

San Francisco, CA had the highest number of sales. <br>

### Q3.

What time should we display advertisements to maximize the likelihood of
customer buying product? <br>

``` r
# Use lubridate to extract hours and minutes 

sales_df_v2$Order.Date <- as.character(sales_df_v2$Order.Date) # convert from factor type to character type
class(sales_df_v2$Order.Date) # [1] "character"
```

    ## [1] "character"

``` r
sales_df_v2 <- sales_df_v2 %>%
  mutate(Order.Date = mdy_hm(Order.Date))

sales_df_v2 <- sales_df_v2 %>% mutate(
                    hour = hour(Order.Date),
                    minute = minute(Order.Date)) 

sales_df_v2 %>% 
  group_by(hour) %>% 
  summarize(count = n()) %>% 
  #arrange(member_casual, weekday) %>% 
  ggplot(aes(x = hour, y = count)) +
  geom_line(size=1,color='#004c6d') +
  scale_y_continuous(labels = comma_format()) +
  labs(title = "Orders by hour") + 
  ylab("Order") +
  xlab("Hour")
```

![](sales_data_analysis_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

### A3.

We have peaks at approx. 11:00 (11 am) and 19:00 (7 pm.) These are the
times we should be targeting for advertising.
