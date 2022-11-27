
library(ggplot2)
library(dplyr)
library(tibble)
library(stringr)
library(tidyverse)
# library(hrbrthemes)

# The below is used to manipulate data and save in .rda format and will need to use SQlite in furture update


# Set working directory
setwd("~/Desktop/Streeteasy Shiny App")


# read files
median_rent_all <- read.csv('./Data/AllSizes/medianAskingRent_All.csv', header = TRUE)

inventory_all <- read.csv('./Data/AllSizes/rentalInventory_All.csv', header = TRUE)

discount_all <- read.csv('./Data/AllSizes/discountShare_All.csv', header = TRUE)


# filter files by removing neighborhood names no longer used and narrow down the date
median_rent_all_manhattan<- median_rent_all %>%
  filter(Borough == 'Manhattan'  & areaName != 'Civic Center' & areaName != 'Marble Hill') %>%
  select(areaName, X2017.01:X2022.10)     

inventory_all_manhattan<- inventory_all %>%
  filter(Borough == 'Manhattan' & areaName != 'Civic Center' & areaName != 'Marble Hill') %>%
  select(areaName, X2017.01:X2022.10)     


discount_all_manhattan<- discount_all %>%
  filter(Borough == 'Manhattan' & areaName != 'Civic Center' & areaName != 'Marble Hill') %>%
  select(areaName, X2017.01:X2022.10)     



# How to switch (transpose) columns and rows and make the dates as observations instead of variables
median_rent_all_manhattant <- data.frame(t(median_rent_all_manhattan[-1]))
colnames(median_rent_all_manhattant) <- median_rent_all_manhattan[, 1]

inventory_all_manhattant <- data.frame(t(inventory_all_manhattan[-1]))
colnames(inventory_all_manhattant) <- inventory_all_manhattan[, 1]

discount_all_manhattant <- data.frame(t(discount_all_manhattan[-1]))
colnames(discount_all_manhattant) <- discount_all_manhattan[, 1]


# give a name to the first column

median_rent_all_manhattant <- tibble::rownames_to_column(median_rent_all_manhattant, "Date")

inventory_all_manhattant <- tibble::rownames_to_column(inventory_all_manhattant, "Date")

discount_all_manhattant <- tibble::rownames_to_column(discount_all_manhattant, "Date")



# Remove all the spaces in neighborhood names and replace with a dot(.)
median_rent_all_manhattant <- median_rent_all_manhattant %>% rename_with(make.names)

inventory_all_manhattant <- inventory_all_manhattant %>% rename_with(make.names)

discount_all_manhattant <- discount_all_manhattant %>% rename_with(make.names)


# change date from X2017.01 to 01.2017.01 to allow date format(day.year.month)

median_rent_all_manhattant <- median_rent_all_manhattant %>% mutate(across('Date', str_replace, 'X', '01.' ))

inventory_all_manhattant <- inventory_all_manhattant %>% mutate(across('Date', str_replace, 'X', '01.' ))

discount_all_manhattant <- discount_all_manhattant %>% mutate(across('Date', str_replace, 'X', '01.' ))

# change Date column to date format

median_rent_all_manhattant$Date <- as.Date(median_rent_all_manhattant$Date, "%d.%Y.%m")

inventory_all_manhattant$Date <- as.Date(inventory_all_manhattant$Date, "%d.%Y.%m")

discount_all_manhattant$Date <- as.Date(discount_all_manhattant$Date, "%d.%Y.%m")

# check for missing data
which(is.na(median_rent_all_manhattant))

which(is.na(inventory_all_manhattant))

which(is.na(discount_all_manhattant))


# converted to long dataset which works better for ggplot with multiple lines
avg_rent_all_long <- 
  median_rent_all_manhattant %>% 
  pivot_longer(cols = -Date, names_to = "category", values_to = "value")

inventory_all_long <- 
  inventory_all_manhattant %>% 
  pivot_longer(cols = -Date, names_to = "category", values_to = "value")

discount_all_long <- 
  discount_all_manhattant %>% 
  pivot_longer(cols = -Date, names_to = "category", values_to = "value")

# Saving as an Rdataframe to be used in shiny app
save(avg_rent_all_long ,file="avg_rent_all_long.Rda")
save(inventory_all_long ,file="inventory_all_long.Rda")
save(discount_all_long ,file="discount_all_long.Rda")


# set min and max dates based on first dataset.  min and max from all datasets are the same
date_min = min(avg_rent_all_long$Date)
date_max = max(avg_rent_all_long$Date)


# type of data
class(discount_all_long)
typeof(discount_all_long)
















# The below is figuring out how to make line chart with ggplot
ggplot(data = median_rent_all_manhattant, aes(x = Date, y = East.Village)) + geom_point()




ggplot(data = median_rent_all_manhattant, aes_string(colnames(median_rent_all_manhattant)[1], colnames(median_rent_all_manhattant)[2])) + geom_point() +
  geom_line(color = "steelblue") +
  xlab("") + theme(axis.text.x=element_text(angle=60, hjust = 1)) +
  scale_x_date(limit=c(as.Date("2021-10-01"),as.Date("2022-10-01")))


avg_rent_all_long %>% 
   filter(category == 'Chelsea') %>% 
  ggplot(
    aes(x = Date, y = value, colour = category)
  ) + 
  geom_line() +
  xlab("") + theme(axis.text.x=element_text(angle=60, hjust = 1)) +
  scale_x_date(limit=c(as.Date("2021-10-01"),as.Date("2022-10-01")))


avg_rent_all_long %>%
  filter(category =="Flatiron",
         Date >= "2017-01-01" &
           Date <= "2020-01-01") %>% 
  sum()

sel_avg <- avg_rent_all_long %>%
  group_by(category) %>% 
  filter(category ==  c("Flatiron","Chelsea"),
         Date >= "2017-01-01" &
           Date <= "2022-01-01") %>% 
  summarise(round((last(value)- first(value))/first(value)*100))
 
paste0(mean(sel_avg[[2]]), " %")

mean(sel_avg[[2]])
format(mean(sel_avg[[2]]), nsmall = 2)


avg_rent_all_long %>%
  filter(category ==  c("Flatiron","Chelsea"),
         Date >= "2017-01-01" &
           Date <= "2022-01-01") 

mean(avg_rent_all_long[[3]])


