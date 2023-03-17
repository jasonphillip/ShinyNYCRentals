



# The below is used to import and manipulate the data.  The dataframe is then stored 
#  as an R dataframe so their is less processing for the shiny app



# load libraries ####
library(ggplot2)
library(dplyr)
library(tibble)
library(stringr)
library(tidyverse)






# Set working directory  ####


setwd("~/Desktop/NYCDS_Bootcamp/Data Analysis with R/Shiny/ShinyNYCRentals")



# Creating a function because I have multiple datasets to transform
process_rent_data <- function(rent_data) {
  
  # Filter files by removing neighborhood names no longer used and narrow down the date
  rent_data <- rent_data %>%
    filter(Borough == 'Manhattan' & areaName != 'Civic Center' & areaName != 'Marble Hill') %>%
    select(areaName, X2014.01:X2022.10)
  
  # Transpose columns and rows and make the dates as observations instead of variables
  rent_datat <- data.frame(t(rent_data[-1]))
  colnames(rent_datat) <- rent_data[, 1]
  
  # Giving a name to the first column
  rent_datat <- tibble::rownames_to_column(rent_datat, "Date")
  
  # Remove all the spaces in neighborhood names and replace with a dot(.)
  rent_datat <- rent_datat %>% rename_with(make.names)
  
  # Change date from X2017.01 to 01.2017.01 to allow date format(day.year.month)
  rent_datat <- rent_datat %>% mutate(across('Date', str_replace, 'X', '01.'))
  
  # Change Date column to date format
  rent_datat$Date <- as.Date(rent_datat$Date, "%d.%Y.%m")
  
  # Fill NA's and remove columns with no data
  rent_datat <- rent_datat %>%
    fill(All.Downtown:West.Village, .direction = 'up') %>%
    fill(All.Downtown:West.Village, .direction = 'down') %>%
    select(-Stuyvesant.Town.PCV)
  
  # Convert to long dataset which works better for ggplot with multiple lines
  rent_datat <- rent_datat %>%
    pivot_longer(cols = -Date, names_to = "category", values_to = "value")
  
  return(rent_datat)
}





# read files ####
median_rent_all <- read.csv('./Data/All/medianAskingRent_All.csv', header = TRUE)
inventory_all <- read.csv('./Data/All/rentalInventory_All.csv', header = TRUE)
discount_all <- read.csv('./Data/All/discountShare_All.csv', header = TRUE)

median_rent_studio <- read.csv('./Data/Studio/medianAskingRent_Studio.csv', header = TRUE)
inventory_studio <- read.csv('./Data/Studio/rentalInventory_Studio.csv', header = TRUE)
discount_studio <- read.csv('./Data/Studio/discountShare_Studio.csv', header = TRUE)

median_rent_one <- read.csv('./Data/OneBd/medianAskingRent_OneBd.csv', header = TRUE)
inventory_one <- read.csv('./Data/OneBd/rentalInventory_OneBd.csv', header = TRUE)
discount_one <- read.csv('./Data/OneBd/discountShare_OneBd.csv', header = TRUE)

median_rent_two <- read.csv('./Data/TwoBd/medianAskingRent_TwoBd.csv', header = TRUE)
inventory_two <- read.csv('./Data/TwoBd/rentalInventory_TwoBd.csv', header = TRUE)
discount_two <- read.csv('./Data/TwoBd/discountShare_TwoBd.csv', header = TRUE)

median_rent_three <- read.csv('./Data/ThreePlusBd/medianAskingRent_ThreePlusBd.csv', header = TRUE)
inventory_three <- read.csv('./Data/ThreePlusBd/rentalInventory_ThreePlusBd.csv', header = TRUE)
discount_three <- read.csv('./Data/ThreePlusBd/discountShare_ThreePlusBd.csv', header = TRUE) 



# Using function to process data

avg_rent_all_long <- process_rent_data(median_rent_all)
inventory_all_long <- process_rent_data(inventory_all)
discount_all_long <- process_rent_data(discount_all)

avg_rent_studio_long <- process_rent_data(median_rent_studio)
inventory_studio_long <- process_rent_data(inventory_studio)
discount_studio_long <- process_rent_data(discount_studio)

avg_rent_one_long <- process_rent_data(median_rent_one)
inventory_one_long <- process_rent_data(inventory_one)
discount_one_long <- process_rent_data(discount_one)

avg_rent_two_long <- process_rent_data(median_rent_two)
inventory_two_long <- process_rent_data(inventory_two)
discount_two_long <- process_rent_data(discount_two)

avg_rent_three_long <- process_rent_data(median_rent_three)
inventory_three_long <- process_rent_data(inventory_three)
discount_three_long <- process_rent_data(discount_three)



# Saving as an Rdataframe to be used in shiny app ####

save(avg_rent_all_long ,file="./ProcessedData/avg_rent_all_long.Rda")
save(inventory_all_long ,file="./ProcessedData/inventory_all_long.Rda")
save(discount_all_long ,file="./ProcessedData/discount_all_long.Rda")

save(avg_rent_studio_long ,file="./ProcessedData/avg_rent_studio_long.Rda")
save(inventory_studio_long ,file="./ProcessedData/inventory_studio_long.Rda")
save(discount_studio_long ,file="./ProcessedData/discount_studio_long.Rda")

save(avg_rent_one_long ,file="./ProcessedData/avg_rent_one_long.Rda")
save(inventory_one_long ,file="./ProcessedData/inventory_one_long.Rda")
save(discount_one_long ,file="./ProcessedData/discount_one_long.Rda")

save(avg_rent_two_long ,file="./ProcessedData/avg_rent_two_long.Rda")
save(inventory_two_long ,file="./ProcessedData/inventory_two_long.Rda")
save(discount_two_long ,file="./ProcessedData/discount_two_long.Rda")

save(avg_rent_three_long ,file="./ProcessedData/avg_rent_three_long.Rda")
save(inventory_three_long ,file="./ProcessedData/inventory_three_long.Rda")
save(discount_three_long ,file="./ProcessedData/discount_three_long.Rda")



