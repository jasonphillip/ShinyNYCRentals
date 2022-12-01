
library(ggplot2)
library(dplyr)
library(tibble)
library(stringr)
library(tidyverse)

# library(hrbrthemes)

# The below is used to manipulate data and save in .rda format and will need to use SQlite in furture update


# df=data.frame(country=c("US", "GB", "BR"), 
#               val1=c(10,13,14), 
#               val2=c(23,12,32))

# df <- median_rent_all_manhattant %>%
#   select(Date, Flatiron, Chelsea)
#   # filter(col ==  c("Flatiron","Chelsea"),
#   #        Date >= "2017-01-01" &
#   #          Date <= "2022-01-01") 
# Line <- gvisLineChart(df)
# plot(Line)
# 



# Set working directory
setwd("~/Desktop/NYCDS_Bootcamp/ShinyNYCRentals")


# read files
median_rent_all <- read.csv('./Data/AllSizes/medianAskingRent_All.csv', header = TRUE)

inventory_all <- read.csv('./Data/AllSizes/rentalInventory_All.csv', header = TRUE)

discount_all <- read.csv('./Data/AllSizes/discountShare_All.csv', header = TRUE)

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

# filter files by removing neighborhood names no longer used and narrow down the date
median_rent_all_manhattan<- median_rent_all %>%
  filter(Borough == 'Manhattan'  & areaName != 'Civic Center' & areaName != 'Marble Hill') %>%
  select(areaName, X2014.01:X2022.10)     

inventory_all_manhattan<- inventory_all %>%
  filter(Borough == 'Manhattan' & areaName != 'Civic Center' & areaName != 'Marble Hill') %>%
  select(areaName, X2014.01:X2022.10)     

discount_all_manhattan<- discount_all %>%
  filter(Borough == 'Manhattan' & areaName != 'Civic Center' & areaName != 'Marble Hill') %>%
  select(areaName, X2014.01:X2022.10)    

median_rent_studio<- median_rent_studio %>%
  filter(Borough == 'Manhattan'  & areaName != 'Civic Center' & areaName != 'Marble Hill') %>%
  select(areaName, X2014.01:X2022.10)     

inventory_studio<- inventory_studio %>%
  filter(Borough == 'Manhattan' & areaName != 'Civic Center' & areaName != 'Marble Hill') %>%
  select(areaName, X2014.01:X2022.10)     


discount_studio<- discount_studio %>%
  filter(Borough == 'Manhattan' & areaName != 'Civic Center' & areaName != 'Marble Hill') %>%
  select(areaName, X2014.01:X2022.10)     

median_rent_one<- median_rent_one %>%
  filter(Borough == 'Manhattan'  & areaName != 'Civic Center' & areaName != 'Marble Hill') %>%
  select(areaName, X2014.01:X2022.10)     

inventory_one<- inventory_one %>%
  filter(Borough == 'Manhattan' & areaName != 'Civic Center' & areaName != 'Marble Hill') %>%
  select(areaName, X2014.01:X2022.10)     

discount_one<- discount_one %>%
  filter(Borough == 'Manhattan' & areaName != 'Civic Center' & areaName != 'Marble Hill') %>%
  select(areaName, X2014.01:X2022.10)    

median_rent_two<- median_rent_two %>%
  filter(Borough == 'Manhattan'  & areaName != 'Civic Center' & areaName != 'Marble Hill') %>%
  select(areaName, X2014.01:X2022.10)     

inventory_two<- inventory_two %>%
  filter(Borough == 'Manhattan' & areaName != 'Civic Center' & areaName != 'Marble Hill') %>%
  select(areaName, X2014.01:X2022.10)     


discount_two<- discount_two %>%
  filter(Borough == 'Manhattan' & areaName != 'Civic Center' & areaName != 'Marble Hill') %>%
  select(areaName, X2014.01:X2022.10) 

median_rent_three<- median_rent_three %>%
  filter(Borough == 'Manhattan'  & areaName != 'Civic Center' & areaName != 'Marble Hill') %>%
  select(areaName, X2014.01:X2022.10)     

inventory_three<- inventory_three %>%
  filter(Borough == 'Manhattan' & areaName != 'Civic Center' & areaName != 'Marble Hill') %>%
  select(areaName, X2014.01:X2022.10)     

discount_three<- discount_three %>%
  filter(Borough == 'Manhattan' & areaName != 'Civic Center' & areaName != 'Marble Hill') %>%
  select(areaName, X2014.01:X2022.10)    


# How to switch (transpose) columns and rows and make the dates as observations instead of variables
median_rent_all_manhattant <- data.frame(t(median_rent_all_manhattan[-1]))
colnames(median_rent_all_manhattant) <- median_rent_all_manhattan[, 1]

inventory_all_manhattant <- data.frame(t(inventory_all_manhattan[-1]))
colnames(inventory_all_manhattant) <- inventory_all_manhattan[, 1]

discount_all_manhattant <- data.frame(t(discount_all_manhattan[-1]))
colnames(discount_all_manhattant) <- discount_all_manhattan[, 1]

median_rent_studiot <- data.frame(t(median_rent_studio[-1]))
colnames(median_rent_studiot) <- median_rent_studio[, 1]

inventory_studiot <- data.frame(t(inventory_studio[-1]))
colnames(inventory_studiot) <- inventory_studio[, 1]

discount_studiot <- data.frame(t(discount_studio[-1]))
colnames(discount_studiot) <- discount_studio[, 1]

median_rent_onet <- data.frame(t(median_rent_one[-1]))
colnames(median_rent_onet) <- median_rent_one[, 1]

inventory_onet <- data.frame(t(inventory_one[-1]))
colnames(inventory_onet) <- inventory_one[, 1]

discount_onet <- data.frame(t(discount_one[-1]))
colnames(discount_onet) <- discount_one[, 1]

median_rent_twot <- data.frame(t(median_rent_two[-1]))
colnames(median_rent_twot) <- median_rent_two[, 1]

inventory_twot <- data.frame(t(inventory_two[-1]))
colnames(inventory_twot) <- inventory_two[, 1]

discount_twot <- data.frame(t(discount_two[-1]))
colnames(discount_twot) <- discount_two[, 1]


median_rent_threet <- data.frame(t(median_rent_three[-1]))
colnames(median_rent_threet) <- median_rent_three[, 1]

inventory_threet <- data.frame(t(inventory_three[-1]))
colnames(inventory_threet) <- inventory_three[, 1]

discount_threet <- data.frame(t(discount_three[-1]))
colnames(discount_threet) <- discount_three[, 1]



# give a name to the first column

median_rent_all_manhattant <- tibble::rownames_to_column(median_rent_all_manhattant, "Date")

inventory_all_manhattant <- tibble::rownames_to_column(inventory_all_manhattant, "Date")

discount_all_manhattant <- tibble::rownames_to_column(discount_all_manhattant, "Date")

median_rent_studiot <- tibble::rownames_to_column(median_rent_studiot, "Date")

inventory_studiot <- tibble::rownames_to_column(inventory_studiot, "Date")

discount_studiot <- tibble::rownames_to_column(discount_studiot, "Date")

median_rent_onet <- tibble::rownames_to_column(median_rent_onet, "Date")

inventory_onet <- tibble::rownames_to_column(inventory_onet, "Date")

discount_onet <- tibble::rownames_to_column(discount_onet, "Date")

median_rent_twot <- tibble::rownames_to_column(median_rent_twot, "Date")

inventory_twot <- tibble::rownames_to_column(inventory_twot, "Date")

discount_twot <- tibble::rownames_to_column(discount_twot, "Date")

median_rent_threet <- tibble::rownames_to_column(median_rent_threet, "Date")

inventory_threet <- tibble::rownames_to_column(inventory_threet, "Date")

discount_threet <- tibble::rownames_to_column(discount_threet, "Date")


# Remove all the spaces in neighborhood names and replace with a dot(.)
median_rent_all_manhattant <- median_rent_all_manhattant %>% rename_with(make.names)

inventory_all_manhattant <- inventory_all_manhattant %>% rename_with(make.names)

discount_all_manhattant <- discount_all_manhattant %>% rename_with(make.names)

median_rent_studiot <- median_rent_studiot %>% rename_with(make.names)

inventory_studiot <- inventory_studiot %>% rename_with(make.names)

discount_studiot <- discount_studiot %>% rename_with(make.names)

median_rent_onet <- median_rent_onet %>% rename_with(make.names)

inventory_onet <- inventory_onet %>% rename_with(make.names)

discount_onet <- discount_onet %>% rename_with(make.names)

median_rent_twot <- median_rent_twot %>% rename_with(make.names)

inventory_twot <- inventory_twot %>% rename_with(make.names)

discount_twot <- discount_twot %>% rename_with(make.names)

median_rent_threet <- median_rent_threet %>% rename_with(make.names)

inventory_threet <- inventory_threet %>% rename_with(make.names)

discount_threet <- discount_threet %>% rename_with(make.names)


# change date from X2017.01 to 01.2017.01 to allow date format(day.year.month)

median_rent_all_manhattant <- median_rent_all_manhattant %>% mutate(across('Date', str_replace, 'X', '01.' ))

inventory_all_manhattant <- inventory_all_manhattant %>% mutate(across('Date', str_replace, 'X', '01.' ))

discount_all_manhattant <- discount_all_manhattant %>% mutate(across('Date', str_replace, 'X', '01.' ))

median_rent_studiot <- median_rent_studiot %>% mutate(across('Date', str_replace, 'X', '01.' ))

inventory_studiot <- inventory_studiot %>% mutate(across('Date', str_replace, 'X', '01.' ))

discount_studiot <- discount_studiot %>% mutate(across('Date', str_replace, 'X', '01.' ))


median_rent_onet <- median_rent_onet %>% mutate(across('Date', str_replace, 'X', '01.' ))

inventory_onet <- inventory_onet %>% mutate(across('Date', str_replace, 'X', '01.' ))

discount_onet <- discount_onet %>% mutate(across('Date', str_replace, 'X', '01.' ))

median_rent_twot <- median_rent_twot %>% mutate(across('Date', str_replace, 'X', '01.' ))

inventory_twot <- inventory_twot %>% mutate(across('Date', str_replace, 'X', '01.' ))

discount_twot <- discount_twot %>% mutate(across('Date', str_replace, 'X', '01.' ))

median_rent_threet <- median_rent_threet %>% mutate(across('Date', str_replace, 'X', '01.' ))

inventory_threet <- inventory_threet %>% mutate(across('Date', str_replace, 'X', '01.' ))

discount_threet <- discount_threet %>% mutate(across('Date', str_replace, 'X', '01.' ))

# change Date column to date format

median_rent_all_manhattant$Date <- as.Date(median_rent_all_manhattant$Date, "%d.%Y.%m")

inventory_all_manhattant$Date <- as.Date(inventory_all_manhattant$Date, "%d.%Y.%m")

discount_all_manhattant$Date <- as.Date(discount_all_manhattant$Date, "%d.%Y.%m")

median_rent_studiot$Date <- as.Date(median_rent_studiot$Date, "%d.%Y.%m")

inventory_studiot$Date <- as.Date(inventory_studiot$Date, "%d.%Y.%m")

discount_studiot$Date <- as.Date(discount_studiot$Date, "%d.%Y.%m")

median_rent_onet$Date <- as.Date(median_rent_onet$Date, "%d.%Y.%m")

inventory_onet$Date <- as.Date(inventory_onet$Date, "%d.%Y.%m")

discount_onet$Date <- as.Date(discount_onet$Date, "%d.%Y.%m")

median_rent_twot$Date <- as.Date(median_rent_twot$Date, "%d.%Y.%m")

inventory_twot$Date <- as.Date(inventory_twot$Date, "%d.%Y.%m")

discount_twot$Date <- as.Date(discount_twot$Date, "%d.%Y.%m")

median_rent_threet$Date <- as.Date(median_rent_threet$Date, "%d.%Y.%m")

inventory_threet$Date <- as.Date(inventory_threet$Date, "%d.%Y.%m")

discount_threet$Date <- as.Date(discount_threet$Date, "%d.%Y.%m")



# check for missing data
which(is.na(median_rent_all_manhattant))

which(is.na(inventory_all_manhattant))

which(is.na(discount_all_manhattant))


which(is.na(median_rent_studiot))

which(is.na(inventory_studiot))

which(is.na(discount_studiot))


which(is.na(median_rent_onet))

which(is.na(inventory_onet))

which(is.na(discount_onet))


which(is.na(median_rent_twot))

which(is.na(inventory_twot))

which(is.na(discount_twot))


which(is.na(median_rent_threet))

which(is.na(inventory_threet))

which(is.na(discount_threet))






#filling NA's and removing columns with no data

median_rent_studiot <- 
  median_rent_studiot %>%
  fill(All.Downtown:West.Village, .direction = 'up') %>% 
  fill(All.Downtown:West.Village, .direction = 'down') %>% 
  select( -Stuyvesant.Town.PCV)



inventory_studiot <- 
  inventory_studiot %>%
  fill(All.Downtown:West.Village, .direction = 'up') %>% 
  fill(All.Downtown:West.Village, .direction = 'down') %>% 
  select( -Stuyvesant.Town.PCV)



discount_studiot <- 
  discount_studiot %>%
  fill(All.Downtown:West.Village, .direction = 'up') %>% 
  fill(All.Downtown:West.Village, .direction = 'down') %>% 
  select( -Stuyvesant.Town.PCV)





median_rent_onet <- 
  median_rent_onet %>%
  fill(All.Downtown:West.Village, .direction = 'up') %>% 
  fill(All.Downtown:West.Village, .direction = 'down') %>% 
  select( -Stuyvesant.Town.PCV)



inventory_onet <- 
  inventory_onet %>%
  fill(All.Downtown:West.Village, .direction = 'up') %>% 
  fill(All.Downtown:West.Village, .direction = 'down') %>% 
  select( -Stuyvesant.Town.PCV)



discount_onet <- 
  discount_onet %>%
  fill(All.Downtown:West.Village, .direction = 'up') %>% 
  fill(All.Downtown:West.Village, .direction = 'down') %>% 
  select( -Stuyvesant.Town.PCV)






median_rent_twot <- 
  median_rent_twot %>%
  fill(All.Downtown:West.Village, .direction = 'up') %>% 
  fill(All.Downtown:West.Village, .direction = 'down') %>% 
  select( -Stuyvesant.Town.PCV)



inventory_twot <- 
  inventory_twot %>%
  fill(All.Downtown:West.Village, .direction = 'up') %>% 
  fill(All.Downtown:West.Village, .direction = 'down') %>% 
  select( -Stuyvesant.Town.PCV)



discount_twot <- 
  discount_twot %>%
  fill(All.Downtown:West.Village, .direction = 'up') %>% 
  fill(All.Downtown:West.Village, .direction = 'down') %>% 
  select( -Stuyvesant.Town.PCV)






median_rent_threet <- 
  median_rent_threet %>%
  fill(All.Downtown:West.Village, .direction = 'up') %>% 
  fill(All.Downtown:West.Village, .direction = 'down') %>% 
  select( -Stuyvesant.Town.PCV)



inventory_threet <- 
  inventory_threet %>%
  fill(All.Downtown:West.Village, .direction = 'up') %>% 
  fill(All.Downtown:West.Village, .direction = 'down') %>% 
  select( -Stuyvesant.Town.PCV)



discount_threet <- 
  discount_threet %>%
  fill(All.Downtown:West.Village, .direction = 'up') %>% 
  fill(All.Downtown:West.Village, .direction = 'down') %>% 
  select( -Stuyvesant.Town.PCV)











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

avg_rent_studio_long <- 
  median_rent_studiot %>% 
  pivot_longer(cols = -Date, names_to = "category", values_to = "value")

inventory_studio_long <- 
  inventory_studiot %>% 
  pivot_longer(cols = -Date, names_to = "category", values_to = "value")

discount_studio_long <- 
  discount_studiot %>% 
  pivot_longer(cols = -Date, names_to = "category", values_to = "value")

avg_rent_one_long <- 
  median_rent_onet %>% 
  pivot_longer(cols = -Date, names_to = "category", values_to = "value")

inventory_one_long <- 
  inventory_onet %>% 
  pivot_longer(cols = -Date, names_to = "category", values_to = "value")

discount_one_long <- 
  discount_onet %>% 
  pivot_longer(cols = -Date, names_to = "category", values_to = "value")

avg_rent_two_long <- 
  median_rent_twot %>% 
  pivot_longer(cols = -Date, names_to = "category", values_to = "value")

inventory_two_long <- 
  inventory_twot %>% 
  pivot_longer(cols = -Date, names_to = "category", values_to = "value")

discount_two_long <- 
  discount_twot %>% 
  pivot_longer(cols = -Date, names_to = "category", values_to = "value")

avg_rent_three_long <- 
  median_rent_threet %>% 
  pivot_longer(cols = -Date, names_to = "category", values_to = "value")

inventory_three_long <- 
  inventory_threet %>% 
  pivot_longer(cols = -Date, names_to = "category", values_to = "value")

discount_three_long <- 
  discount_threet %>% 
  pivot_longer(cols = -Date, names_to = "category", values_to = "value")


# Fill NA data

# avg_rent_studio_long2 <- 
#   avg_rent_studio_long %>%
#   group_by(category)
#   fill(value, .direction = 'up')

# Saving as an Rdataframe to be used in shiny app
save(avg_rent_all_long ,file="avg_rent_all_long.Rda")
save(inventory_all_long ,file="inventory_all_long.Rda")
save(discount_all_long ,file="discount_all_long.Rda")

save(avg_rent_studio_long ,file="avg_rent_studio_long.Rda")
save(inventory_studio_long ,file="inventory_studio_long.Rda")
save(discount_studio_long ,file="discount_studio_long.Rda")

save(avg_rent_one_long ,file="avg_rent_one_long.Rda")
save(inventory_one_long ,file="inventory_one_long.Rda")
save(discount_one_long ,file="discount_one_long.Rda")

save(avg_rent_two_long ,file="avg_rent_two_long.Rda")
save(inventory_two_long ,file="inventory_two_long.Rda")
save(discount_two_long ,file="discount_two_long.Rda")

save(avg_rent_three_long ,file="avg_rent_three_long.Rda")
save(inventory_three_long ,file="inventory_three_long.Rda")
save(discount_three_long ,file="discount_three_long.Rda")




# set min and max dates based on first dataset.  min and max from all datasets are the same
date_min = min(avg_rent_all_long$Date)
date_max = max(avg_rent_all_long$Date)


# type of data
class(discount_all_long)
typeof(discount_all_long)
















# The below is figuring out how to make line chart with ggplot
ggplot(data = median_rent_all_manhattant, aes(x = Date, y = East.Village)) + geom_point()




ggplot(data = median_rent_all_manhattant, 
       aes_string(colnames(median_rent_all_manhattant)[1], 
                  colnames(median_rent_all_manhattant)[2])) +
  geom_point() +
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


avg_rent_studio_long %>%
  filter(category ==  c("Flatiron","Chelsea"),
         Date >= "2017-01-01" &
           Date <= "2022-01-01") 

mean(avg_rent_all_long[[3]])



avg_rent_studio_long %>%  
  group_by(category) %>%
  filter(category %in% input$category,
         Date >= input$date_range[[1]] &
           Date <= input$date_range[[2]]) %>%
  summarise(first(value))
# taking the average percent of change if more than one neighborhood is selected
med_size <- paste0("$ ", mean(mediansize[[2]]))


load("discount_all_long.Rda")

t2 <- discount_all_long %>%
  group_by(category) %>% 
  filter(category ==  c("Flatiron","Chelsea"),
         Date >= "2017-01-01" &
           Date <= "2022-01-01") %>% 
  summarise(first(value))
med_size <- paste0(mean(t2[[2]]))
med_size <- paste0("$ ", mean(mediansize[[2]]))


avg_rent_studio_long %>%
  group_by(category) %>% 
  filter(category ==  c("Flatiron","Chelsea"),
         Date >= "2017-01-01" &
           Date <= "2022-01-01") %>% 
  summarise(round((last(value)- first(value))/first(value)*100))


summarise(first(value))
# taking the average percent of change if more than one neighborhood is selected
med_size <- paste0(round(mean(mediansize[[2]])))
infoBox("Starting Discounted Listings", med_size, icon = icon("calculator"))


# ui: code in the first tabPanel 
tabPanel(title = "Home",
         imageOutput("map_img")
)

# add output in server
output$map_img <- renderImage({
  
  list(src = "www/manhattan-neighborhood-map.png",
       width = "100%",
       height = 330)
  
}, deleteFile = F)



