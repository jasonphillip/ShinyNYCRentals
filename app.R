
# using one file for shiny app instead of 3(global, ui and server)

#set working directory
setwd("~/Desktop/Shiny_App_NYC_Rental")

# loading libraries
library(shinydashboard)
library(dplyr)
library(tibble)
library(stringr)
library(tidyverse)
library(ggplot2)

# loading datafranes created in pre_process.  Will need to use SQLite in future update
load("avg_rent_all_long.Rda")
load("inventory_all_long.Rda")
load("discount_all_long.Rda")


# setting min and max for date range(all files loaded have the same date range)
date_min = min(avg_rent_all_long$Date)
date_max = max(avg_rent_all_long$Date)




ui <- dashboardPage(
  dashboardHeader(title = "Manhattan Rentals"),
  dashboardSidebar(
    sidebarUserPanel("Jason Phillip", image = "https://www.wikihow.com/images/thumb/a/ab/Jason_Phillip.png/-crop-320-320-320px-Jason_Phillip.png"),
    sidebarMenu(
      menuItem("Info",
               tabName = "info",
               icon = icon("th")),
      menuItem(
        "All Apartment Sizes",
        tabName = "All",
        icon = icon("home", lib = "glyphicon"),
        selected = TRUE  # app opens on main page which is currently all apartment sizes
        
      ),
      menuItem(
        "One Bedroom Place Holder",
        tabName = "one_bedroom",
        icon = icon("home", lib = "glyphicon")
      )
    )
  ),
  ## Body content
  dashboardBody(tabItems(
    # First tab content
    tabItem(tabName = "info",
            fluidRow(
              box(
                title = "Manhattan Apartment Rentals",
                width = 4,
                background = "light-blue",
                "The purpose of this app is to allow landlords or tenants to view how the rental market has changed over a period of time which can be helpful when a lease is up for renewal.
                   The line graph charts the amount of change in the average asking rent, the amount of inventory(number of units advertised for rent), and the percantage of units on the market
                   that have discounted the asking rent since it was listed.  Multiple neighborhoods can be selected since sometimes an apartment is on the border of
                   a particular neighborhood.  The 3 infoboxes show percantage of change for the selected dates for each of the three categories.  When more than one nighborhood is selected
                   the infobox will will average the neighborhoods together.
                   "
              )
              
              
            )),
    # Second tab content with graphs and infoboxes of all aparpent sizes
    tabItem(tabName = "All",
            fluidRow(
              box(
                width = '100%' ,
                h3(align = "center", style = 'font-size:32px;',
                   "Amount of Change for Selected Dates and Neighborhood(s)"), # doesnt look right.  Need to update
                infoBoxOutput("priceBox"),
                infoBoxOutput("inventoryBox"),
                infoBoxOutput("discountBox")
              )
            ),
            fluidRow(
              box(plotOutput("place_plot", height = 350)),
              box(
                radioButtons(
                  "file",
                  "Choose graph to display:",
                  inline = TRUE,
                  c(
                    "Average Asking Rent" = "avg_rent_all_long",
                    "Number of Listings" = "inventory_all_long",
                    "Percent of Listings with Discounted Asking Rents" = "discount_all_long"
                  )
                )
              ),
              box(
                dateRangeInput(
                  'date_range',
                  h3("Date range"),
                  format = "mm/yyyy", # pop up calendar still shows days instead of months  and years
                  start = date_min,
                  end = date_max,
                  min = date_min,
                  max = date_max
                )
              ),
              
              box(
                title = "Neighborhood",
                checkboxGroupInput(
                  'category',  
                  "Choose a neighborhood:",
                  inline = TRUE,
                  selected = "Battery.Park.City" ,
                  choices = list(   # created list by hand.  should of used a function?
                    "Battery Park City" = "Battery.Park.City",
                    "Central Harlem" = "Central.Harlem",
                    "Central Park South" = "Central.Park.South",
                    "Chelsea" = "Chelsea",
                    "Chinatown" = "Chinatown",
                    "East Harlem" = "East.Harlem",
                    "East Village" = "East.Village",
                    "Financial District" = "Financial.District",
                    "Flatiron" = "Flatiron",
                    "Gramercy Park" = "Gramercy.Park",
                    "Greenwich Village" = "Greenwich.Village",
                    "Hamilton Heights" = "Hamilton.Heights",
                    "Inwood" = "Inwood",
                    "Little Italy" = "Little.Italy",
                    "Lower East.Side" = "Lower.East.Side",
                    "Midtown" = "Midtown",
                    "Midtown East" = "Midtown.East",
                    "Midtown South" = "Midtown.South",
                    "Midtown West" = "Midtown.West",
                    "Morningside Heights" = "Morningside.Heights",
                    "Nolita" = "Nolita",
                    "Roosevelt Island " = "Roosevelt.Island",
                    "Soho" = "Soho",
                    "Stuyvesant Town" = "Stuyvesant.Town.PCV",
                    "Tribeca" = "Tribeca",
                    "Upper East Side" = "Upper.East.Side",
                    "Upper West Side" = 'Upper.West.Side',
                    "Washington Heights" = "Washington.Heights",
                    "West Harlem" = "West.Harlem",
                    "West Village" = "West.Village"
                  )
                ),
              )
            )),
    
    # Second tab content, considering adding a tab for each size apartment 
    tabItem(tabName = "one_bedroom",
            fluidRow(
              box(
                title = "Apartment Sizes",
                width = 4,
                background = "light-blue",
                "Placeholder to add different apartment sizes.
                   "
              )
            ))
  ))
)



server <- function(input, output) {
  place_plot <- reactive({  
    file1 <- switch(  # radio buttons in UI select file to be used for ggplot graph
      input$file,
      avg_rent_all_long = avg_rent_all_long,   
      inventory_all_long = inventory_all_long,
      discount_all_long = discount_all_long,
      avg_rent_all_long
    )
    file1 %>%  # creating plot after file is selected. 
      filter(category %in% input$category,
             Date >= input$date_range[[1]] &
               Date <= input$date_range[[2]]) %>%
      ggplot(aes(x = Date, y = value, colour = category)) +
      geom_line() +
      scale_colour_hue(name = "Neighborhood",      # Set legend title
                       l = 30) +
      xlab("") + theme(axis.text.x = element_text(angle = 60, hjust = 1))
    
  })
  
  output$place_plot <- renderPlot({  #rendering plot
    place_plot()
  })
  
  output$priceBox <- renderInfoBox({  
    sel_avg <- avg_rent_all_long %>%  # Creates dataframe from selected date range and neighborhood(category)
      group_by(category) %>%
      filter(category %in% input$category,
             Date >= input$date_range[[1]] &
               Date <= input$date_range[[2]]) %>%
      summarise((last(value) - first(value)) / first(value) * 100) 
    # taking the average percent of change if more than one neighborhood is selected 
    sel_avg_all <- paste0(round(mean(sel_avg[[2]])), " %")  
    infoBox("Asking Price", sel_avg_all, icon = icon("calculator"))
  })
  
  output$inventoryBox <- renderInfoBox({
    sel_avg <- inventory_all_long %>%
      group_by(category) %>%
      filter(category %in% input$category,
             Date >= input$date_range[[1]] &
               Date <= input$date_range[[2]]) %>%
      summarise((last(value) - first(value)) / first(value) * 100)
    sel_avg_all <- paste0(round(mean(sel_avg[[2]])), " %")
    infoBox("Number of Listings", sel_avg_all, icon = icon("calculator"))
  })
  
  output$discountBox <- renderInfoBox({
    sel_avg <- discount_all_long %>%
      group_by(category) %>%
      filter(category %in% input$category,
             Date >= input$date_range[[1]] &
               Date <= input$date_range[[2]]) %>%
      summarise((last(value) - first(value)) / first(value) * 100)
    sel_avg_all <- paste0(round(mean(sel_avg[[2]])), " %")
    infoBox("Discounted Listings", sel_avg_all, icon = icon("calculator"))
  })
  

}



# shinyApp(ui, server)
shinyApp(ui = ui, server = server)
