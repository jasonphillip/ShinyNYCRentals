

# using one file for shiny app instead of 3(global, ui and server)

# Global ####

#set working directory when run locally
# setwd("~/Desktop/NYCDS_Bootcamp/Data Analysis with R/Shiny/ShinyNYCRentals")

# loading libraries
library(shinydashboard)
library(dplyr)
library(tibble)
library(stringr)
library(tidyverse)
library(ggplot2)

# loading dataframes created in pre_process.  Will need to use SQLite in future update
load("./ProcessedData/avg_rent_all_long.Rda")
load("./ProcessedData/inventory_all_long.Rda")
load("./ProcessedData/discount_all_long.Rda")

load("./ProcessedData/avg_rent_studio_long.Rda")
load("./ProcessedData/inventory_studio_long.Rda")
load("./ProcessedData/discount_studio_long.Rda")

load("./ProcessedData/avg_rent_one_long.Rda")
load("./ProcessedData/inventory_one_long.Rda")
load("./ProcessedData/discount_one_long.Rda")

load("./ProcessedData/avg_rent_two_long.Rda")
load("./ProcessedData/inventory_two_long.Rda")
load("./ProcessedData/discount_two_long.Rda")

load("./ProcessedData/avg_rent_three_long.Rda")
load("./ProcessedData/inventory_three_long.Rda")
load("./ProcessedData/discount_three_long.Rda")


# Creating function to convert numbers to percentage
percent <- function(x,
                    digits = 2,
                    format = "f",
                    ...) {
  paste0(formatC(x * 100, format = format, digits = digits, ...), "%")
}



# setting min and max for date range(all files loaded have the same date range)
date_min = min(avg_rent_all_long$Date)
date_max = max(avg_rent_all_long$Date)



# UI ####
ui <- dashboardPage(
  dashboardHeader(title = "NYC Rentals"),
  dashboardSidebar(
    sidebarUserPanel(''),
    # SidebarMenu ####
    sidebarMenu(
      menuItem(
        "Info",
        tabName = "info",
        icon = icon("info-sign", lib = "glyphicon")
      ),
      menuItem(
        "Median Asking Rent",
        tabName = "median",
        icon = icon("home", lib = "glyphicon")
      ),
      menuItem(
        "Percent of Listings Discounted",
        tabName = "discounts",
        icon = icon("home", lib = "glyphicon")
      ),
      menuItem(
        "Amount of Listings",
        tabName = "listings",
        icon = icon("home", lib = "glyphicon")
      ),
      

      dateRangeInput(
        'date_range',
        h3("Date range"),
        format = "mm/yyyy",
        startview = 'year',
        start = date_min,
        end = date_max,
        min = date_min,
        max = date_max
      ),
      radioButtons(
        "size",
        "Choose Apartment Size:",
        c(
          "All Apartment Sizes" = "all",
          "Studio" = "studio",
          "One Bedroom" = "one",
          "Two Bedroom" = "two",
          "Three or More Bedrooms" = "three"
          
        )
      )
      
    )
  ),
  ## Body content ####
  dashboardBody(tabItems(

    
    # First tab content with Info ####
    tabItem(tabName = "info",
            fluidPage(

              box(p("Welcome the Manhattan Rental Trends App", style = 'font-size:42px;'),
                "Coded by: Jason Phillip <Jason.Phillip@gmail.com>" , style = 'font-size:20px;', align = "center", width = 12),
              box(
                width = '12', height = "700",
                "This app provides landlords with valuable information on how the rental market has changed since their tenant's last lease renewal. Landlords can adjust the start and end dates, apartment size, and neighborhood to align with their specific situation. Each of the three sections consists of information boxes that show the change for the selected time period, as well as a line graph that provides a clear picture of the overall trend. Additionally, if an apartment is located on the border of multiple neighborhoods, the app allows landlords to select multiple neighborhoods for a more accurate view of the data.",   style = 'font-size:20px;', align = "center",
                imageOutput("map_img")
              )
            )
            ),
    # Second tab content with graphs and infoboxes showing median rent ####
    tabItem(tabName = "median",
            fluidRow(
              box(
                width = '100%' ,
                infoBoxOutput("startBox"),
                infoBoxOutput("endBox"),
                infoBoxOutput("changeBox")
              )
            ),
            fluidRow(
              box(plotOutput("place_plot", height = 350)),

              box(
                checkboxGroupInput(
                  'category',
                  "Choose a neighborhood:",
                  inline = TRUE,
                  selected = "East.Village" ,
                  choices = list(
                    # List of neighborhoods
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
                    "Lower East Side" = "Lower.East.Side",
                    "Midtown" = "Midtown",
                    "Midtown East" = "Midtown.East",
                    "Midtown South" = "Midtown.South",
                    "Midtown West" = "Midtown.West",
                    "Morningside Heights" = "Morningside.Heights",
                    "Nolita" = "Nolita",
                    "Roosevelt Island " = "Roosevelt.Island",
                    "Soho" = "Soho",
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
    
 
    # Third tab content percent of discounted units ####
    tabItem(tabName = "discounts",
            fluidRow(
              box(
                width = '100%' ,
                h3(align = "center",
                   style = 'font-size:32px;',
                   ""),
                infoBoxOutput("startdiscBox"),
                infoBoxOutput("enddiscBox"),
                infoBoxOutput("changediscBox")
              )
            ),
            fluidRow(
              box(plotOutput("place3_plot", height = 350)),
              box(
                checkboxGroupInput(
                  'category3',
                  "Choose a neighborhood:",
                  inline = TRUE,
                  selected = "East.Village" ,
                  choices = list(
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
                    "Lower East Side" = "Lower.East.Side",
                    "Midtown" = "Midtown",
                    "Midtown East" = "Midtown.East",
                    "Midtown South" = "Midtown.South",
                    "Midtown West" = "Midtown.West",
                    "Morningside Heights" = "Morningside.Heights",
                    "Nolita" = "Nolita",
                    "Roosevelt Island " = "Roosevelt.Island",
                    "Soho" = "Soho",
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
    
# Fourth tab content number of listings on market ####
    tabItem(tabName = "listings",
            fluidRow(
              box(
                width = '100%' ,
                h3(align = "center",
                   style = 'font-size:32px;',
                   ""),
                infoBoxOutput("startlistBox"),
                infoBoxOutput("endlistBox"),
                infoBoxOutput("changelistBox")
              )
            ),
            fluidRow(
              box(plotOutput("place2_plot", height = 350)),
              box(
                checkboxGroupInput(
                  'category2',
                  "Choose a neighborhood:",
                  inline = TRUE,
                  selected = "East.Village" ,
                  choices = list(
                    # created list by hand.  should of used a function?
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
                    "Lower East Side" = "Lower.East.Side",
                    "Midtown" = "Midtown",
                    "Midtown East" = "Midtown.East",
                    "Midtown South" = "Midtown.South",
                    "Midtown West" = "Midtown.West",
                    "Morningside Heights" = "Morningside.Heights",
                    "Nolita" = "Nolita",
                    "Roosevelt Island " = "Roosevelt.Island",
                    "Soho" = "Soho",
                    "Tribeca" = "Tribeca",
                    "Upper East Side" = "Upper.East.Side",
                    "Upper West Side" = 'Upper.West.Side',
                    "Washington Heights" = "Washington.Heights",
                    "West Harlem" = "West.Harlem",
                    "West Village" = "West.Village"
                  )
                ),
              )
            ))
  ))
)

#  SERVER ####

server <- function(input, output) {
  # Line chart for median rent (second tab) ####
  place_plot <- reactive({
    mediansize <-
      switch(
        # radio buttons in UI select file to be used for ggplot graph
        input$size,
        all = avg_rent_all_long,
        studio = avg_rent_studio_long,
        one = avg_rent_one_long,
        two = avg_rent_two_long,
        three = avg_rent_three_long,
        avg_rent_all_long
      )
    mediansize %>%  # creating plot after file is selected.
      filter(category %in% input$category,
             Date >= input$date_range[[1]] &
               Date <= input$date_range[[2]]) %>%
      ggplot(aes(x = Date, y = value, colour = category)) +
      geom_line() +
      scale_colour_hue(name = "Neighborhood",    # Set legend title
                       l = 30) +
      ylim(0,NA) +
      xlab("") +
      ylab("Median Asking Rent") +
      theme(axis.text.x = element_text(angle = 60, hjust = 1))
  })
  output$place_plot <- renderPlot({
    #rendering plot
    place_plot()
  })
  
  
  # Info boxes for median rent (second tab) ####
  med_start_box <- reactive({
    mediansize <-
      switch(
        # radio buttons in UI select file to be used for ggplot graph
        input$size,
        all = avg_rent_all_long,
        studio = avg_rent_studio_long,
        one = avg_rent_one_long,
        two = avg_rent_two_long,
        three = avg_rent_three_long,
        avg_rent_all_long
      )
    mediansize <- mediansize %>%
      group_by(category) %>%
      filter(category %in% input$category,
             Date >= input$date_range[[1]] &
               Date <= input$date_range[[2]]) %>%
      summarise(first(value))
    # taking the average percent of change if more than one neighborhood is selected
    med_size <- paste0("$ ", round(mean(mediansize[[2]])))
    infoBox("Starting Price", med_size, icon = icon("calculator"))
    
  })
  output$startBox <- renderInfoBox({
    med_start_box()
  })
  med_end_box <- reactive({
    mediansize <-
      switch(
        # radio buttons in UI select file to be used for ggplot graph
        input$size,
        all = avg_rent_all_long,
        studio = avg_rent_studio_long,
        one = avg_rent_one_long,
        two = avg_rent_two_long,
        three = avg_rent_three_long,
        avg_rent_all_long
      )
    mediansize <- mediansize %>%
      group_by(category) %>%
      filter(category %in% input$category,
             Date >= input$date_range[[1]] &
               Date <= input$date_range[[2]]) %>%
      summarise(last(value))
    # taking the average percent of change if more than one neighborhood is selected
    med_size <- paste0("$ ", round(mean(mediansize[[2]])))
    infoBox("Ending Price", med_size, icon = icon("calculator"))
    
  })
  output$endBox <- renderInfoBox({
    med_end_box()
  })
  
  med_change_box <- reactive({
    mediansize <-
      switch(
        # radio buttons in UI select file to be used for ggplot graph
        input$size,
        all = avg_rent_all_long,
        studio = avg_rent_studio_long,
        one = avg_rent_one_long,
        two = avg_rent_two_long,
        three = avg_rent_three_long,
        avg_rent_all_long
      )
    mediansize <- mediansize %>%
      group_by(category) %>%
      filter(category %in% input$category,
             Date >= input$date_range[[1]] &
               Date <= input$date_range[[2]]) %>%
      summarise((last(value) - first(value)) / first(value) * 100)
    med_size <- paste0(round(mean(mediansize[[2]])), " %")
    infoBox("Change", med_size, icon = icon("calculator"))
    
  })
  output$changeBox <- renderInfoBox({
    med_change_box()
  })
  
  
  # Line chart for discounted listings (third tab) ####
  place3_plot <- reactive({
    mediansize <-
      switch(
        # radio buttons in UI select file to be used for ggplot graph
        input$size,
        all = discount_all_long,
        studio = discount_studio_long,
        one = discount_one_long,
        two = discount_two_long,
        three = discount_three_long,
        discount_all_long
      )
    mediansize %>%  # creating plot after file is selected.
      filter(
        category %in% input$category3,
        Date >= input$date_range[[1]] &
          Date <= input$date_range[[2]]
      ) %>%
      ggplot(aes(x = Date, y = value, colour = category)) +
      geom_line() +
      scale_colour_hue(name = "Neighborhood",      # Set legend title
                       l = 30) +
      xlab("") +
      ylab("Percent of Listings Discounted") +
      scale_y_continuous(labels = scales::percent) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1))
  })
  output$place3_plot <- renderPlot({
    #rendering plot
    place3_plot()
  })
  

  # Info boxes for discounted listings (third tab) ####
  disc_start_box <- reactive({
    mediansize <-
      switch(
        # radio buttons in UI select file to be used for ggplot graph
        input$size,
        all = discount_all_long,
        studio = discount_studio_long,
        one = discount_one_long,
        two = discount_two_long,
        three = discount_three_long,
        discount_all_long
      )
    mediansize <- mediansize %>%
      group_by(category) %>%
      filter(
        category %in% input$category3,
        Date >= input$date_range[[1]] &
          Date <= input$date_range[[2]]
      ) %>%
      summarise(first(value))
    # taking the average percent of change if more than one neighborhood is selected
    med_size <- paste0(percent(mean(mediansize[[2]])))
    infoBox("Starting Discounted Listings",
            med_size,
            icon = icon("calculator"))
    
  })
  output$startdiscBox <- renderInfoBox({
    disc_start_box()
  })
  
  
  disc_end_box <- reactive({
    mediansize <-
      switch(
        # radio buttons in UI select file to be used for ggplot graph
        input$size,
        all = discount_all_long,
        studio = discount_studio_long,
        one = discount_one_long,
        two = discount_two_long,
        three = discount_three_long,
        discount_all_long
      )
    mediansize <- mediansize %>%
      group_by(category) %>%
      filter(
        category %in% input$category3,
        Date >= input$date_range[[1]] &
          Date <= input$date_range[[2]]
      ) %>%
      summarise(last(value))
    # taking the average percent of change if more than one neighborhood is selected
    med_size <- percent(mean(mediansize[[2]]))
    infoBox("Ending Discounted Units", med_size, icon = icon("calculator"))
    
  })
  output$enddiscBox <- renderInfoBox({
    disc_end_box()
  })
  
  disc_change_box <- reactive({
    mediansize <-
      switch(
        # radio buttons in UI select file to be used for ggplot graph
        input$size,
        all = discount_all_long,
        studio = discount_studio_long,
        one = discount_one_long,
        two = discount_two_long,
        three = discount_three_long,
        discount_all_long
      )
    mediansize <- mediansize %>%
      group_by(category) %>%
      filter(
        category %in% input$category3,
        Date >= input$date_range[[1]] &
          Date <= input$date_range[[2]]
      ) %>%
      summarise((last(value) - first(value)) / first(value) * 100)
    med_size <- paste0(round(mean(mediansize[[2]])), " %")
    infoBox("Change", med_size, icon = icon("calculator"))
    
  })
  output$changediscBox <- renderInfoBox({
    disc_change_box()
  })
  
  # Line chart for inventory (fourth tab) ####
  place2_plot <- reactive({
    mediansize <-
      switch(
        # radio buttons in UI select file to be used for ggplot graph
        input$size,
        all = inventory_all_long,
        studio = inventory_studio_long,
        one = inventory_one_long,
        two = inventory_two_long,
        three = inventory_three_long,
        inventory_all_long
      )
    mediansize %>%  # creating plot after file is selected.
      filter(
        category %in% input$category2,
        Date >= input$date_range[[1]] &
          Date <= input$date_range[[2]]
      ) %>%
      ggplot(aes(x = Date, y = value, colour = category)) +
      geom_line() +
      scale_colour_hue(name = "Neighborhood",      # Set legend title
                       l = 30) +
      ylim(0,NA) +
      xlab("") +
      ylab("Listings Available") +
      theme(axis.text.x = element_text(angle = 60, hjust = 1))
  })
  output$place2_plot <- renderPlot({
    #rendering plot
    place2_plot()
  })
  
  # Info boxes for inventory (fourth tab) ####
  list_start_box <- reactive({
    mediansize <-
      switch(
        # radio buttons in UI select file to be used for ggplot graph
        input$size,
        all = inventory_all_long,
        studio = inventory_studio_long,
        one = inventory_one_long,
        two = inventory_two_long,
        three = inventory_three_long,
        inventory_all_long
      )
    mediansize <- mediansize %>%
      group_by(category) %>%
      filter(
        category %in% input$category2,
        Date >= input$date_range[[1]] &
          Date <= input$date_range[[2]]
      ) %>%
      summarise(first(value))
    # taking the average percent of change if more than one neighborhood is selected
    med_size <- paste0(round(mean(mediansize[[2]])))
    infoBox("Starting Inventory", med_size, icon = icon("calculator"))
    
  })
  output$startlistBox <- renderInfoBox({
    list_start_box()
  })
  
  
  list_end_box <- reactive({
    mediansize <-
      switch(
        # radio buttons in UI select file to be used for ggplot graph
        input$size,
        all = inventory_all_long,
        studio = inventory_studio_long,
        one = inventory_one_long,
        two = inventory_two_long,
        three = inventory_three_long,
        inventory_all_long
      )
    mediansize <- mediansize %>%
      group_by(category) %>%
      filter(
        category %in% input$category2,
        Date >= input$date_range[[1]] &
          Date <= input$date_range[[2]]
      ) %>%
      summarise(last(value))
    # taking the average percent of change if more than one neighborhood is selected
    med_size <- paste0(round(mean(mediansize[[2]])))
    infoBox("Ending Inventory", med_size, icon = icon("calculator"))
    
  })
  output$endlistBox <- renderInfoBox({
    list_end_box()
  })
  
  list_change_box <- reactive({
    mediansize <-
      switch(
        # radio buttons in UI select file to be used for ggplot graph
        input$size,
        all = inventory_all_long,
        studio = inventory_studio_long,
        one = inventory_one_long,
        two = inventory_two_long,
        three = inventory_three_long,
        inventory_all_long
      )
    mediansize <- mediansize %>%
      group_by(category) %>%
      filter(
        category %in% input$category2,
        Date >= input$date_range[[1]] &
          Date <= input$date_range[[2]]
      ) %>%
      summarise((last(value) - first(value)) / first(value) * 100)
    med_size <- paste0(round(mean(mediansize[[2]])), " %")
    infoBox("Change", med_size, icon = icon("calculator"))
    
  })
  output$changelistBox <- renderInfoBox({
    list_change_box()
  })
  
  
  # server side to render map image ####
  output$map_img <- renderImage({
    list(src = "./images/map5.gif",
         width = "260px",
         height = "500px",
         style = "display: block; margin: auto;")
    
  }, deleteFile = F)
}

# shinyApp(ui, server)
shinyApp(ui = ui, server = server)
