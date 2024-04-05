library(shiny)
library(tidyverse)
library(plotKML)
library(ggmap)
library(shinydashboard)
library(lubridate)
library(ggthemes)
library(rsconnect)


# Define UI for data upload app ----

ui <- dashboardPage(
  dashboardHeader(title = "Visualisation of Strava Data",
                  titleWidth = 250), 
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("Maps", tabName = "dashboard", icon = icon("map")),
      menuItem("Stats Analysis", tabName = "widgets", icon = icon("chart-area"))),
      p(
        class = "text",
        paste("This is a Dashboard aimed at visualising a user's Strava data. To download your own strava data, please go to the Strava website. From there, go 'Settings' > 'My Account' > 'Download or Delete Your Account' (It wont delete, don't worry!) > 'Request your archive'. It will be emailled to you. To access the heatmap, you will need to go 'Activities' folder in the downloaded file, and select the files ending in '.gpx'. E.g. '2806430305.gpx'. To analyse your metadata, please click on the 'Stats Analysis' tab and upload the file called 'activites.csv'."), br(), br(), paste("This is only a small project and errors may arise -- please let me know if there are any problems or if you have any suggested improvements!")
      )
  ),
  dashboardBody(
    tags$head( 
      tags$style(HTML('
      .main-sidebar {
      font-size: 20px; 
      color: "#4d3a7d;"
       }')) #change the font size to 20
    ),
    tabItems(
      tabItem(tabName = "dashboard", 
              fluidRow(
                box(title = "Heatmap of strava data: I would recommend inputting a small number of files to begin with and then refining the location and zoom. If you upload a number of files (50 +) you might find it takes a while to load the image. Please be patient!",
                  plotOutput("contents", height = 560, width = NULL), height = 600, width = NULL),
          
          # Input: Select a file ----
                box(title = "Input '.gpx' Files Here",
                      fileInput("file1", "Choose .gpx Files",
                      multiple = TRUE,
                      accept = c(".gpx"))),
                box(sliderInput('zoom', 'Zoom', 3, 18, 10)),
                box(textInput('address', 'Select Postcode Here (Addresses work too but a bit less reliably)', value = "London, UK")))
      ),
      tabItem(tabName = "widgets",
              fluidRow(
                box(plotOutput("plot_2")),
                box(plotOutput("plot_3")),
                box(title = "Input 'activities.csv' Here",
                    fileInput("file2", "Choose .csv Files",
                              multiple = F,
                              accept = c(".csv"))),
                box(plotOutput("plot_4"))
                
              )
      )
    )
  )
) 




          
          # Text input for location: Set your central postcode here
    #      textInput('address', 'Select postcode here')



server <- function(input, output) {
  
  output$contents <- renderPlot({
    req(input$file1)
    
    convertToCommonFormat <- function(df=NA, x=NA, source, file, idx=NA) {
      if (is.na(x)) x <- df[[source]]
      if (is.null(x)) return(
        tibble(
          FileName=character(), 
          Source=character(), 
          Index=NA_integer_
        )
      )
      if (is.data.frame(x)) {
        # Using tibble rather than dataframe simply for ease of printing
        return(
          as_tibble(
            x %>% add_column(
              FileName=file, 
              Source=source, 
              Index=idx, 
              .before=1
            )
          )
        )
      } else if (is.list(x)) {
        return(
          bind_rows(
            lapply(
              1:length(x), 
              function(y) convertToCommonFormat(x=x[[y]], idx=y, file=file, source=source)
            )
          )
        )
      } else {
        print(x)
        stop(paste0("Don't know what to do with ", str(x)))
      }
    }
    
    
    processOneFile <- function(gpxFile) {
      df <- readGPX(gpxFile)
      tracks <- convertToCommonFormat(df, source="tracks", file=gpxFile)
      return(bind_rows(tracks))
    }
    
    tbl_test <-
#      map(input$file1$datapath, ~processOneFile(.))
    lapply(input$file1$datapath, processOneFile)
    
    tbl_test_joined <- do.call(rbind, tbl_test) %>% 
      as_data_frame()
    
    ggmap::register_google(key = "AIzaSyCaiNvJSB9YKLBujlWR7ItyD1cvATlEWUs")

    mapImageData <- get_map(location = input$address,
                            zoom = input$zoom,
                            color = 'bw',
                            #scale = 1,
                            maptype = "terrain")
    
    ggmap(mapImageData, extent = "device") + # removes axes, etc.
      geom_point(aes(x = lon,
                     y = lat),
                 data = tbl_test_joined,
                 colour = "red3",
                 alpha = 0.025,
                 size = .1)
    
  
  })
  
    output$plot_2 <- renderPlot({
      req(input$file2)
    
      strava_data <- read_csv(input$file2$datapath)
      strava_data_clean <- strava_data %>% 
        mutate(elevation_gain = round(`Elevation Gain`, 0),
               distance = round(Distance_1, digits = -2),
               distance_km = Distance_1 / 1000,
               date = dmy_hms(`Activity Date`),
               date = floor_date(date, "1 day")) %>% 
        select(-`Activity Date`)
      
      strava_data_clean <- strava_data_clean %>% 
        mutate(cum_total = cumsum(distance_km)) %>% 
        mutate(ride = seq.int(nrow(.))) 
      
      strava_data_clean$days_passed <- as.numeric(strava_data_clean$date - min(strava_data_clean$date)) / 86400
      
      
     strava_data_elevation <- strava_data_clean %>% 
        select(`Elevation Gain`, days_passed) %>% 
        na.omit()  %>% 
        mutate(total_elev = cumsum(`Elevation Gain`)) %>% 
        mutate(ride = seq.int(nrow(.)))
      
      strava_data_clean %>% 
        ggplot(aes(ride, cum_total)) +  
        geom_line() + 
        labs(x = "Ride number", y= "Cumulative total in km",
             title = "Cumulative meters cycled") +
        theme_clean() +
        theme(plot.title = element_text(family = "IBM Plex Sans", size = 18))
    })

    
    output$plot_3 <- renderPlot({
      req(input$file2)
      
      strava_data <- read_csv(input$file2$datapath)
      strava_data_clean <- strava_data %>% 
        mutate(elevation_gain = round(`Elevation Gain`, 0),
               distance = round(Distance_1, digits = -2),
               distance_km = Distance_1 / 1000,
               date = dmy_hms(`Activity Date`),
               date = floor_date(date, "1 day")) %>% 
        select(-`Activity Date`)
      
      strava_data_clean <- strava_data_clean %>% 
        mutate(cum_total = cumsum(distance_km)) %>% 
        mutate(ride = seq.int(nrow(.))) 
      
      strava_data_clean$days_passed <- as.numeric(strava_data_clean$date - min(strava_data_clean$date)) / 86400
      
      
      strava_data_elevation <- strava_data_clean %>% 
        select(`Elevation Gain`, days_passed) %>% 
        na.omit()  %>% 
        mutate(total_elev = cumsum(`Elevation Gain`)) %>% 
        mutate(ride = seq.int(nrow(.)))
      
      strava_data_elevation %>% 
        ggplot(aes(ride, total_elev)) +
        geom_line() +
        theme_clean() +
        labs(title = "Cumulative meters climbed whilst cycling", x = "Ride number", y = "") +
        theme(plot.title = element_text(family = "IBM Plex Sans", size = 18))
              
    })
    
    output$plot_4 <- renderPlot({
      req(input$file2)
      
      strava_data <- read_csv(input$file2$datapath)
      strava_data_clean <- strava_data %>% 
        mutate(elevation_gain = round(`Elevation Gain`, 0),
               distance = round(Distance_1, digits = -2),
               distance_km = Distance_1 / 1000,
               date = dmy_hms(`Activity Date`),
               date = floor_date(date, "1 day")) %>% 
        select(-`Activity Date`)
      
      strava_data_clean <- strava_data_clean %>% 
        mutate(cum_total = cumsum(distance_km)) %>% 
        mutate(ride = seq.int(nrow(.))) 
      
      strava_data_clean$days_passed <- as.numeric(strava_data_clean$date - min(strava_data_clean$date)) / 86400
      
      
      strava_data_clean %>% 
        select(ride, `Average Speed`) %>% 
        na.omit() %>% 
        mutate(avg_speed = `Average Speed` * 3600 / 1000) %>% 
        filter(avg_speed < 40) %>% 
        filter(avg_speed > 10) %>% 
        ggplot(aes(ride, avg_speed)) +
        geom_smooth(method = "lm", se = F) +
       # geom_point() +
        geom_line() +
        theme_clean() +
        labs(title = "Average speed per ride",
             x = "Ride number",
             y = "Average speed (km/h)")+
        theme(plot.title = element_text(family = "IBM Plex Sans", size = 18))
      
  
      

    })
    
    
    
}







# Create Shiny app ----
shinyApp(ui, server)



