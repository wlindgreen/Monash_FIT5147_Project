# Data Visualisation Project
# FIT5147 Data Exploration and Visualisation Semester 2, 2022 - Monash University
# William Lindgreen - Student ID: 33263426
# 31 October 2022

# 1. Import libraries, set wd and import data ####

# Import relevant libraries
library("rstudioapi")
library(shiny)
library(shinythemes)
library(leaflet)
library(ggplot2)
library(dplyr)
library(rgdal)
library(RColorBrewer)
library(htmltools)
library(htmlwidgets)
library(plotly)
library(lubridate)

# Set wd
setwd(dirname(getActiveDocumentContext()$path))

# Import data files 
# Hour of day - weekend/weekday
gb_hod <- read.csv("Data_DVP/gb_hod.csv", sep = ",", header = TRUE)

# Hour bucket of the week
gb_hourB_wk <- read.csv("Data_DVP/gb_hourB_wk.csv", sep = ",", header = TRUE)

# Monthly changes between the 5 zones
month_5zone_dev <- read.csv("Data_DVP/data_month_5zone_dev.csv", sep = ",", header = TRUE)

# Monthly changes between the 5 zones and all other zones
month_5zone_map <- read.csv("Data_DVP/data_month_5zone_map.csv", sep = ",", header = TRUE)

# Import Melbourne zones as a SHAPEFILE
mel_shp <- readOGR("Data_DVP/melbourne_tz/melbourne_tz.shp", layer = "melbourne_tz", GDAL1_integer64_policy = TRUE)


# 2. UPDATE DATA FOR VISUALISATIONS ####

# New column with zone name in 'month_5zone_map' and 'month_5zone_dev'
# St Kilda = 1017, Airport = 362 , Burnley Tunnel = 962, Marvel Stadium = 960, 927 = Carlton Gardens
month_5zone_map$sourceName <- ifelse(month_5zone_map$sourceid == 1017, "St Kilda", 
                                     ifelse(month_5zone_map$sourceid == 362, "Melbourne Airport",
                                            ifelse(month_5zone_map$sourceid == 962, "Burnley Tunnel",
                                                   ifelse(month_5zone_map$sourceid == 960, "Marvel Stadium", "Carlton Gardens"))))

month_5zone_dev$sourceName <- ifelse(month_5zone_dev$sourceid == 1017, "St Kilda", 
                                     ifelse(month_5zone_dev$sourceid == 362, "Melbourne Airport",
                                            ifelse(month_5zone_dev$sourceid == 962, "Burnley Tunnel",
                                                   ifelse(month_5zone_dev$sourceid == 960, "Marvel Stadium", "Carlton Gardens"))))

month_5zone_dev$dstName <- ifelse(month_5zone_dev$dstid == 1017, "St Kilda", 
                                     ifelse(month_5zone_dev$dstid == 362, "Melbourne Airport",
                                            ifelse(month_5zone_dev$dstid == 962, "Burnley Tunnel",
                                                   ifelse(month_5zone_dev$dstid == 960, "Marvel Stadium", "Carlton Gardens"))))
# Change formatting
month_5zone_dev$YM <- as.Date(month_5zone_dev$YM, format = "%d/%m/%Y") 
month_5zone_dev$season <- factor(month_5zone_dev$season)

# New column with weekend/weekday in 'gb_hourB_wk'
gb_hourB_wk$weekday_weekend <- ifelse(gb_hourB_wk$dow < 5, "Weekday", "Weekend")



# 3. SHINY APP ####

# 3.1 Define UI for the shiny app ----
ui <- navbarPage(theme = shinytheme("simplex"),
  "UBER MOVEMENT",
  
  # Define first tab for the narrative visualization
  tabPanel(
    "Project",
    # Source: https://unleash-shiny.rinterface.com/beautify-css.html
    (tags$style(
      HTML(
        ".navbar-nav > li:first-child > a {
            font-size: 16px;
            font-weight: bold;}
            .navbar-nav > li:first-child > a::after {content: ' 🚗'}"
      )
    )),
    
    # Title of page
    titlePanel(
      HTML(
        "<center>Uber movements to understand traffic patterns in Melbourne</center>"
      )
    ),
    
    # Brief description of the context of the data and project
    br(),
    fixedRow(column(12, align = "left",
    ("Uber is a flexible and easy way of getting around in cities in Australia, in particular the city of Melbourne.
      There is no doubt about Uber's presence in the transport system in recent years. Uber has become an integrated part of many travellers’ way of getting from A to B in a busy everyday life
    – maybe it is also one of your preferred transport modes when the car needs a break."),
    p("This site aims to give you a broader understanding of traffic patterns in Melbourne based on  Uber movements.
    Hopefully, you will get answers to the following questions:"),
    HTML("<ul><li>During which time periods can you expect the highest travel time in an Uber?
         </li><li>How does the Uber travel time differ between weekdays and weekends?
         </li><li>How has the Uber travel time changed from different areas in Melbourne to other areas in the city?
         </li><li>How has the Uber travel time between different areas in Melbourne developed over time?</li></ul>"),
    div(HTML("<em>So, lean back and enjoy an intriguing journey in the Uber Movement universe and play along with the different interaction tools.</em>"), style = "color: #2c7bb6")
    )),
    br(),
    tags$figure(
      align = "center",
      tags$img(
        src = "UberCar.png",
        width = 600,
        alt = "Uber car"
      ),
      tags$figcaption("Source: ", tags$a(href="https://www.ridester.com/driving-for-uber/", "Ridester"))
    )
    ,
    tags$hr(style="border-color: grey, border-top: 1px solid #000000;"),
    
    # HOUR OF THE DAY VISUALISATION
    fixedRow(column(12, align="left", h3("Travel time changes during a day and a week"))),
    fixedRow(column(12, align = "left",
    p("The illustration below shows the average travel time during the day on ", span("weekdays", style = "color:#aa0000")," and ", span("weekends", style = "color:#2c7bb6"), "respectively.
    The peak hour on weekdays stands out, including the morning peak hour (07-08) and the afternoon peak hour (15-18).
    The higher travel time can be explained by more traffic and congestion on the roads, which also influence a trip in an Uber car.
    The travel time also differs between", span("weekdays", style = "color:#aa0000")," and ", span("weekends", style = "color:#2c7bb6")," during the day but equalise in the evening hours.
    The travel time in the night hours is actually higher at the weekends than on weekdays.
    This could be explained by the fact that the demand for a Uber is higher at the weekend after a night out, which increases the number of Uber trips
      - especially for long trips with longer travel times.")
    )),
    fixedRow(column(12, align="center", h4("Average travel time between all zones in Melbourne during a day"))),
    fixedRow(column(12, align="center", plotlyOutput("hodPlot", width = "80%"))),
    br(),
    
    # HOUR BUCKET OF THE WEEK VISUALISATION
    fixedRow(column(12, align = "left",
    p("Showing the daily fluctuations in travel time give indications of when you, in general, can expect the longest journey with Uber in Melbourne on ",
    span("weekdays", style = "color:#aa0000")," and ", span("weekends", style = "color:#2c7bb6"), 
    ". What the illustration above does not reveal, is whether there are specific days and times during the week, which stand out.
    The illustrations below show the average travel time divided by predefined hour buckets for each day of the week.
    Pay special attention to the Friday peak hour between 16 and 19, which is the hour bucket of the week where the average travel time is the highest.
    Think about why the travel time on Fridays is higher than on the other weekdays. Could an explanation be that more people are driving to and from the city with Uber as there are going out for dinner or drinks?
    In combination with car owners driving home from work, this could explain the longest travel time of the week.")
    )),
    fixedRow(column(12, align="center", h4("Average travel time between all zones in Melbourne during the week"))),
    fixedRow(column(12, align="center", plotlyOutput("hourBPlot", width = "80%"))),
    
    tags$hr(style="border-color: grey, border-top: 1px solid #000000;"),
    
    # TRAVEL TIME FROM ZONES VISUALISATION
    fixedRow(column(12, align="left", h3("Spatial and temporal changes in travel time in Melbourne"))),
   
     # User input drop down menu for zones and radio buttons for year
    fixedRow(column(12, align = "left",
    p("Hopefully, you have gathered a greater comprehension of how the travel time for Uber trips varies during a day and week, but how has the travel time changed over the years between different areas?
    The map below gives you the potential to examine the temporal differences in travel time between five different zones and the rest of Melbourne.
    You can also choose which year you want to compare. As it appears from the map there are many grey zones without any values.
    The Uber Movement data consist of more than 2,300 zones in Melbourne. The data is not complete and there are zones without Uber trips, in particular the outer areas of Melbourne and the first years 2015 and 2016."),
    p("Try to play with the different zones and years to see if there are interesting patterns. 
    Hint: Take a look at the travel time from Melbourne Airport between 2017 and 2019 and observe the extensive decrease in travel time to most of the areas south of the Airport."),
    )),
    
    fixedRow(column(12, align = 'center', selectInput(
      inputId = "start_zone",
      label = "Start zone",
      choices = list("St Kilda", "Melbourne Airport", "Carlton Gardens", "Burnley Tunnel", "Marvel Stadium")
      ))
    ),
    
    fixedRow(column(6, align = 'center',
      radioButtons(
        inputId = "start_year",
        label = "Start year",
        list("2015", "2016", "2017", "2018", "2019", "2020"),
        inline = TRUE
      )
    ),
    column(6, align = 'center',
      radioButtons(
        inputId = "end_year",
        label = "End year",
        list("2015", "2016", "2017", "2018", "2019", "2020"),
        selected = "2020",
        inline = TRUE
      )
    )),
    
    # Create map canvas on the page and title above
    fixedRow(column(12, style = "border: 4px red;", align="center", h4(textOutput("map_header")))),
    fixedRow(column(12, align="center", style = "border: 4px red;", leafletOutput("map_mel", width = "80%", height = '800px'))),
    br(),
    
    # TRAVEL TIME DEVELOPMENT TO AND FROM ZONES VISUALISATION
    fixedRow(column(12, align = "left", 
    p("It can be difficult to infer the direct travel time changes over time between areas on the map above.
    The visualisation below gives you the chance to explore how the travel time has changed between two zones in Melbourne.
    The author has limited the number of zones you can choose due to data limitation, but this should not deteriorate your user experience, quite the reverse."),
    p("Note that the visualisation lack data for some of the months, including observations from October to December 2018.")
    )),
    
    # User input drop down zones for from and to zones
    fixedRow(column(6, align = 'center', selectInput(
      inputId = "start_zone_dev",
      label = "Start zone",
      choices = list(
        "St Kilda",
        "Melbourne Airport",
        "Carlton Gardens",
        "Burnley Tunnel",
        "Marvel Stadium"
      ))),
      column(6, align = 'center', selectInput(
      inputId = "end_zone_dev",
      label = "End zone",
      choices = list(
        "St Kilda",
        "Melbourne Airport",
        "Carlton Gardens",
        "Burnley Tunnel",
        "Marvel Stadium"
      )
    ))),
    
    # Create line chart development on the page and title above
    fixedRow(column(12, align="center", h4(textOutput("chart_header")))), 
    fixedRow(column(12, align = "center", plotlyOutput("dev_Plot", width = "80%"))),
    fixedRow(column(12, align = "left",
    br(),
    p("It can be derived from the illustration above that there are some limitations in the data, as some months stand out from the rest, which can be difficult to explain.
    Though, it gives you a clear indication of how Uber travel time changes during seasons and over the years.
    Take a closer look at the decrease in March 2020: The all-encompassing pandemic also influenced Uber trips.")
    )),
    br(),
    fixedRow(column(12, align = "left", "The page aimed to give your insight into travel time patterns in Melbourne based on Uber Movement data.
    If you have got wiser and can answer the four questions at the top, the mission has been successful.
                    Enter the About tab if you want to find the data and explore the data yourself – maybe based with the basis of another city in the world?"
                    )),
    br()
  ),
  
  # Define the second tab 'About'
  tabPanel(
    "About",
    titlePanel(HTML(
      "<center>Uber movements to understand traffic patterns in Melbourne</center>"
    )),
    br(),
    # About section with URL'S
    tags$div(
      "The project is based on data from Uber Movement. Origin-destination data of Uber movements between zones in Melbourne is the primary data source.
      Different tabular and spatial data sets have been downloaded and loaded into the project file from the following page ",
      tags$a(href="https://movement.uber.com/explore/melbourne/travel-times/query?si=962&ti=&ag=tz&dt[tpb]=ALL_DAY&dt[wd;]=1,2,3,4,5,6,7&dt[dr][sd]=2020-01-01&dt[dr][ed]=2020-03-31&cd=&sa;=&sdn=&lang=en-AU",
             "Melbourne - Uber Movement.")),
    tags$div(
      "If you want to know more about the data sources refer to the following page: ",
      tags$a(href="https://movement.uber.com/?lang=en-AU", "Uber Movement"),
      
    br(),
    tags$figure(
      align = "center",
      tags$img(
        src = "UberMovement.png",
        width = 600,
        alt = "Uber Movement"
      ),
      tags$figcaption("Source: ", tags$a(href="https://movement.uber.com/?lang=en-AU", "Uber Movement"))
    ))
  )
)


# 3.2 Define the server for the shiny app ----
server <- function(input, output) {
  
  # A. Visualization of Hour of of the day
  # Output ploT: HOUR OF THE DAY
  output$hodPlot <- renderPlotly({
    gb_hod %>%
      plot_ly(
        split =  ~ cat,
        type = "scatter",
        color = ~ cat,
        colors = c("#aa0000", "#2c7bb6"),
        mode = "lines+markers",
        hovertemplate = paste(
          '<b>Hour</b>: %{x}',
          '<br><b>Avg. travel time</b>: %{y:.1f} min.',
          '<extra></extra>'
        )
      ) %>%
      add_trace(
        x = ~ hod,
        y =  ~ avg_TT,
        type = "scatter",
        mode = "lines",
        showlegend = T,
        line = list(width = 2)
      ) %>%
      add_trace(
        x = ~ hod,
        y =  ~ avg_TT,
        type = "scatter",
        mode = "markers",
        showlegend = F,
        marker = list(
          size = 10,
          opacity = 0.9,
          line  = list(color = "black", width = 1)
        )
      ) %>%
      layout(
        title = "",
        xaxis = list(
          title = "Hour of Day",
          tickvals = 0:23,
          ticktext = paste0(00:23, "-", 1:24),
          tickangle = 45,
          showgrid = FALSE
        ),
        yaxis = list(title = "Avg. travel time (min.)")
      )
  })
  
  
  # B. Visualization of Hour Buckets of the Week
  output$hourBPlot <- renderPlotly({
    
    # Reorder weekly hour buckets
    gb_hourB_wk$dow_HB <-
      factor(
        gb_hourB_wk$dow_HB,
        levels = c("Mon 0 - 7", "Mon 7 - 10", "Mon 10 - 16", "Mon 16 - 19", "Mon 19 - 0", 
                   "Tue 0 - 7", "Tue 7 - 10", "Tue 10 - 16", "Tue 16 - 19", "Tue 19 - 0", 
                   "Wed 0 - 7", "Wed 7 - 10", "Wed 10 - 16", "Wed 16 - 19", "Wed 19 - 0",  
                   "Thu 0 - 7", "Thu 7 - 10", "Thu 10 - 16", "Thu 16 - 19", "Thu 19 - 0",
                   "Fri 0 - 7", "Fri 7 - 10", "Fri 10 - 16", "Fri 16 - 19", "Fri 19 - 0", 
                   "Sat 0 - 7", "Sat 7 - 10", "Sat 10 - 16", "Sat 16 - 19", "Sat 19 - 0",
                   "Sun 0 - 7","Sun 7 - 10", "Sun 10 - 16", "Sun 16 - 19", "Sun 19 - 0"))
    gb_hourB_wk <- gb_hourB_wk[with(gb_hourB_wk, order(dow_HB)), ]
    
    # Output plot: HOUR BUCKET OF THE WEEK
    # Source: https://plotly.com/r/bar-charts/
    gb_hourB_wk %>%
      plot_ly(
        type = "bar",
        x = ~ dow_HB,
        y =  ~ avg_TT,
        color = ~ weekday_weekend,
        colors = c("#aa0000", "#2c7bb6"),
        hovertemplate = paste(
          '<b>Hour bucket</b>: %{x}',
          '<br><b>Avg. travel time</b>: %{y:.1f} min.',
          '<extra></extra>'
        )
      ) %>%
      layout(
        title = "",
        xaxis = list(title = "Hour Bucket of the Week", tickangle = 45),
        yaxis = list(title = "Avg. travel time (min.)")
      )
  })
  
  # C. Visualization of map
  # Output user chosen zones and years as text
  output$map_header <-
    renderText({
      paste(
        "Travel time difference between ",
        input$start_year,
        " and ",
        input$end_year,
        " from ",
        input$start_zone
      )
    })
  
  # Output map of Melbourne with travel time changes
  output$map_mel <- renderLeaflet({
    
    # Filter data based on user selection
    f_data <- month_5zone_map[month_5zone_map$sourceName == input$start_zone,]
    f_start <- f_data[f_data$year == input$start_year,]
    f_end <- f_data[f_data$year == input$end_year,]
    f_combi <- merge(f_start, f_end, by = c("sourceid", "dstid", "sourceName"), all = TRUE)
    
    # Calculation of travel time differences based on user selection
    f_combi$abs_diff <- f_combi$avg_travel_time.y - f_combi$avg_travel_time.x
    f_combi$pct_diff <- (f_combi$avg_travel_time.y - f_combi$avg_travel_time.x) / f_combi$avg_travel_time.x * 100
    
    # Color zones based on travel time difference in pct.
    # Source: https://rstudio.github.io/leaflet/choropleths.html
    tt_pct_diff <- f_combi$pct_diff[match(mel_shp$MOVEMENT_I, f_combi$dstid)]
    tt_abs_diff <- f_combi$abs_diff[match(mel_shp$MOVEMENT_I, f_combi$dstid)] # Create for tooltips
    cpal <- colorNumeric("RdBu", tt_pct_diff, reverse = TRUE)
    
    # Prepare the text for tooltips on the map
    # Source: https://r-graph-gallery.com/183-choropleth-map-with-leaflet.html
    map_tooltip <- paste(
      "Zone name: ",
      mel_shp$DISPLAY_NA,
      "<br/>",
      "Avg. % change: ",
      round(tt_pct_diff, 1),
      "<br/>",
      "Avg. change in min.: ",
      round(tt_abs_diff, 2),
      "<br/>",
      sep = ""
    ) %>% lapply(htmltools::HTML)
    
    # CEATE MAP OF TRAVEL TIME
    leaflet(data = mel_shp) %>%
      # Esri Grey Background map
      addProviderTiles("Esri.WorldGrayCanvas") %>%
      # Set zoom
      setView(lat = -37.84, lng = 144.94 , zoom = 9) %>%
      
      # Add polygons and features
      addPolygons(
        stroke = FALSE,
        smoothFactor = 0.2,
        fillOpacity = 0.9,
        weight = 2,
        color = ~ cpal(tt_pct_diff),
        
        # Add interactions when hovering over zones
        # Highlighting polygons
        highlightOptions = highlightOptions(
          weight = 10,
          color = "#666",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        # Add labels in tooltip
        label = map_tooltip,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      # Add Legend to map
      addLegend(
        pal = cpal,
        values = ~ tt_pct_diff,
        opacity = 0.7,
        title = "Avg. Travel Time <br>[pct.]",
        position = "bottomright"
      )
  })
  
  # D. Visualization of LINE CHART DEVELOPMENT
  output$chart_header <-
    renderText({
      paste(
        "Development of the Travel Time from ",
        input$start_zone_dev,
        " to ",
        input$end_zone_dev
      )
    })
  
  output$dev_Plot <- renderPlotly({
    
    # Filter data based on user selection
    t_data <- month_5zone_dev[month_5zone_dev$sourceName == input$start_zone_dev & month_5zone_dev$dstName == input$end_zone_dev, ]
    
    # Coloring of markers by season
    seasonCol = c("Summer" = "#FFD700", "Winter"="#42687C", "Spring"="orange","Autumn" = "#603C14")
    
    # CEATE CHART OF TRAVEL TIME DEVELOPMENT
    plot_ly(data = t_data, type = "scatter", mode = "lines", customdata = ~season,
            hovertemplate = paste('<b>Month & Year </b>: %{x}',
                                  '<br><b>Season</b>: %{customdata}',
                                  '<br><b>Avg. travel time</b>: %{y:.1f} min.',
                                  '<extra></extra>')) %>%
      add_trace(x = ~YM, y=~mean_travel_time, type = "scatter", mode = "lines", showlegend = F, line = list(color = 'black', width = 2)) %>%
      add_trace(x = ~YM, y=~mean_travel_time, type = "scatter", mode = "markers",
                color = ~season, marker = list(size = 10, opacity = 0.9, line  = list(color = "black", width = 1))) %>%
      layout(legend = list(title = list(text='<b> Season </b>')), 
             xaxis = list(title = "Month and year", dtick = "M1", tickformat="%b %Y", showgrid = FALSE),
             yaxis = list(title = "Avg. travel time (min.)")
      )
    
  })
}

# 3.3 Execute shiny app ----
shinyApp(ui = ui, server = server)

