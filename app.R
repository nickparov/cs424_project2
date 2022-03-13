library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
library(readr)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(rgdal)

# READ ALL FILES FROM DIRECTORY
temp = list.files(pattern="*.tsv")
allData2 <- lapply(temp, read.delim)
allData <- do.call(rbind, allData2)

# Location
locData <- read_tsv('locationData/locationData.tsv')
names(locData)[names(locData) == 'STATION_NAME'] <- "stationname"
names(locData)[names(locData) == 'Location'] <- "location"
locData <- locData[!duplicated(locData$stationname), ] 
allNedeedData <- left_join(allData, locData, by = c("stationname"))

# extra locations
extraLocData <- read_tsv('locationData/extraLocations.tsv')
# one more join 
allNedeedData <- left_join(allNedeedData, extraLocData, by = c("stationname"))
allNedeedData <- allNedeedData %>% select(-STOP_ID)
allNedeedData <- allNedeedData %>% select(-DIRECTION_ID)
allNedeedData <- allNedeedData %>% select(-STOP_NAME)
allNedeedData <- allNedeedData %>% select(-STATION_DESCRIPTIVE_NAME)
allNedeedData <- allNedeedData %>% select(-MAP_ID)
allNedeedData <- allNedeedData %>% select(-ADA)
allNedeedData <- allNedeedData %>% select(-RED)
allNedeedData <- allNedeedData %>% select(-BLUE)
allNedeedData <- allNedeedData %>% select(-G)
allNedeedData <- allNedeedData %>% select(-BRN)
allNedeedData <- allNedeedData %>% select(-P)
allNedeedData <- allNedeedData %>% select(-Pexp)
allNedeedData <- allNedeedData %>% select(-Y)
allNedeedData <- allNedeedData %>% select(-Pnk)
allNedeedData <- allNedeedData %>% select(-O)

# Merge Locations
allNedeedData$location <- paste(allNedeedData$location.y,allNedeedData$location.x)

# Remove NAs and create new 
locationsWithoutNAs <- allNedeedData %>% mutate(location = coalesce(location.x,location.y)) %>% select(, location)

# remove col names
allNedeedData <- allNedeedData %>% select(-location.x)
allNedeedData <- allNedeedData %>% select(-location.y)
allNedeedData <- allNedeedData %>% select(-location)

# rename location.location
locationsWithoutNAs <- pull(locationsWithoutNAs, location)
allNedeedData$location <- locationsWithoutNAs

# remove all ( and ) first
allNedeedData$location<-sub('.', '', as.character(allNedeedData$location))
allNedeedData$location<-substr(as.character(allNedeedData$location), 1, nchar(as.character(allNedeedData$location))-2)

# separated
separated <- data.frame(do.call("rbind", strsplit(as.character(allNedeedData$location), ",", fixed = TRUE)))
separated$X1 <- as.numeric(as.character(separated$X1))
separated$X2 <- as.numeric(as.character(separated$X2))

# join
allNedeedData$lat <- separated$X1
allNedeedData$long <- separated$X2

# CONVERT DATES
allNedeedData$newDate <- as.Date(allNedeedData$date, "%m/%d/%Y")
allNedeedData$date <- NULL

# MENU ITEMS
stationNamesFalse <- c(allNedeedData$stationname)
stationNames <- sort(unique(stationNamesFalse))
mapThemes <- c("Basic", "Topo Map", "Grey")


# UI START
ui <- dashboardPage(
  dashboardHeader(title = "CTA stations data"),
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
                   sidebarMenu(
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL)),
                  
                   dateInput("date1", label = "Select date", value = "08/23/21"),
                   # dateInput("date2", label = "Select date to compare", value = "08/23/21"),
                   
                   sidebarMenu(
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL)),
                   
                   selectInput("Station1", "Select first station", stationNames, selected = 'UIC-Halsted'),
                   selectInput("Station2", "Select second station", stationNames, selected = "O'Hare Airport"),


                   sidebarMenu(
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL)),
                   
                   selectInput("mapTheme", "Select Map Theme", mapThemes, selected = 'Basic'),
                   
                   sidebarMenu(
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("Dashboard", tabName = "Dashboard", icon = NULL),
                     menuItem("About Page", tabName = "About", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL))
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Dashboard",
              fluidRow(
                column(8,
                       fluidRow(
                         column(6, box(
                           title = "Total entries for all stations on selected day",
                           solidHeader = TRUE, status = "info", width = 12,
                           plotOutput("totalBar", height = 400)
                         )),
                         column(6, box(
                           title = "Table for total entries for all stations on selected day",
                           solidHeader = TRUE, status = "info", width = 12,
                           dataTableOutput("totalTable", height = 400)
                         ))
                       ),
                       fluidRow(
                         column(2, box(
                           title = "Table for rides per day for the first station",
                           solidHeader = TRUE, status = "primary", width = 12,
                           dataTableOutput("dayTable1", height = 400)
                         )),
                         column(4, box(
                           title = "Rides per day for the first station",
                           solidHeader = TRUE, status = "primary", width = 12,
                           plotOutput("dayBar1", height = 400)
                         )),
                         column(4, box(
                           title = "Rides per day for the second station",
                           solidHeader = TRUE, status = "primary", width = 12,
                           plotOutput("dayBar2", height = 400)
                         )),
                         column(2, box(
                           title = "Table for rides per day for the second station",
                           solidHeader = TRUE, status = "primary", width = 12,
                           dataTableOutput("dayTable2", height = 400)
                         ))
                       ),
                       fluidRow(
                         column(2, box(
                           title = "Table for rides per day of the week for the first station",
                           solidHeader = TRUE, status = "primary", width = 12,
                           dataTableOutput("weekTable1", height = 400)
                         )),
                         column(4, box(
                           title = "Rides per day of the week for the first station",
                           solidHeader = TRUE, status = "primary", width = 12,
                           plotOutput("weekBar1", height = 400)
                         )),
                         column(4, box(
                           title = "Rides per day of the week the for the second station",
                           solidHeader = TRUE, status = "primary", width = 12,
                           plotOutput("weekBar2", height = 400)
                         )),
                         column(2, box(
                           title = "Table for rides per day of the week the for the second station",
                           solidHeader = TRUE, status = "primary", width = 12,
                           dataTableOutput("weekTable2", height = 400)
                         ))
                       ),
                       fluidRow(
                         column(2, box(
                           title = "Table for rides per month for the first station",
                           solidHeader = TRUE, status = "primary", width = 12,
                           dataTableOutput("monthTable1", height = 400)
                         )),
                         column(4, box(
                           title = "Rides per month for the first station",
                           solidHeader = TRUE, status = "primary", width = 12,
                           plotOutput("monthBar1", height = 400)
                         )),
                         column(4, box(
                           title = "Rides per month for the second station",
                           solidHeader = TRUE, status = "primary", width = 12,
                           plotOutput("monthBar2", height = 400)
                         )),
                         column(2, box(
                           title = "Table for rides per month for the second station",
                           solidHeader = TRUE, status = "primary", width = 12,
                           dataTableOutput("monthTable2", height = 400)
                         ))
                       ),
                       fluidRow(
                         column(2, box(
                           title = "Table for rides per year for the first station",
                           solidHeader = TRUE, status = "primary", width = 12,
                           dataTableOutput("yearTable1", height = 400)
                         )),
                         column(4, box(
                           title = "Rides per year for the first station",
                           solidHeader = TRUE, status = "primary", width = 12,
                           plotOutput("yearBar1", height = 400)
                         )),
                         column(4, box(
                           title = "Rides per year for the second station",
                           solidHeader = TRUE, status = "primary", width = 12,
                           plotOutput("yearBar2", height = 400)
                         )),
                         column(2, box(
                           title = "Table for rides per year for the second station",
                           solidHeader = TRUE, status = "primary", width = 12,
                           dataTableOutput("yearTable2", height = 400)
                         ))
                       ),
                       
                ),
                column(4, box(
                  title = "Map with all CTA stations", 
                  solidHeader = TRUE, status = "info", width = 12,
                  leafletOutput("map", height = 2335),
                  absolutePanel(bottom = 30, left = 10)
                ))
              ) #end of fluid row
              
      ),
      tabItem(tabName = "About",
              fluidRow(
                column(8, box(
                  width = '100%',
                  title = "Where the data was taken",
                  solidHeader = TRUE, 
                  status = "info",
                  p("The data was taken from the Chicago Data Portal at: "),
                  a("https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f"),
                  p("and was exported in the TSV for Excel version. Data contains 1.09M or rows and 5 columns. Columns include: station_id, stationname, date, daytype and number of rides"),
                  p("The additional data for maps was taken from "),
                  a("https://data.cityofchicago.org/Transportation/CTA-System-Information-List-of-L-Stops/8pix-ypme"),
                  p("It has a list of 'L' stops provides location and basic service availability information for each place on the CTA system where a train stops, along with formal station names and stop descriptions."),
                  p("The data was updated on Updated October 19, 2021, andd has 300 rows and 17 columns")
                )),
                column(4, box(
                  width = '100%',
                  title = "Who wrote the app",
                  solidHeader = TRUE, 
                  status = "info",
                  p("The creator of this app is Malika Yelyubayeva and Mykyta Porivyi. We are Computer Science undergraduate students at the University of Illinois at Chicago. This app was created for the Project 2 of CS424 class 'Data visualization'")
                ))
              ),
              
              fluidRow(
                column(12, box(
                  width = '100%',
                  title = "Why this app exists",
                  solidHeader = TRUE, 
                  status = "info",
                  p('This app exists to visually see and analyze the data about CTA stations, their load depending on the time of the year or week or even day'),
                  p("In addition to that this app can be used for people who are commuting to campus/work using CTA services. This map would be useful for people who want to avoid traffic by looking at the map's specific featurer of showing the most loaded stations on a specific day"),
                  p("It is known that the patterns of using the CTA stations usually stay the same, so it is possible to predict the load of the specific station by looking at the older data")
                ))
              )
      )
    )
    
    
    
  ) #end of dashboard body
) #end of ui

# Define server logic required to draw a histogram
server <- function(input, output) {
  theme_set(theme_grey(base_size = 14))
  
  # calculate the values one time and re-use them in multiple charts to speed things up
  justOneDayReactive <- reactive({subset(allNedeedData, date(allNedeedData$newDate) == date(input$date1))})
  
  # each day in year
  justOneYearReactiveFirst <- reactive({subset(allNedeedData, year(allNedeedData$newDate) == format(as.Date(input$date1, format="%m/%d/%Y"),"%Y") & allNedeedData$stationname == input$Station1)})
  justOneYearReactiveSecond <- reactive({subset(allNedeedData, year(allNedeedData$newDate) == format(as.Date(input$date1, format="%m/%d/%Y"),"%Y") & allNedeedData$stationname == input$Station2)})
  
  # each weekday in year
  justOneWeekReactiveFirst <- reactive({subset(allNedeedData, year(allNedeedData$newDate) == format(as.Date(input$date1, format="%m/%d/%Y"),"%Y") & allNedeedData$stationname == input$Station1)})
  
  #each month in year
  justOneMonthReactiveFirst <- reactive({subset(allNedeedData, format(as.Date(allNedeedData$newDate, format="%m/%d/%Y"),"%d%Y") == format(as.Date(input$date1, format="%m/%d/%Y"),"%m%Y") & allNedeedData$stationname == input$Station1)})
  justOneMonthReactiveSecond <- reactive({subset(allNedeedData, format(as.Date(allNedeedData$newDate, format="%m/%d/%Y"),"%d%Y") == format(as.Date(input$date1, format="%m/%d/%Y"),"%m%Y") & allNedeedData$stationname == input$Station2)})
  
  #each year
  justOneStationReactiveFirst <- reactive({subset(allNedeedData, allNedeedData$stationname == input$Station1)})
  justOneStationReactiveSecond <- reactive({subset(allNedeedData, allNedeedData$stationname == input$Station2)})
  

  # -- 1 ROW-- 
  # show a bar chart and table of all station rides in a specific day and year
  output$totalBar <- renderPlot({
    totalDay <- justOneDayReactive()
    ggplot(totalDay, aes(x=stationname, y=rides)) + 
      geom_bar(stat="identity", fill="lightskyblue1") +
      labs(x="Stations", y = "Rides")+
      theme_minimal()
      # scale_fill_continuous(limits = c(0, max(allNedeedData$rides)))
  })

  output$totalTable <- DT::renderDataTable({
    v <- justOneDayReactive()
    datatable(v[c("stationname","rides")], 
              colnames = c('Station name', 'Total Rides'),
              options = list(pageLength = 8, searching = FALSE, lengthChange = FALSE, order = list(list(1, 'desc'))), 
              rownames = FALSE
    )
  })
  
  # -- 2  ROW--
  # show a bar chart and table of one station rides in days in spec year
  output$dayBar1 <- renderPlot({
    justOneYearFirst <- justOneYearReactiveFirst()
    ggplot(justOneYearFirst, aes(x=newDate, y=rides)) +
      geom_bar(stat="identity", fill="cadetblue3") +
      labs(x=paste("Days in", format(as.Date(input$date1, format="%m/%d/%Y"),"%Y")), y = "Rides")+
      theme_minimal()
  })
  
  output$dayTable1 <- DT::renderDataTable({
    v <- justOneYearReactiveFirst()
    datatable(v[c("newDate","rides")], 
              colnames = c('Day', 'Total Rides'),
              options = list(pageLength = 8, searching = FALSE, lengthChange = FALSE, order = list(list(1, 'desc'))), 
              rownames = FALSE
    )
  })

  output$dayBar2 <- renderPlot({
    justOneYearSecond <- justOneYearReactiveSecond()
    ggplot(justOneYearSecond, aes(x=newDate, y=rides)) +
      geom_bar(stat="identity", fill="darkslategray4") +
      labs(x=paste("Days in", format(as.Date(input$date1, format="%m/%d/%Y"),"%Y")), y = "Rides")+
      theme_minimal()
  })
  
  output$dayTable2 <- DT::renderDataTable({
    v <- justOneYearReactiveSecond()
    datatable(v[c("newDate","rides")], 
              colnames = c('Day', 'Total Rides'),
              options = list(pageLength = 8, searching = FALSE, lengthChange = FALSE, order = list(list(1, 'desc'))),  
              rownames = FALSE
    )
  })
  
  # -- 3  ROW--
  # show a bar chart and table of one station rides in days in spec day of the week
  output$weekBar1 <- renderPlot({
    justOneYearFirst <- justOneYearReactiveFirst()
    data <- aggregate(x = justOneYearFirst$rides, by = list(weekdays(as.Date(justOneYearFirst$newDate))), FUN = sum)
    data$Group.1 <- factor(data$Group.1, levels= c("Monday", "Tuesday", 
                                                          "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
    ggplot(data, aes(x=Group.1, y=x)) +
      geom_bar(stat="identity", fill="cadetblue3") +
      labs(x=paste("Weekdays in", format(as.Date(input$date1, format="%m/%d/%Y"),"%Y")), y = "Rides")+
      theme_minimal()
  })
  
  output$weekTable1 <- DT::renderDataTable({
    temp <- justOneYearReactiveFirst()
    data <- aggregate(x = temp$rides, by = list(weekdays(as.Date(temp$newDate))), FUN = sum)
    datatable(data[c("Group.1","x")], 
              colnames = c('Weekday', 'Total Rides'),
              options = list(pageLength = 8, searching = FALSE, lengthChange = FALSE, order = list(list(1, 'desc'))),  
              rownames = FALSE
    )
  })
  
  output$weekBar2 <- renderPlot({
    justOneYearSecond <- justOneYearReactiveSecond()
    data2 <- aggregate(x = justOneYearSecond$rides, by = list(weekdays(as.Date(justOneYearSecond$newDate))), FUN = sum)
    data2$Group.1 <- factor(data2$Group.1, levels= c("Monday", "Tuesday", 
                                                   "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
    ggplot(data2, aes(x=Group.1, y=x)) +
      geom_bar(stat="identity", fill="darkslategray4") +
      labs(x=paste("Weekdays in", format(as.Date(input$date1, format="%m/%d/%Y"),"%Y")), y = "Rides")+
      theme_minimal()
  })
  
  output$weekTable2 <- DT::renderDataTable({
    temp <- justOneYearReactiveSecond()
    data <- aggregate(x = temp$rides, by = list(weekdays(as.Date(temp$newDate))), FUN = sum)
    datatable(data[c("Group.1","x")], 
              colnames = c('Weekday', 'Total Rides'),
              options = list(pageLength = 8, searching = FALSE, lengthChange = FALSE, order = list(list(1, 'desc'))),  
              rownames = FALSE
    )
  })
  
  # -- 4  ROW--
  # show a bar chart and table of one station rides in months in spec year
  output$monthBar1 <- renderPlot({
    justOneMonthFirst <- justOneMonthReactiveFirst()

    ggplot(justOneMonthFirst, aes(x=newDate, y=rides)) +
      geom_bar(stat="identity", fill="cadetblue3") +
      labs(x=paste("Months in", format(as.Date(input$date1, format="%m/%d/%Y"),"%Y")), y = "Rides") +
      theme_minimal()
  })
  
  output$monthTable1 <- DT::renderDataTable({
    temp <- justOneMonthReactiveFirst()
    data <- aggregate(x = temp$rides, by = list(month(as.Date(temp$newDate))), FUN = sum)

    datatable(data[c("Group.1","x")], 
              colnames = c('Month', 'Total Rides'),
              options = list(pageLength = 8, searching = FALSE, lengthChange = FALSE, order = list(list(1, 'desc'))), 
              rownames = FALSE
    )
  })
  
  output$monthBar2 <- renderPlot({
    justOneMonthSecond <- justOneMonthReactiveSecond()
    ggplot(justOneMonthSecond, aes(x=newDate, y=rides)) +
      geom_bar(stat="identity", fill="darkslategray4") +
      labs(x=paste("Months in", format(as.Date(input$date1, format="%m/%d/%Y"),"%Y")), y = "Rides") +
      theme_minimal()
  })
  
  output$monthTable2 <- DT::renderDataTable({
    temp <- justOneMonthReactiveSecond()
    data <- aggregate(x = temp$rides, by = list(month(as.Date(temp$newDate))), FUN = sum)

    datatable(data[c("Group.1","x")], 
              colnames = c('Month', 'Total Rides'),
              options = list(pageLength = 8, searching = FALSE, lengthChange = FALSE, order = list(list(1, 'desc'))), 
              rownames = FALSE
    )
  })
  
  # -- 5  ROW--
  # show a bar chart and table of one station rides in years
  output$yearBar1 <- renderPlot({
    justOneStationFirst <- justOneStationReactiveFirst()
    ggplot(justOneStationFirst, aes(x=newDate, y=rides)) +
      geom_bar(stat="identity", fill="cadetblue3") +
      labs(x=paste("Rides on ", input$Station1, " in ", format(as.Date(input$date1, format="%m/%d/%Y"),"%Y")), y = "Rides") +
      theme_minimal()
  })
  
  output$yearTable1 <- DT::renderDataTable({
    temp <- justOneStationReactiveFirst()
    data <- aggregate(x = temp$rides, by = list(year(as.Date(temp$newDate))), FUN = sum)

    datatable(data[c("Group.1","x")], 
              colnames = c('Year', 'Total Rides'),
              options = list(pageLength = 8, searching = FALSE, lengthChange = FALSE, order = list(list(1, 'desc'))), 
              rownames = FALSE
    )
  })
  
  output$yearBar2 <- renderPlot({
    justOneStationSecond <- justOneStationReactiveSecond()
    ggplot(justOneStationSecond, aes(x=newDate, y=rides)) +
      geom_bar(stat="identity", fill="darkslategray4") +
      labs(x=paste("Rides on ", input$Station2, " in ", format(as.Date(input$date1, format="%m/%d/%Y"),"%Y")), y = "Rides") +
      theme_minimal()
  })
  
  output$yearTable2 <- DT::renderDataTable({
    temp <- justOneStationReactiveSecond()
    data <- aggregate(x = temp$rides, by = list(year(as.Date(temp$newDate))), FUN = sum)
    datatable(data[c("Group.1","x")], 
              colnames = c('Year', 'Total Rides'),
              options = list(pageLength = 8, searching = FALSE, lengthChange = FALSE, order = list(list(1, 'desc'))), 
              rownames = FALSE
    )
  })
  
  # v <- justOneDayReactive()
  
  
  output$map <- renderLeaflet({
    df.20 <-justOneDayReactive()
    
    getColor <- function(allNedeedData) {
      sapply(allNedeedData$rides, function(rides) {
        if(rides <= 1000) {
          "green"
        } else if(rides <= 2500 ) {
          "orange"
        } else {
          "red"
        } })
    }
    
    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'white',
      library = 'ion',
      markerColor = getColor(df.20)
    )
    
    pal <- colorNumeric(c("#70AC25", "#F2952F", "#D23D29"), domain = allNedeedData$rides)

    if (input$mapTheme == 'Basic') {
      leaflet(df.20) %>% addTiles() %>%
        addAwesomeMarkers(~long, ~lat, icon=icons, label=~as.character(stationname), popup = ~as.character(stationname)) %>%
        setView(lng = -87.64288412109156, lat = 41.88637870008067 , zoom = 12.5) %>%
        addLegend("topright", pal = pal, values = ~rides,
                  title = "Station load",
                  opacity = 1
        )
      
    } else if (input$mapTheme == 'Topo Map') {
      leaflet(df.20) %>% addTiles() %>%
        addProviderTiles("Esri.WorldTopoMap")  %>%
        addAwesomeMarkers(~long, ~lat, icon=icons, label=~as.character(stationname)) %>%
        setView(lng = -87.64288412109156, lat = 41.88637870008067 , zoom = 12.5) %>%
        addLegend("topright", pal = pal, values = ~rides,
                  title = "Station load",
                  opacity = 1
        )
    } else if (input$mapTheme == 'Grey') {
      leaflet(df.20) %>% addTiles() %>%
        addAwesomeMarkers(~long, ~lat, icon=icons, label=~as.character(stationname)) %>%
        setView(lng = -87.64288412109156, lat = 41.88637870008067 , zoom = 12.5) %>%
        addLegend("topright", pal = pal, values = ~rides,
                  title = "Station load",
                  opacity = 1
        ) %>%
        addProviderTiles("Esri.WorldGrayCanvas")
    }
  })
  
  observeEvent(input$map_marker_click, { 
    p <- input$map_marker_click
    print(p)
    # why p doesnt have an id?????
    
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
