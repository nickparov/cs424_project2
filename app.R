library(shinydashboard)
library(data.table)
library(lubridate)
library(ggplot2)
library(leaflet)
library(scales)
library(shiny)
library(dplyr)
library(tidyr)
library(jpeg)
library(grid)
library(DT)

# make uic halsted data
uic_entries <- read.table(file = 'CTA.tsv', sep = '\t', header = TRUE)
uic_entries <- uic_entries[uic_entries$station_id == 40350,]
uic_entries <- separate(uic_entries, date, c("Month", "Day", "Year"))

# make ohare data
ohare_entries <- read.table(file = 'CTA.tsv', sep = '\t', header = TRUE)
ohare_entries <- ohare_entries[ohare_entries$station_id == 40890,]
ohare_entries <- separate(ohare_entries, date, c("Month", "Day", "Year"))

# make indiana data
indiana_entries <- read.table(file = 'CTA.tsv', sep = '\t', header = TRUE)
indiana_entries <- indiana_entries[indiana_entries$station_id == 40300,]
indiana_entries <- separate(indiana_entries, date, c("Month", "Day", "Year"))

# make date numeric
uic_entries$Year <- as.numeric(as.character(uic_entries$Year))
uic_entries$Month <- as.numeric(as.character(uic_entries$Month))
uic_entries$Day <- as.numeric(as.character(uic_entries$Day))

uic_entries$date <- ymd(paste(uic_entries$Year, uic_entries$Month, uic_entries$Day, sep="-"))
uic_entries$each_day <- format(uic_entries$date, '%b %d')
uic_entries$weekday <- weekdays(as.Date(uic_entries$date))
uic_entries$months <- format(uic_entries$date, '%B')

yearly_sums <- aggregate(x = uic_entries$rides, by = list(uic_entries$Year), FUN = sum) 

# make date numeric
ohare_entries$Year <- as.numeric(as.character(ohare_entries$Year))
ohare_entries$Month <- as.numeric(as.character(ohare_entries$Month))
ohare_entries$Day <- as.numeric(as.character(ohare_entries$Day))

ohare_entries$date <- ymd(paste(ohare_entries$Year, ohare_entries$Month, ohare_entries$Day, sep="-"))
ohare_entries$each_day <- format(ohare_entries$date, '%b %d')
ohare_entries$weekday <- weekdays(as.Date(ohare_entries$date))
ohare_entries$months <- format(ohare_entries$date, '%B')

yearly_sums_o <- aggregate(x = ohare_entries$rides, by = list(ohare_entries$Year), FUN = sum) 

# make date numeric
indiana_entries$Year <- as.numeric(as.character(indiana_entries$Year))
indiana_entries$Month <- as.numeric(as.character(indiana_entries$Month))
indiana_entries$Day <- as.numeric(as.character(indiana_entries$Day))

indiana_entries$date <- ymd(paste(indiana_entries$Year, indiana_entries$Month, indiana_entries$Day, sep="-"))
indiana_entries$each_day <- format(indiana_entries$date, '%b %d')
indiana_entries$weekday <- weekdays(as.Date(indiana_entries$date))
indiana_entries$months <- format(indiana_entries$date, '%B')

yearly_sums_i <- aggregate(x = indiana_entries$rides, by = list(indiana_entries$Year), FUN = sum) 

# create menu items
years<-c(2001:2021)


# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title = "CTA data"),
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
    sidebarMenu(
      menuItem("Graphs", tabName = "Graphs", icon = NULL),
      menuItem("Tables", tabName = "Tables", icon = NULL),
      menuItem("About Page", tabName = "About", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL)
    ),
    
    selectInput("Year", "Select the year to visualize UIC Halsted", years, selected = 2021),
    selectInput("Year_o", "Select the year to visualize O'Hare", years, selected = 2021),
    selectInput("Year_i", "Select the year to visualize Indiana", years, selected = 2021)
    
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Graphs",
              fluidRow(
                # uic
                column(4, box(
                  width = '100%',
                  title = "UIC Halsted station load in years", 
                  solidHeader = TRUE, 
                  status = "primary",
                  plotOutput("bar1", height = 200)
                )
                ),
                #ohare
                column(4, box(
                  width = '100%',
                  title = "O'Hare station load in years", 
                  solidHeader = TRUE, 
                  status = "primary",
                  plotOutput("bar1_o", height = 200)
                )
                ),
                column(4, box(
                  width = '100%',
                  title = "Indiana station load in years", 
                  solidHeader = TRUE, 
                  status = "primary",
                  plotOutput("bar1_i", height = 200)
                )
                )
                
              ),
              
              fluidRow(
                # uic
                column(4, box(
                  width = '100%',
                  title = "UIC Halsted station load in each month", 
                  solidHeader = TRUE, 
                  status = "primary",
                  plotOutput("bar3", height = 200)
                )
                ),
                # ohare
                column(4, box(
                  width = '100%',
                  title = "O'Hare station load in each month", 
                  solidHeader = TRUE, 
                  status = "primary",
                  plotOutput("bar3_o", height = 200)
                )
                ),
                column(4, box(
                  width = '100%',
                  title = "Indiana station load in each month", 
                  solidHeader = TRUE, 
                  status = "primary",
                  plotOutput("bar3_i", height = 200)
                )
                )
                
              ),
              
              fluidRow(
                column(4, box(
                  width = '100%',
                  title = "UIC Halsted station load in each day of the week", 
                  solidHeader = TRUE, 
                  status = "primary",
                  plotOutput("bar4", height = 200)
                )
                ),
                column(4, box(
                  width = '100%',
                  title = "O'Hare station load in each day of the week", 
                  solidHeader = TRUE, 
                  status = "primary",
                  plotOutput("bar4_o", height = 200)
                )
                ),
                column(4, box(
                  width = '100%',
                  title = "Indiana station load in each day of the week", 
                  solidHeader = TRUE, 
                  status = "primary",
                  plotOutput("bar4_i", height = 200)
                )
                )
              ),
              
              fluidRow(
                column(4, box(
                  width = '100%',
                  title = "UIC Halsted station load each day", 
                  solidHeader = TRUE, 
                  status = "primary",
                  plotOutput("bar2", height = 1500)
                )
                ),
                column(4, box(
                  width = '100%',
                  title = "O'Hare station load each day", 
                  solidHeader = TRUE, 
                  status = "primary",
                  plotOutput("bar2_o", height = 1500)
                )
                ),
                column(4, box(
                  width = '100%',
                  title = "Indiana station load in each day of the week", 
                  solidHeader = TRUE, 
                  status = "primary",
                  plotOutput("bar2_i", height = 1500)
                )
                )
              )
              
      ),
      
      tabItem( tabName = "Tables",
               fluidRow(
                 column(4, box(
                   width = '100%',
                   title = "UIC Halsted station load in years", 
                   solidHeader = TRUE, 
                   status = "primary",
                   dataTableOutput("tab1", height = 200)
                 )
                 ),
                 column(4, box(
                   width = '100%',
                   title = "O'Hare station load in years", 
                   solidHeader = TRUE, 
                   status = "primary",
                   dataTableOutput("tab1_o", height = 200)
                 )
                 ),
                 column(4, box(
                   width = '100%',
                   title = "Indiana station load in years", 
                   solidHeader = TRUE, 
                   status = "primary",
                   dataTableOutput("tab1_i", height = 200)
                 )
                 )
                 
               ),
               
               fluidRow(
                 column(4, box(
                   width = '100%',
                   title = "UIC Halsted station load in each month", 
                   solidHeader = TRUE, 
                   status = "primary",
                   dataTableOutput("tab2", height = 200)
                 )
                 ),
                 column(4, box(
                   width = '100%',
                   title = "O'Hare station load in each month", 
                   solidHeader = TRUE, 
                   status = "primary",
                   dataTableOutput("tab2_o", height = 200)
                 )
                 ),
                 column(4, box(
                   width = '100%',
                   title = "Indiana station load in each month", 
                   solidHeader = TRUE, 
                   status = "primary",
                   dataTableOutput("tab2_i", height = 200)
                 )
                 )
                 
               ),
               
               fluidRow(
                 column(4, box(
                   width = '100%',
                   title = "UIC Halsted station load in each day of the week", 
                   solidHeader = TRUE, 
                   status = "primary",
                   dataTableOutput("tab3", height = 200)
                 )
                 ),
                 column(4, box(
                   width = '100%',
                   title = "O'Hare station load in each day of the week", 
                   solidHeader = TRUE, 
                   status = "primary",
                   dataTableOutput("tab3_o", height = 200)
                 )
                 ),
                 column(4, box(
                   width = '100%',
                   title = "Indiana station load in each day of the week", 
                   solidHeader = TRUE, 
                   status = "primary",
                   dataTableOutput("tab3_i", height = 200)
                 )
                 )
               ),
               
               fluidRow(
                 column(4, box(
                   width = '100%',
                   title = "UIC Halsted station load each day", 
                   solidHeader = TRUE, 
                   status = "primary",
                   dataTableOutput("tab4", height = 200)
                 )
                 ),
                 column(4, box(
                   width = '100%',
                   title = "O'Hare station load each day", 
                   solidHeader = TRUE, 
                   status = "primary",
                   dataTableOutput("tab4_o", height = 200)
                 )
                 ),
                 column(4, box(
                   width = '100%',
                   title = "Indiana station load each day", 
                   solidHeader = TRUE, 
                   status = "primary",
                   dataTableOutput("tab4_i", height = 200)
                 )
                 )
               )
               
      ),
      tabItem( tabName = "About", 
               fluidRow(
                 column(8, box(
                   width = '100%',
                   title = "Where the data was taken",
                   solidHeader = TRUE, 
                   status = "primary",
                   p("The data was taken from the Chicago Data Portal at: "),
                   a("https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f"),
                   p("and was exported in the TSV for Excel version. Data contains 1.09M or rows and 5 columns. Columns include: station_id, stationname, date, daytype and number of rides")
                 )),
                 column(4, box(
                   width = '100%',
                   title = "Who wrote the app",
                   solidHeader = TRUE, 
                   status = "primary",
                   p("The creator of this app is Malika Yelyubayeva. A Computer Science student at the University of Illinois at Chicago who is graduating this May.")
                 ))
               ),
               
               fluidRow(
                 column(12, box(
                   width = '100%',
                   title = "Why this app exists",
                   solidHeader = TRUE, 
                   status = "primary",
                   p('This app exists to visually see and analyze the data about CTA stations, their load depending on the time of the year or week or even day')
                 ))
               )
      )
    )
  ) #dashboardBody end
) #dashboardPage end

# Define server logic
server <- function(input, output) {
  theme_set(theme_grey(base_size = 14)) 
  
  justOneYearReactive <- reactive({subset(uic_entries, year(uic_entries$date) == input$Year)})
  justOneYearReactive_o <- reactive({subset(ohare_entries, year(ohare_entries$date) == input$Year_o)})
  justOneYearReactive_i <- reactive({subset(indiana_entries, year(indiana_entries$date) == input$Year_i)})
  
  # show a bar chart of the rides all years in UIC Halsted
  output$bar1 <- renderPlot({
    ggplot(uic_entries, aes(x=Year, y=rides)) + 
      geom_bar(stat="identity", width=0.5, fill="steelblue") +
      labs(x="Year", y = "Rides")  +
      scale_x_continuous(breaks=years, labels = years) +
      theme_minimal() +
      theme(axis.title.y = element_text(face = "italic"))
  })
  
  # show a bar chart of the rides all years in OHARE
  output$bar1_o <- renderPlot({
    ggplot(ohare_entries, aes(x=Year, y=rides)) + 
      geom_bar(stat="identity", width=0.5, fill="steelblue") +
      labs(x="Year", y = "Rides")  +
      scale_x_continuous(breaks=years, labels = years) +
      theme_minimal() +
      theme(axis.title.y = element_text(face = "italic"))
  })
  
  # show a bar chart of the rides all years in OHARE
  output$bar1_i <- renderPlot({
    ggplot(indiana_entries, aes(x=Year, y=rides)) + 
      geom_bar(stat="identity", width=0.5, fill="steelblue") +
      labs(x="Year", y = "Rides")  +
      scale_x_continuous(breaks=years, labels = years) +
      theme_minimal() +
      theme(axis.title.y = element_text(face = "italic"))
  })
  
  # bar chart showing entries at UIC-Halsted each day (jan 1, jan 2, ... dec 31)
  output$bar2 <- renderPlot({
    justOneYear <- justOneYearReactive()
    ggplot(justOneYear, aes(x=each_day, y=rides)) + 
      geom_bar(stat="identity",width=0.5, fill="steelblue") +
      labs(x=paste("Days in", input$Year), y = "Rides") +
      coord_flip() +
      theme_minimal() +
      theme(axis.title.y = element_text(face = "italic"))
  })
  
  # bar chart showing entries at ohare each day (jan 1, jan 2, ... dec 31)
  output$bar2_o <- renderPlot({
    justOneYear <- justOneYearReactive_o()
    ggplot(justOneYear, aes(x=each_day, y=rides)) + 
      geom_bar(stat="identity",width=0.5, fill="steelblue") +
      labs(x=paste("Days in", input$Year_o), y = "Rides") +
      coord_flip() +
      theme_minimal() +
      theme(axis.title.y = element_text(face = "italic"))
  })
  
  # bar chart showing entries at ohare each day (jan 1, jan 2, ... dec 31)
  output$bar2_i <- renderPlot({
    justOneYear <- justOneYearReactive_i()
    ggplot(justOneYear, aes(x=each_day, y=rides)) + 
      geom_bar(stat="identity",width=0.5, fill="steelblue") +
      labs(x=paste("Days in", input$Year_i), y = "Rides") +
      coord_flip() +
      theme_minimal() +
      theme(axis.title.y = element_text(face = "italic"))
  })
  
  # bar chart showing total entries at UIC-Halsted for each month (jan, feb, ... dec)
  output$bar3 <- renderPlot({
    justOneYear <- justOneYearReactive()
    justOneYear$months <- factor(justOneYear$months, levels= c("December", "November", "October", "September", "August", "July", "June", 
                                                                 "May", "April", "March", "February", "January"))
    ggplot(justOneYear, aes(x=months, y=rides)) +
      geom_bar(stat="identity",width=0.5, fill="steelblue") +
      labs(x=paste("Months in", input$Year), y = "Rides")  +
      ylim(0, max(justOneYear$rides) + 1000) +
      coord_flip()+
      theme_minimal() +
      theme(axis.title.y = element_text(face = "italic"))
  })
  
  # bar chart showing total entries at ohare for each month (jan, feb, ... dec)
  output$bar3_o <- renderPlot({
    justOneYear <- justOneYearReactive_o()
    justOneYear$months <- factor(justOneYear$months, levels= c("December", "November", "October", "September", "August", "July", "June", 
                                                               "May", "April", "March", "February", "January"))
    ggplot(justOneYear, aes(x=months, y=rides)) +
      geom_bar(stat="identity",width=0.5, fill="steelblue") +
      labs(x=paste("Months in", input$Year_o), y = "Rides")  +
      ylim(0, max(justOneYear$rides) + 1000) +
      coord_flip()+
      theme_minimal() +
      theme(axis.title.y = element_text(face = "italic"))
  })
  
  # bar chart showing total entries at ohare for each month (jan, feb, ... dec)
  output$bar3_i <- renderPlot({
    justOneYear <- justOneYearReactive_i()
    justOneYear$months <- factor(justOneYear$months, levels= c("December", "November", "October", "September", "August", "July", "June", 
                                                               "May", "April", "March", "February", "January"))
    ggplot(justOneYear, aes(x=months, y=rides)) +
      geom_bar(stat="identity",width=0.5, fill="steelblue") +
      labs(x=paste("Months in", input$Year_i), y = "Rides")  +
      ylim(0, max(justOneYear$rides) + 1000) +
      coord_flip()+
      theme_minimal() +
      theme(axis.title.y = element_text(face = "italic"))
  })
  
  # bar chart showing total entries at UIC-Halsted for each day of the week (mon, tue, ... sun)
  output$bar4 <- renderPlot({
    justOneYear <- justOneYearReactive()
    justOneYear$weekday <- factor(justOneYear$weekday, levels= c("Sunday", "Saturday", 
                                          "Friday", "Thursday", "Wednesday", "Tuesday", "Monday"))
    ggplot(justOneYear, aes(x=weekday, y=rides)) +
      geom_bar(stat="identity",width=0.5, fill="steelblue") +
      labs(x=paste("Weekdays in", input$Year), y = "Rides")  +
      coord_flip()+
      theme_minimal() +
      theme(axis.title.y = element_text(face = "italic"))
  })
  
  # bar chart showing total entries at ohare for each day of the week (mon, tue, ... sun)
  output$bar4_o <- renderPlot({
    justOneYear <- justOneYearReactive_o()
    justOneYear$weekday <- factor(justOneYear$weekday, levels= c("Sunday", "Saturday", 
                                                                 "Friday", "Thursday", "Wednesday", "Tuesday", "Monday"))
    ggplot(justOneYear, aes(x=weekday, y=rides)) +
      geom_bar(stat="identity",width=0.5, fill="steelblue") +
      labs(x=paste("Weekdays in", input$Year_o), y = "Rides")  +
      coord_flip()+
      theme_minimal() +
      theme(axis.title.y = element_text(face = "italic"))
  })
  
  # bar chart showing total entries at ohare for each day of the week (mon, tue, ... sun)
  output$bar4_i <- renderPlot({
    justOneYear <- justOneYearReactive_i()
    justOneYear$weekday <- factor(justOneYear$weekday, levels= c("Sunday", "Saturday", 
                                                                 "Friday", "Thursday", "Wednesday", "Tuesday", "Monday"))
    ggplot(justOneYear, aes(x=weekday, y=rides)) +
      geom_bar(stat="identity",width=0.5, fill="steelblue") +
      labs(x=paste("Weekdays in", input$Year_i), y = "Rides")  +
      coord_flip()+
      theme_minimal() +
      theme(axis.title.y = element_text(face = "italic"))
  })
  
  
  # show a table of the rides all years in UIC Halsted
  output$tab1 <- DT::renderDataTable({
    datatable(yearly_sums,
              colnames = c('Year', 'Total Rides'), 
              options = list(pageLength = 12, autoWidth = TRUE))})
  
  # show a table of the rides all years in OHARE
  output$tab1_o <- DT::renderDataTable({
    datatable(yearly_sums_o,
              colnames = c('Year', 'Total Rides'), 
              options = list(pageLength = 12, autoWidth = TRUE))})  
  
  # show a table of the rides all years in OHARE
  output$tab1_i <- DT::renderDataTable({
    datatable(yearly_sums_i,
              colnames = c('Year', 'Total Rides'), 
              options = list(pageLength = 12, autoWidth = TRUE))})  
  
  
  # UIC Halsted station load in each month
  output$tab2 <- DT::renderDataTable({
    justOneYear <- justOneYearReactive()
    monthly_sums <- uic_entries[uic_entries$Year == justOneYear,]
    monthly_sums <- aggregate(x = monthly_sums$rides, by = list(monthly_sums$Month), FUN = sum)
    datatable(monthly_sums,
              colnames = c('Month', 'Total Rides'),
              options = list(searching = FALSE, pageLength = 12, lengthChange = FALSE, order = list(list(1, 'desc'))
  ), rownames = FALSE )})
  
  # OHARE station load in each month
  output$tab2_o <- DT::renderDataTable({
    justOneYear <- justOneYearReactive_o()
    monthly_sums <- ohare_entries[ohare_entries$Year == justOneYear,]
    monthly_sums <- aggregate(x = monthly_sums$rides, by = list(monthly_sums$Month), FUN = sum)
    datatable(monthly_sums,
              colnames = c('Month', 'Total Rides'),
              options = list(searching = FALSE, pageLength = 12, lengthChange = FALSE, order = list(list(1, 'desc'))
              ), rownames = FALSE )})
  
  # OHARE station load in each month
  output$tab2_i <- DT::renderDataTable({
    justOneYear <- justOneYearReactive_i()
    monthly_sums <- indiana_entries[indiana_entries$Year == justOneYear,]
    monthly_sums <- aggregate(x = monthly_sums$rides, by = list(monthly_sums$Month), FUN = sum)
    datatable(monthly_sums,
              colnames = c('Month', 'Total Rides'),
              options = list(searching = FALSE, pageLength = 12, lengthChange = FALSE, order = list(list(1, 'desc'))
              ), rownames = FALSE )})
  
  # UIC Halsted station load in each weekday
  output$tab3 <- DT::renderDataTable({
    justOneYear <- justOneYearReactive()
    weekly_sums <- uic_entries[uic_entries$Year == justOneYear,]
    weekly_sums <- aggregate(x = weekly_sums$rides, by = list(weekly_sums$weekday), FUN = sum)
    datatable(weekly_sums,
              colnames = c('Weekday', 'Total Rides'),
              options = list(pageLength = 7, autoWidth = TRUE))})

  # OHARE station load in each weekday
  output$tab3_o <- DT::renderDataTable({
    justOneYear <- justOneYearReactive_o()
    weekly_sums <- ohare_entries[ohare_entries$Year == justOneYear,]
    weekly_sums <- aggregate(x = weekly_sums$rides, by = list(weekly_sums$weekday), FUN = sum)
    datatable(weekly_sums,
              colnames = c('Weekday', 'Total Rides'),
              options = list(pageLength = 7, autoWidth = TRUE))})
  
  # OHARE station load in each weekday
  output$tab3_i <- DT::renderDataTable({
    justOneYear <- justOneYearReactive_i()
    weekly_sums <- indiana_entries[indiana_entries$Year == justOneYear,]
    weekly_sums <- aggregate(x = weekly_sums$rides, by = list(weekly_sums$weekday), FUN = sum)
    datatable(weekly_sums,
              colnames = c('Weekday', 'Total Rides'),
              options = list(pageLength = 7, autoWidth = TRUE))})
  

  # UIC Halsted station load in each day
  output$tab4 <- DT::renderDataTable({
    justOneYear <- justOneYearReactive()
    daily_sums <- uic_entries[uic_entries$Year == justOneYear,]
    daily_sums <- aggregate(x = daily_sums$rides, by = list(daily_sums$each_day), FUN = sum)
    datatable(daily_sums,
              colnames = c('Day', 'Total Rides'),
              options = list(pageLength = 10, autoWidth = TRUE))})
  
  # OHARE station load in each day
  output$tab4_o <- DT::renderDataTable({
    justOneYear <- justOneYearReactive_o()
    daily_sums <- ohare_entries[ohare_entries$Year == justOneYear,]
    daily_sums <- aggregate(x = daily_sums$rides, by = list(daily_sums$each_day), FUN = sum)
    datatable(daily_sums,
              colnames = c('Day', 'Total Rides'),
              options = list(pageLength = 10, autoWidth = TRUE))})
  
  # OHARE station load in each day
  output$tab4_i <- DT::renderDataTable({
    justOneYear <- justOneYearReactive_i()
    daily_sums <- indiana_entries[indiana_entries$Year == justOneYear,]
    daily_sums <- aggregate(x = daily_sums$rides, by = list(daily_sums$each_day), FUN = sum)
    datatable(daily_sums,
              colnames = c('Day', 'Total Rides'),
              options = list(pageLength = 10, autoWidth = TRUE))})
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)