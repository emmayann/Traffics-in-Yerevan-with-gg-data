library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(magrittr)
library(ggplot2)
library(plotly)
library(ggthemes)
library(viridis)  
library(RColorBrewer)
library(data.table)
#library(summarytools)
library(parameters)
library(DT)
library(descriptr)
library(dashboardthemes)
library(leaflet)
library(leaflet.extras)
library(ggmap)
library( ggbeeswarm)


# importing data ----
df <- read.csv("result_sampple1.csv")
sample <- sample_n(df, 100000) 
df$time <- format(as.POSIXct(df$created_at_date),    
                      format = "%H:%M:%S")
df$created_at <- as.Date(df$created_at, format = "%Y-%m-%d")
df$season <- as.factor(df$season)
df$month <- as.factor(df$month)
df$weekday <- as.factor(df$weekday)
df_jan <- df[df$month == 'January',]
df_jul <- df[df$month == 'July',]
df_Sat <- df[df$weekday == 'Saturday',]
df_sept <- df[df$month == 'September',]
df_mon <- df[df$weekday == 'Monday',]
hours_grouped_Sat <- df_Sat %>% 
  mutate(group = case_when(
    between(hour, 6 ,10) ~ "Morning",
    between(hour, 18, 22) ~ "Evening",
    TRUE ~ NA_character_
  ))
hours_grouped_Jul <- df_jul %>% 
  mutate(group = case_when(
    between(hour, 6 ,10) ~ "Morning",
    between(hour, 18, 22) ~ "Evening",
    TRUE ~ NA_character_
  ))

hours_grouped <- df %>% 
  mutate(group = case_when(
    between(hour, 6 ,10) ~ "Morning",
    between(hour, 18, 22) ~ "Evening",
    TRUE ~ NA_character_
  ))
hours_grouped_sept <- df_sept %>% 
  mutate(group = case_when(
    between(hour, 6 ,10) ~ "Morning",
    between(hour, 18, 22) ~ "Evening",
    TRUE ~ NA_character_
  ))

hours_grouped_mon <- df_mon %>% 
  mutate(group = case_when(
    between(hour, 6 ,10) ~ "Morning",
    between(hour, 18, 22) ~ "Evening",
    TRUE ~ NA_character_
  ))
hours_grouped_jan <- df_jan %>% 
  mutate(group = case_when(
    between(hour, 6 ,10) ~ "Morning",
    between(hour, 18, 22) ~ "Evening",
    TRUE ~ NA_character_
  ))
df_cutted_scatterplot = df %>% group_by(created_at, weekday) %>% summarise(Count = n()) 
scatter_weekly <- ggplot(df_cutted_scatterplot, aes(x = weekday, y = Count, colour = weekday, text = created_at)) +
  #      facet_wrap(.~ df_cutted_scatterplot[,input$scatterplot_c], scale = "free") +
  ggbeeswarm::geom_quasirandom() +
  theme_bw() +
  scale_fill_viridis(discrete = TRUE) +
  theme(axis.text.x = element_text(angle = 45))
df_cutted_scatterplot1 = df %>% group_by(created_at, month) %>% summarise(Count = n()) 
scatter_monthly <- ggplot(df_cutted_scatterplot1, aes(x = month, y = Count, colour = month, text = created_at)) +
  #      facet_wrap(.~ df_cutted_scatterplot[,input$scatterplot_c], scale = "free") +
  ggbeeswarm::geom_quasirandom() +
  theme_bw() +
  scale_fill_viridis(discrete = TRUE) +
  theme(axis.text.x = element_text(angle = 45))

categorical_cols = c('season', 'month', 'weekday')
categorical_cols1 = c( 'weekday', 'month')

categorical_names = c('Season', 'Month', 'Weekday')
categorical_names1 = c('Weekday', 'Month')

numeric_cols = c('fare')

numeric_names = c('Fare')

name_to_cols <- function(columns) {
  output_columns = c()
  for (col in columns) {
    if (col == 'Season') {
      output_columns = c(output_columns, 'season')
    } else if (col == 'Month') {
      output_columns = c(output_columns, 'month')
    } else if (col == 'Weekday') {
      output_columns = c(output_columns, 'weekday')
    } 
  }
  return(output_columns)
}

cols_to_names = function(columns) {
  output_columns = c()
  for (col in columns) {
    if (col == '(Intercept)') {
      output_columns = c(output_columns, 'Intercept')
    } else if (col == 'seasonspring') {
      output_columns = c(output_columns, 'Season: Spring')
    } else if (col == 'seasonsummer') {
      output_columns = c(output_columns, 'Season: Summer')
    } else if (col == 'seasonwinter') {
      output_columns = c(output_columns, 'Season: Winter')
    } else if (col == 'seasonfall') {
      output_columns = c(output_columns, 'Season: Fall')
    } else if (col == 'months_sep') {
      output_columns = c(output_columns, 'Month: September')
    } else if (col == 'months_oct') {
      output_columns = c(output_columns, 'Month: October')
    } else if (col == 'months_nov') {
      output_columns = c(output_columns, 'Month: November')
    } else if (col == 'months_dec') {
      output_columns = c(output_columns, 'Month: December')
    } else if (col == 'months_jan') {
      output_columns = c(output_columns, 'Month: January')
    } else if (col == 'months_feb') {
      output_columns = c(output_columns, 'Month: February')
    } else if (col == 'months_mar') {
      output_columns = c(output_columns, 'Month: March')
    } else if (col == 'months_apr') {
      output_columns = c(output_columns, 'Month: April')
    } else if (col == 'months_may') {
      output_columns = c(output_columns, 'Month: May')
    } else if (col == 'months_jun') {
      output_columns = c(output_columns, 'Month: June')
    } else if (col == 'months_jul') {
      output_columns = c(output_columns, 'Month: July')
    } else if (col == 'months_aug') {
      output_columns = c(output_columns, 'Month: August')
    } else if (col == 'weekday_mon') {
      output_columns = c(output_columns, 'Weekday: Monday')
    } else if (col == 'weekday_tue') {
      output_columns = c(output_columns, 'Weekday: Tuesday')
    } else if (col == 'weekday_wed') {
      output_columns = c(output_columns, 'Weekday: Wednesday')
    } else if (col == 'weekday_thu') {
      output_columns = c(output_columns, 'Weekday: Thursday')
    } else if (col == 'weekday_fri') {
      output_columns = c(output_columns, 'Weekday: Friday')
    } else if (col == 'weekday_sat') {
      output_columns = c(output_columns, 'Weekday: Saturday')
    } else if (col == 'weekday_sun') {
      output_columns = c(output_columns, 'Weekday: Sunday')
    }
  }
  return(output_columns)
}
vector1 = c('(Intercept)', 'seasonspring', 'seasonsummer', 'seasonwinter', 'seasonfall', 
            'months_sep', 'months_oct', 'months_nov', 'months_dec', 'months_jan',
            'months_feb', 'months_mar', 'months_apr', 'months_may', 'months_jun',
            'months_jul', 'months_aug', 'weekday_mon', 'weekday_tue','weekday_wed',
            'weekday_thu', 'weekday_fri', 'weekday_sat', 'weekday_sun')


categorical_cols = c('season', 'month', 'weekday')


categorical_names = c('Season', 'Month', 'Weekday')


numeric_cols = c('fare', 'distance', 'hour')

numeric_names = c('Fare', "Distance", "Hour")


col_names <- c('Season', 'Month', 'Weekday', 'Fare', 'Distance', 'Hour')

header = dashboardHeader(
  title = "Traffic Flow in Yerevan"
)
sidebar = dashboardSidebar(
  sidebarMenu(
    menuItem("Explore",
             tabName = 'explore'),
    menuItem("Visualization",
             tabName = 'eda')
  )
)



body = dashboardBody(
  shinyDashboardThemes(
    theme = "grey_dark"
  ),
  tabItems(
    tabItem(tabName = 'explore',
            imageOutput('image'),
            textOutput("Explore1"),
            textOutput("Explore2"),
            textOutput("Explore3"),
            textOutput("Explore4"),
            textOutput("Explore5"),
            textOutput("Explore6"),
            textOutput("Explore7"),
            textOutput("Explore8"),
            textOutput("Explore9"),
            textOutput("Explore10"),
            textOutput("Explore11"),
            textOutput("Explore12"),
            tags$head(tags$style("#Explore1{color: white;
                                 font-size: 50px;
                                 font-style: oblique;
                                 margin-top: -360px;
                                 margin-left: 20px;
                                 }")),
            tags$head(tags$style("#Explore2{color: white;
                                 font-size: 25px;
                                 font-style: oblique;
                                 margin-top: 20px;
                                 margin-left: 20px;
                                 }")),
            tags$head(tags$style("#Explore3{color: white;
                                 font-size: 25px;
                                 font-style: oblique;
                                 margin-left: 20px;
                                 }")),
            tags$head(tags$style("#Explore4{color: white;
                                 font-size: 25px;
                                 font-style: oblique;
                                 margin-left: 20px;
                                 }")),
            tags$head(tags$style("#Explore5{color: white;
                                 font-size: 25px;
                                 font-style: oblique;
                                 margin-left: 20px;
                                 }")),
            tags$head(tags$style("#Explore6{color: white;
                                 font-size: 25px;
                                 font-style: oblique;
                                 margin-left: 20px;
                                 height: 50px;
                                 }")),
            tags$head(tags$style("#Explore7{color: white;
                                 font-size: 25px;
                                 font-style: oblique;
                                 margin-left: 20px;
                                 margin-top: -15px;
                                 height: 50px;
                                 }")),
            tags$head(tags$style("#Explore8{color: white;
                                 font-size: 25px;
                                 font-style: oblique;
                                 margin-left: 20px;
                                 margin-top: -15px;
                                 height: 50px;
                                 }")),
            tags$head(tags$style("#Explore9{color: white;
                                 font-size: 25px;
                                 font-style: oblique;
                                 margin-left: 20px;
                                 margin-top: 15px;
                                 height: 50px;
                                 
                                 }")),
            tags$head(tags$style("#Explore10{color: white;
                                 font-size: 25px;
                                 font-style: oblique;
                                 margin-left: 20px;
                                 margin-top: 20px;
                                 height: 50px;
                                 }")),
            tags$head(tags$style("#Explore11{color: white;
                                 font-size: 25px;
                                 font-style: oblique;
                                 margin-left: 20px;
                                 margin-top: -15px;
                                 height: 50px;
                                 }")),
            tags$head(tags$style("#Explore12{color: white;
                                 font-size: 25px;
                                 font-style: oblique;
                                 margin-left: 20px;
                                 margin-top: 30px;
                                 height: 50px;
                                 }")),
            tags$head(tags$style("#Explore{color: white;
                                 font-size: 20px;
                                 font-style: italic;
                                 margin-top: 250px;
                                 margin-left: 20px;
                                 }"))
    ),
    
    tabItem(tabName = 'eda',
            tabsetPanel(
              tabPanel("Barplot",
                       sidebarLayout(
                         sidebarPanel(
                           radioGroupButtons(
                             inputId = "barplot_x",
                             label = "X-axis",
                             choiceNames = categorical_names,
                             choiceValues = categorical_cols
                           ),
                           radioGroupButtons(
                             inputId = "barplot_c",
                             label = "Color",
                             choiceNames = categorical_names,
                             choiceValues = categorical_cols
                           ),
                           dateRangeInput("barplot_date",
                                          "Choose date range",
                                          start = min(df$created_at),
                                          end = max(df$created_at),
                                          min = min(df$created_at),
                                          max = max(df$created_at)
                           ),
                           prettyRadioButtons(
                             inputId = "barplot_count_type",
                             label = "Y-axis:", 
                             choices = c("Count", "Percentage"),
                             icon = icon("check"),
                             animation = "jelly"
                           )
                         ),
                         mainPanel(plotOutput('barplot'))
                       )
              ),tabPanel("Leaflet",
                         sidebarLayout(
                           sidebarPanel(
                             dateRangeInput("leaf_date",
                                            "Choose date range",
                                            start = min(df$created_at),
                                            end = max(df$created_at),
                                            min = min(df$created_at),
                                            max = max(df$created_at)
                             ),
                             
                            
                           ),
                           mainPanel(leafletOutput("my_map", width = "100%"))
                         )
              ),
              
              tabPanel("Scatterplot",
                       sidebarLayout(
                         sidebarPanel(
                           radioGroupButtons(
                             inputId = "scatterplot_x",
                             label = "X-axis",
                             choiceNames = categorical_names1,
                             choiceValues = categorical_cols1
                           ),
                           radioGroupButtons(
                             inputId = "scatterplot_c",
                             label = "Color",
                             choiceNames = categorical_names1,
                             choiceValues = categorical_cols1
                           ),
                           dateRangeInput("scatterplot_date",
                                          "Choose date range",
                                          start = min(df$created_at),
                                          end = max(df$created_at),
                                          min = min(df$created_at),
                                          max = max(df$created_at)
                           ),
                         ),
                         mainPanel(plotOutput('scatterplot'))
                       )
                       ),
              tabPanel("Histogram",
                       sidebarLayout(
                         sidebarPanel(
                           radioGroupButtons(
                             inputId = "histogram_x",
                             label = "X-axis",
                             choiceNames = numeric_names,
                             choiceValues = numeric_cols
                           ),
                           radioGroupButtons(
                             inputId = "histogram_c",
                             label = "Color",
                             choiceNames = categorical_names,
                             choiceValues = categorical_cols
                           ),
                           dateRangeInput("histogram_date",
                                          "Choose date range",
                                          start = min(df$created_at),
                                          end = max(df$created_at),
                                          min = min(df$created_at),
                                          max = max(df$created_at)
                           ),
                           sliderInput("range", "Bins:",
                                       min = 1, max = 100,
                                       value = 50)
                           
                         ),
                         mainPanel(plotOutput('histogram'))
                       )
              ),
              
                tabPanel("Maps",
                         sidebarLayout(
                           sidebarPanel(width = 3,
                                        
                                        
                                        fluidPage(h3("Filters"),
                                                  
                                                  checkboxInput(inputId = "mapall", label = "All",value = FALSE),
                                                  
                                                  checkboxInput(inputId = "mapjan", label = "January",value = FALSE),
                                                  checkboxInput(inputId = "mapjul", label = "July",value = FALSE),
                                                  checkboxInput(inputId = "mapsept", label = "September",value = FALSE),
                                                  checkboxInput(inputId = "mapmon", label = "Monday",value = FALSE),
                                                  checkboxInput(inputId = "mapsat", label = "Saturday",value = FALSE)
                                                 
                                                  
                                        )),
                           mainPanel(width = 9,plotOutput(outputId = "Maps", width = 900, height =700 )))
                       )
              )
            )
    )
    
  )




ui <- dashboardPage(header, sidebar, body)


server <- function(input, output) {
  output$Explore1 = renderText({
    "What is going on in Yerevan"
  })
  output$Explore2 <- renderText({
    
    " The aim of the project is to have general understanding about whats going on in Yerevan." 
  })
  
  output$Explore3 = renderText({
    " We used time series data to be able to have insights of in transits."
  })
  
  output$Explore4 = renderText({
    "Our Data is taken from GG" 
  })
  
  output$Explore5 = renderText({
    "Data includes Longitude, Latitude, Month, Hour, Weekday, When order was created, Canceled, Fare."
  })
  
  output$Explore6 = renderText({
    "Our main tools are GGPLOT2, dplyr, tydiverse, shiny, leaflet, maps."
  })
  
  output$Explore7 = renderText({
    "This will help people to understand during which hours it will be the most"
  })
  output$Explore8 = renderText({
    "difficult to move from one place to another, when it will be better to use Subway, order a taxi or take a bus.We wll have visualizations"
  })
  output$Explore9 = renderText({
    "categorized by the season(winter, spring, summer, autumn), by the hours, by the months and by the weekdays."
  })
  output$Explore10 = renderText({
    "We will also show the fare and the distance passed,"
  })
  output$Explore11 = renderText({
    "if the order was canceled or not, and the dates"
  })
  output$Explore12 = renderText({
    "By Narine Marutyan, Emma Hovhannisyan, Izabella Martirosyan"
  })
  output$image = renderImage(
    list(
      src = "yerevan.jpg",
      contentType = "image/jpg",
      width = "50%", height = "730px",
      align='right'
      
      
    )
  )
  
  
  output$barplot <- renderPlot({
    df_cutted_barplot = df %>%
      filter(created_at >= input$barplot_date[1], created_at <= input$barplot_date[2])
    if (input$barplot_count_type == 'Percentage') {
      
      ggplot(df_cutted_barplot, aes(x = df_cutted_barplot[, input$barplot_x],
                                    fill = df_cutted_barplot[,input$barplot_c])) + 
        geom_bar(aes(y = (..count..)/sum(..count..)), position = 'dodge') +
        theme_bw() + 
        labs(x = input$barplot_x, y = input$barplot_y, fill = input$barplot_c,
             title = paste0("Barplot of ", input$barplot_x, " separated by ",
                            input$barplot_c)) +
        scale_fill_brewer(palette = "Set1")
    } else if (input$barplot_count_type == "Count") {
      ggplot(df_cutted_barplot, aes(x = df_cutted_barplot[, input$barplot_x],
                                    fill = df_cutted_barplot[, input$barplot_c])) + 
        geom_bar(position = 'dodge') +
        theme_bw() + 
        labs(x = input$barplot_x, y = input$barplot_y, fill = input$barplot_c,
             title = paste0("Barplot of ", input$barplot_x, " separated by ",
                            input$barplot_c)) +
        scale_fill_brewer(palette = "Set1") 
    }
    
  })
  
  output$my_map <- renderLeaflet({
    df_cutted_map = hours_grouped_Sat %>%
      filter(created_at >= input$leaf_date[1], created_at <= input$leaf_date[2])
    r_birthplace_map <- leaflet(df_cutted_map) %>% addTiles() %>% 
      addCircleMarkers(lng = ~originLng, lat = ~oroginLat, 
                       popup = "Place", clusterOptions = markerClusterOptions()) %>% 
      setView(lat = 40.1872, lng = 44.5152, zoom = 10)
      
    r_birthplace_map
  })
  
  output$scatterplot <- renderPlot({
    if(input$scatterplot_x == "weekday"){
      scatter_weekly
    }
    else{ 
      scatter_monthly
    }
    
  })
  
  output$histogram <- renderPlot({
    df_cutted_histogram = df %>%
      filter(created_at >= input$histogram_date[1], created_at <= input$histogram_date[2])
    ggplot(df_cutted_histogram, aes(x = df_cutted_histogram[,input$histogram_x],
                                    fill = df_cutted_histogram[,input$histogram_c])) +
      facet_wrap(.~ df_cutted_histogram[,input$histogram_c], scale = "free") +
      geom_histogram(bins = input$range) +
      theme_bw() + 
      labs(x = input$histogram_x, fill = input$histogram_c,
           title = paste0("Histogram of ", input$histogram_x, " seperated by ",
                          input$histogram_c)) +
      scale_fill_viridis(discrete = TRUE)
    
    
    
  })
  
  output$Maps <- renderPlot({
    longlats <- data.frame(x = c(44.478145,44.550805),y = c(40.161538, 40.200461))
      qmplot( x = x,  y =  y , data =longlats ,source="stamen", geom = "blank", maptype = 'terrain') +
      {if(input$mapall) {geom_point(data = hours_grouped, aes(x = originLng, y = oroginLat, color = factor(group, levels =c ("Morning", "Evening"))), size = 1, alpha = 0.5)}}+
      {if(input$mapjan) {geom_point(data = hours_grouped_jan, aes(x = originLng, y = oroginLat, color = factor(group, levels =c ("Morning", "Evening"))), size = 1, alpha = 0.5)}}+
      {if(input$mapsat) {geom_point(data = hours_grouped_Sat, aes(x = originLng, y = oroginLat, color = factor(group, levels =c ("Morning", "Evening"))), size = 1, alpha = 0.5)}}+
      {if(input$mapjul) {geom_point(data = hours_grouped_Jul, aes(x = originLng, y = oroginLat, color = factor(group, levels =c ("Morning", "Evening"))), size = 1, alpha = 0.5)}}+
      {if(input$mapsept) {geom_point(data = hours_grouped_sept, aes(x = originLng, y = oroginLat, color = factor(group, levels =c ("Morning", "Evening"))), size = 1, alpha = 0.5)}}+
      {if(input$mapmon) {geom_point(data = hours_grouped_mon, aes(x = originLng, y = oroginLat, color = factor(group, levels =c ("Morning", "Evening"))), size = 1, alpha = 0.5)}}
        
        
      
        
        })
    
    
}
  
enableBookmarking(store = "url")
shinyApp(ui = ui, server = server)


