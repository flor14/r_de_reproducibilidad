library(tidyverse)
library(leaflet)
library(leafpop)
library(shiny)
library(shinythemes)
library(shinyWidgets)

bikes <- read_csv("https://raw.githubusercontent.com/whipson/Ottawa_Bicycles/master/bikes_app.csv", col_types = c("?nnnnnnnnnnnnnn"))

#For ease, I've put the coordinates in a separate file, but you could just as easily rerun the 'coords' object above

coords <- read_csv("https://raw.githubusercontent.com/whipson/Ottawa_Bicycles/master/coords.csv")

bikes_plot <- bikes %>%
  pivot_longer(names_to = "counter", values_to = "count", -date) %>%
  left_join(coords, by = "counter")

bikes_total <- bikes_plot %>%
  group_by(date) %>%
  summarize(count = sum(count, na.rm = TRUE))

bikes_mean <- bikes_plot %>%
  group_by(date) %>%
  summarize(count = mean(count, na.rm = TRUE))

ui <- fluidPage(theme = shinytheme("flatly"),

                sidebarLayout(  #Layout

                  sidebarPanel(id = "Sidebar",  #Side panel
                               h2("Ottawa Bicycle Counters", align = "center", tags$style("#Sidebar{font-family: Verdana;}")),
                               fluidRow(  # Row 1 of side panel
                                 htmlOutput("caption"),  # Caption output, provides descriptive text
                                 tags$style("#caption{font-size: 16px; height: 200px; font-family: Verdana;}")
                               ),
                               fluidRow(  # Row 2 of side panel
                                 htmlOutput("stats"),  # Statistics output, provides descriptive statistics
                                 tags$style("#stats{font-size: 16px; height: 125px; font-family: Verdana;}")
                               ),
                               fluidRow(  # Row 3 of side panel
                                 switchInput("average",  # User input, allows the user to turn a switch to display the average
                                             "Display Average",
                                             value = FALSE)
                               ),
                               fluidRow(  # Row 4 of side panel
                                 htmlOutput("caption2"),  # More caption output
                                 tags$style("#caption2{font-size: 12px; height: 80px; font-family: Verdana;}")
                               ),
                               fluidRow(  # Row 5 of side panel
                                 downloadButton("download", "Download Data")  # A button so that users can download the data
                               )
                  ),
                  mainPanel(id = "Main",  # Main panel (this is where the plots and map go)
                            fluidRow(  # Row 1 of main panel
                              leafletOutput("map", height = 400)  # Here's the output for the map
                            ),
                            fluidRow(  # Row 2 of main panel
                              plotOutput("timeplot", height = 300)  # Here's the output for the time plots
                            )
                  )
                )
)

server <- function(input, output) {

  output$map <- renderLeaflet({  # Map output
    leaflet(data = coords) %>%
      addTiles() %>%
      addMarkers(~long, ~lat, label = ~name) %>%
      setMaxBounds(-75.65, 45.38, -75.75, 45.46) %>%
      addProviderTiles(providers$CartoDB.Positron)
  })

  output$caption2 <- renderUI({  # Lower caption output
    str1 <- paste("Created by ", a("Will Hipson.", href = "https://willhipson.netlify.com/"))
    str2 <- paste("Data courtesy of ", a("Open Data Ottawa.", href = "https://open.ottawa.ca/datasets/bicycle-trip-counters"))
    str3 <- "2010-01-01 - 2019-09-30"
    str4 <- "Updated on 2019-10-24"
    HTML(paste(str1, str2, str3, str4, sep = '<br/>'))
  })

  observeEvent(input$map_marker_click, { # If the user clicks a marker, this line is run.
    output$timeplot <- renderPlot({
      if(input$average == TRUE) { # if average is selected we get average overlayed
        ggplot() +
          geom_line(data = bikes_plot[bikes_plot$lat == input$map_marker_click$lat, ],
                    aes(x = date, y = count), size = .5, alpha = .70, color = "#36648B") +
          geom_line(data = bikes_mean, aes(x = date, y = count), alpha = .50, color = "#9F79EE") +
          scale_x_datetime(date_breaks = "2 years", date_labels = "%Y") +
          scale_y_continuous(limits = c(0, 6000)) +
          labs(x = NULL,
               y = "Count",
               title = paste(bikes_plot[bikes_plot$lat == input$map_marker_click$lat,]$name)) +
          theme_minimal(base_size = 16) +
          theme(plot.title = element_text(hjust = .5),
                axis.text.x = element_text(size = 16),
                text = element_text(family = "Verdana"))
      } else { # if average is not selected, then it's just the total
        ggplot() +
          geom_line(data = bikes_plot[bikes_plot$lat == input$map_marker_click$lat, ],
                    aes(x = date, y = count), size = .5, alpha = .70, color = "#36648B") +
          scale_x_datetime(date_breaks = "2 years", date_labels = "%Y") +
          scale_y_continuous(limits = c(0, 6000)) +
          labs(x = NULL,
               y = "Count",
               title = paste(bikes_plot[bikes_plot$lat == input$map_marker_click$lat,]$name)) +
          theme_minimal(base_size = 16) +
          theme(plot.title = element_text(hjust = .5),
                axis.text.x = element_text(size = 16),
                text = element_text(family = "Verdana"))
      }
    })

    output$caption <- renderUI({ # counter specific description
      str1 <- coords[coords$lat == input$map_marker_click$lat, ]$desc
      HTML(str1)
    })

    output$stats <- renderUI({ # counter specific statistics
      str1 <- "<b>Statistics</b>"
      str2 <- paste("Total count: ", format(round(sum(bikes_plot[bikes_plot$lat == input$map_marker_click$lat,]$count, na.rm = TRUE)), big.mark = ","))
      str3 <- paste("Average count: ", format(round(mean(bikes_plot[bikes_plot$lat == input$map_marker_click$lat,]$count, na.rm = TRUE), 1), big.mark = ","))
      str4 <- paste("Busiest day: ", bikes_plot[which.max(bikes_plot[bikes_plot$lat == input$map_marker_click$lat,]$count),]$date)
      HTML(paste(str1, str2, str3, str4, sep = '<br/>'))
    })
  })

  observeEvent(input$map_click, ignoreNULL = FALSE, {  # If the user clicks on the map it goes back to the cumulative data
    output$timeplot <- renderPlot({
      if(input$average == TRUE) {  # if the average is selected, it displays average
        ggplot(data = bikes_mean, aes(x = date, y = count)) +
          geom_line(size = .5, alpha = .70, color = "#36648B") +
          scale_x_datetime(date_breaks = "2 years", date_labels = "%Y") +
          labs(x = NULL,
               y = "Count") +
          theme_minimal(base_size = 16) +
          theme(plot.title = element_text(hjust = .5),
                axis.text.x = element_text(size = 16),
                text = element_text(family = "Verdana"))
      } else { # if average is not selected it is the total
        ggplot(data = bikes_total, aes(x = date, y = count)) +
          geom_line(size = .5, alpha = .70, color = "#36648B") +
          scale_x_datetime(date_breaks = "2 years", date_labels = "%Y") +
          labs(x = NULL,
               y = "Count") +
          theme_minimal(base_size = 16) +
          theme(plot.title = element_text(hjust = .5),
                axis.text.x = element_text(size = 16),
                text = element_text(family = "Verdana"))
      }
    })

    output$caption <- renderUI({  # the default caption
      str1 <- "Presenting data from bicycle counters across Ottawa. There are 14 counters spread across the city. The graph below displays how daily counts change over time. Click on a map marker to select a specific counter."
      HTML(str1)
    })

    output$stats <- renderUI({  # Statistics output
      str1 <- "<b>Statistics</b>"
      str2 <- paste("Total count: ", format(round(sum(bikes_total$count, na.rm = TRUE)), big.mark = ","))
      str3 <- paste("Average count: ", format(round(mean(bikes_total$count, na.rm = TRUE), 1), big.mark = ","))
      str4 <- paste("Busiest day: ", bikes_total[which.max(bikes_total$count),]$date)
      HTML(paste(str1, str2, str3, str4, sep = '<br/>'))
    })
  })

  output$download <- downloadHandler( # download button. Will turn 'bikes' object into a csv file.
    filename = function() {
      paste("ottawa_bikes", ".csv", sep = "")
    },

    content = function(file) {
      write.csv(bikes, file)
    }
  )
}
shinyApp(ui, server)
