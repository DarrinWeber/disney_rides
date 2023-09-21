library(tidyverse)
library(lubridate)
library(shiny)
library(DT)

now_time <- now(tzone = "US/Eastern")
current_time <- force_tz(as_datetime(hm(str_c(
  hour(now_time),
  minute(now_time),
  sep = " "
))),
tzone = "US/Eastern") %>%
  update(
    year = year(now_time),
    month = month(now_time),
    day = day(now_time)
  )

begin_time <- update(now_time,
                     hour = 8,
                     minute = 0,
                     second = 0)
end_time <- update(now_time,
                   hour = 23,
                   minute = 59,
                   second = 59)

df <- read_csv("data/disney_ride_wait_times.csv",
               col_types = cols(
                 park = col_character(),
                 ride = col_character(),
                 type = col_character(),
                 day = col_datetime(),
                 hour = col_character(),
                 wait = col_integer()
               ),
               na = c("", "na")
) %>%
  mutate(new_time = update(
    parse_date_time(hour,
                    "%H:%M",
                    tz = "US/Eastern"),
    year = year(now_time),
    month = month(now_time),
    day = day(now_time)
  )) %>% 
  drop_na()


ui <- fluidPage(
  
  titlePanel("Disney World Wait Times"),
  
  wellPanel(
    selectInput(inputId = "park",
                label = "Select Park:",
                choices = unique(df$park))
  ),
  tabsetPanel(
    type = "tabs",
    tabPanel("Plot", 
             uiOutput("park_rides"),
             plotOutput("plot")),
    tabPanel("Right Now",
             DTOutput("table")),
    tabPanel("Best Times",
             numericInput("table_length",
                          "Number of Times to Show per Ride",
                          value = 4),
             DTOutput("top_times")),
    
  ),
  tags$footer("Error bars represent minimum and maximum wait times.")
  
)


server <- function(input, output) {
  
  df_plot <- reactive(
    df %>% 
      filter(park == input$park,
             ride %in% input$selected_rides) %>% 
      group_by(park, ride, new_time) %>% 
      summarize(avg = mean(wait, na.rm = TRUE), 
                min = min(wait, na.rm = TRUE),
                max = max(wait, na.rm = TRUE),
                .groups = "drop")
  )
  
  output$plot <- renderPlot({
    req(input$park, input$selected_rides)
    ggplot(df_plot()) +
      geom_col(aes(x = new_time, y = avg)) +
      geom_errorbar(aes(x = new_time, y = avg, ymin = min, ymax = max),
                    width = 1000) +
      geom_vline(xintercept = current_time,
                 color = "red",
                 linewidth = 1.5) +
      ggforce::facet_col(facets = vars(ride), 
                         scales = "fixed", 
                         space = "fixed") +
      scale_x_datetime(breaks = scales::date_breaks("2 hours"),
                       minor_breaks = scales::date_breaks("1 hour"), 
                       date_labels = "%I %p",
                       limits = c(begin_time, end_time)) +
      scale_y_continuous(n.breaks = 7) +
      labs(x = "Time of Day", y = "Average Wait Time (min)")
  })
  
  output$park_rides <- renderUI({
    rides <- df %>% 
      filter(park == input$park) %>% 
      pull(ride) %>% 
      unique()
    selectizeInput("selected_rides",
                   label = "Select rides:",
                   choices = rides,
                   multiple = TRUE)
  })
  
  output$table <- renderDT({
    DT::datatable(
      df %>% 
        filter(park == input$park,
               hour(new_time) == hour(current_time) + 1) %>% 
        group_by(park, ride, hour) %>% 
        summarize(avg = round(mean(wait, na.rm = TRUE), 0), .groups = "drop") %>% 
        select(ride, avg) %>% 
        arrange(avg, ride) %>% 
        rename(Ride = ride, "Avg. Wait Time" = avg), 
      options = list(
        lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
        pageLength = -1
      )
    )
  })
  
  output$top_times <- renderDT({
    DT::datatable(
      df %>% 
        filter(park == input$park) %>% 
        group_by(ride, hour) %>% 
        summarize(avg = round(mean(wait, na.rm = TRUE), 0), .groups = "drop") %>% 
        group_by(ride) %>% 
        slice_min(order_by = avg, n = input$table_length, na_rm = TRUE) %>% 
        mutate(hour_formatted = format(strptime(hour, "%H:%M"), "%I %p")) %>% 
        select(ride, hour_formatted, avg) %>% 
        rename(Ride = ride, Time = hour_formatted, "Avg. Wait Time" = avg), 
      options = list(
        lengthMenu = list(c(input$table_length * 3, input$table_length * 4, -1), 
                          c(str_c(input$table_length * 3), 
                            str_c(input$table_length * 4),
                            'All')),
        pageLength = input$table_length * 3
      )
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
