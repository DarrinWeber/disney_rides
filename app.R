library(tidyverse)
library(lubridate)
library(shiny)

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

    # Application title
    titlePanel("Disney World Wait Times"),

    wellPanel(
      selectInput(inputId = "park",
                  label = "Select Park:",
                  choices = unique(df$park)),
      uiOutput("park_rides")
    ),
    plotOutput("plot")
)


server <- function(input, output) {
  
    df_react <- reactive(
      df %>% 
        filter(park == input$park,
               ride %in% input$selected_rides) %>% 
        group_by(park, ride, new_time) %>% 
        summarize(avg = mean(wait, na.rm = TRUE), .groups = "drop")
    )

    output$plot <- renderPlot({
       ggplot(df_react()) +
        geom_col(aes(x = new_time, y = avg)) +
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
                     selected = rides[1],
                     multiple = TRUE)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
