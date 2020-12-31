library(dplyr)
library(ggplot2)
library(readr)
library(shiny)

nyc_mortality = read_csv("cleaned_nyc_data.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("NYC Mortality by Race and Sex, 2007-2014"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
           selectInput(inputId = "year",
                       label = "Select Year:",
                       choices = c("2007",
                                  "2008",
                                  "2009",
                                  "2010",
                                  "2011",
                                  "2012",
                                  "2013",
                                  "2014")),
            radioButtons(inputId = "sex",
                        label = "Sex:",
                        choices = c(
                            "Female" = "F",
                            "Male" = "M"
                        )),
            radioButtons(inputId = "race",
                         label = "Race/Ethnicity:",
                         choices = unique(nyc_mortality$race_ethnicity))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("deathPlot"),
           DT::dataTableOutput("deathTable")
           )
        )
    )

# Define server logic required to draw a histogram

 server <- function(input, output) {

    selections = reactive({
      req(input$year)
      req(input$sex)
      req(input$race)
      filter(nyc_mortality, year == input$year) %>%
          filter(sex %in% input$sex) %>%
          filter(race_ethnicity %in% input$race)

    })

    output$deathPlot = renderPlot({
        ggplot(data = selections(), aes(x = reorder(leading_cause, -deaths), y = deaths)) +
        geom_bar(stat = 'identity', color = 'steelblue', fill = 'steelblue') +
        labs(
          title = "Top 10 Leading Causes of Death",
          x = "Causes",
          y = "Number of Deaths"
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust=1))
    })
    
    output$deathTable = 
      DT::renderDataTable({
      DT::datatable(selections()[,c("leading_cause", "deaths", "death_rate", "age_adjusted_death_rate")],
                    colnames = c("Leading Cause of Death", "Number of Deaths", "Death Rate", "Age-Adjusted Death Rate"),
                    options = list(order = list(2, 'des')),
                    rownames = FALSE,
        )
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
