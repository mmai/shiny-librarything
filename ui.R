library(shiny)
library(markdown)

shinyUI(navbarPage("Librarything",
    tabPanel("Documentation",       
             fluidRow(
                 column(6, includeMarkdown("about.md")),
                 column(6, includeMarkdown("documentation.md"))
             )
        #includeMarkdown("documentation.md")
    ),
    tabPanel("Application",
             # Application title
             titlePanel("Reading habits"),
    # Sidebar with a slider input for the number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput('year', 'Year:', c("All" = 0, 2007:2015))
            #,dateRangeInput('dateRange', label = 'Date range input: yyyy-mm-dd',  start = "2007-01-01", end = "2015-02-01"),
            #,radioButtons("granularity", "Time granularity:", c("All" = "all", "year" = "year", "month" = "month"))
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            wellPanel(
                span("Number of books bought:", textOutput("total_acquired")),
                span("Number of books started:", textOutput("total_started")),
                span("Number of books finished:", textOutput("total_finished")),
                span("Average rating:", textOutput("total_rating"))
            ),
            plotOutput("globalPlot"),
            plotOutput("pagesPlot"),
            plotOutput("ratingsPlot"),
            plotOutput("distPlot")
        )
    )
    )
))

