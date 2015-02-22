library(shiny)
library(markdown)

shinyUI(
    pageWithSidebar(
        headerPanel('Reading habits'),
        sidebarPanel(
            includeMarkdown("documentation.md")    
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            fluidRow(
                column(6,
                       selectInput('year', 'Year:', c("All" = 0, 2007:2015)),
                       checkboxGroupInput('languages', 'Original language', 
                                          c(mainlanguages, "Other"),
                                          selected =  c(mainlanguages, "Other"),
                       )
                      ),
                column(6,   wellPanel(
                    h4("Summary", textOutput("summary_info", inline=TRUE)),
                    div("Number of books acquired:", strong(textOutput("total_acquired", inline=TRUE))),
                    div("Number of books started:", strong(textOutput("total_started", inline=TRUE))),
                    div("Number of books finished:", strong(textOutput("total_finished", inline=TRUE))),
                    #div("Number of pages read:", strong(textOutput("total_pages", inline=TRUE))),
                    div("Average rating:", strong(textOutput("total_rating", inline=TRUE)))
                ))
                )
          ,
            #plotOutput("pagesPlot"),
            plotOutput("distPlot"),
            plotOutput("ratingsPlot"),
            plotOutput("authorsPlot")
        )
    )
)

