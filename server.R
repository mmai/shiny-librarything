library(shiny)
library(dplyr)

loadBooks <- function(tsvfile){
    librarything <- read.delim2(tsvfile)
    #Select variables
    books <- select(librarything, one_of(c("Book.Id", "Title", "Primary.Author", "Date","Rating", "Page.Count", "Acquired", "Date.Started", "Date.Read", "Languages", "Original.Languages", "ISBN", "Dewey.Decimal", "Entry.Date")))
    
    #Set correct variable types
    books$Entry.Date = as.Date(books$Entry.Date, "[%Y-%m-%d]")
    books$Acquired = as.Date(books$Acquired, "[%Y-%m-%d]")
    books$Date.Started = as.Date(books$Date.Started, "[%Y-%m-%d]")
    books$Date.Read = as.Date(books$Date.Read, "[%Y-%m-%d]")
    books$Date = as.Date(books$Date, "%Y")
    
    books$Rating = as.numeric(levels(books$Rating))[books$Rating]
    
    books$Title = as.character(books$Title)
    books$ISBN = as.character(books$ISBN)
    
    #Fill missing values
    books$Acquired[is.na(books$Acquired)] = books$Entry.Date[is.na(books$Acquired)]
    books
}

#Load books
books <- loadBooks("librarything_aipotu.tsv")

shinyServer(function(input, output) {
  #  viz <- reactive({
    output$distPlot <- renderPlot({ 
        periods <- ""
        dateformat <- ""
        
        #date_begin <- input$dateRange[1]
        #date_end <- input$dateRange[2]
        #granularity <- input$granularity
        
        year <- input$year

        if (year == "0"){
            granularity = "year"
            date_begin = as.Date("2007-01-01")
            date_end = as.Date(Sys.time())
        } else {
            granularity = "month"
            date_begin = as.Date(paste(year, "-01-01", sep=""))
            date_end = as.Date(paste(year, "-12-31", sep=""))
        }
        
        switch(granularity, 
               "all" = {
                   periods <- c("All")
               },
               "year"  = { 
                   dateformat <- "%Y"
                   periods <- strftime(seq(date_begin, date_end, "year"), dateformat) 
               },
               "month" = {
                   dateformat <- "%Y-%m"
                   periods <- strftime(seq(date_begin, date_end, "month"), dateformat) 
               }
        )
        
        acquired <- books[books$Acquired >= date_begin & books$Acquired <= date_end,]
        started <- books[!is.na(books$Date.Started) & books$Date.Started >= date_begin & books$Date.Started <= date_end,]
        finished <- books[!is.na(books$Date.Read) & books$Date.Read >= date_begin & books$Date.Read <= date_end,]
        
        countByPeriod <- function(datevector, periods, dateformat){
            if (dateformat == ""){
                length(datevector)
            } else {
                sapply(periods, function(period){ 
                    length(datevector[strftime(datevector, dateformat) == period]) 
                })
            }
        }
        
        total_acquired <- nrow(acquired)
        acquired.agg <- countByPeriod(acquired$Acquired, periods, dateformat)
        
        total_started <- nrow(started)
        started.agg <- countByPeriod(started$Date.Started, periods, dateformat)
        
        total_finished <- nrow(finished)
        finished.agg <- countByPeriod(finished$Date.Read, periods, dateformat)
        
        #output$distPlot <- renderPlot({
            agg <- matrix(c(acquired.agg, started.agg, finished.agg), byrow=TRUE, nrow=3)
            barplot(agg, names.arg = periods, beside=TRUE) 
        #})
        
        output$globalPlot <- renderPlot({
            counts <- c(total_acquired, total_started, total_finished)
            barplot(counts, col=c("red", "blue", "green"))
        })

        output$total_acquired <- renderText({ total_acquired })
        output$total_started <- renderText({ total_started })
        output$total_finished <- renderText({ total_finished })
    })

})


