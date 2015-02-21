library(shiny)
library(dplyr)

loadBooks <- function(tsvfile){
    librarything <- read.delim2(tsvfile)
    #Select variables
    books <- select(librarything, one_of(c("Book.Id", "Title", "Primary.Author", "Date","Rating", "Page.Count", "Acquired", "Date.Started", "Date.Read", "Languages", "Original.Languages", "ISBN", "Dewey.Decimal", "Entry.Date")))
    
    #Set correct variable types
    books$Entry.Date <- as.Date(books$Entry.Date, "[%Y-%m-%d]")
    books$Acquired <- as.Date(books$Acquired, "[%Y-%m-%d]")
    books$Date.Started <- as.Date(books$Date.Started, "[%Y-%m-%d]")
    books$Date.Read <- as.Date(books$Date.Read, "[%Y-%m-%d]")
    books$Date <- as.Date(books$Date, "%Y")
    
    books$Rating <- as.numeric(levels(books$Rating))[books$Rating]
    
    books$Title <- as.character(books$Title)
    books$ISBN <- as.character(books$ISBN)
    
    #Fill missing values
    missingAcquired <- is.na(books$Acquired)
    books$Acquired[missingAcquired] <- books$Entry.Date[missingAcquired]
    
    missingDateStarted <- is.na(books$Date.Started) & !is.na(books$Date.Read)
    books$Date.Started[missingDateStarted] <- min(books$Acquired[missingDateStarted], books$Date.Read[missingDateStarted])
    # XXX : approx 20 read books miss page counts, quick fix waiting for a manual correction in the data:
    books$Page.Count[is.na(books$Page.Count) & !is.na(books$Date.Read)] <- 300
    
    #Calculate average number of pages read by day for each finished book
    books$DailyPagesRead <- with(books, 
        ifelse(is.na(Date.Read), 
            yes = 0,
            no = Page.Count / (1 + as.numeric(Date.Read - Date.Started))
        )
    )
    
    books
}

#Load books
books <- loadBooks("librarything_aipotu.tsv")

#Calculate average number of pages read by day
readingDays <- seq(min(books$Date.Started, na.rm=TRUE), max(books$Date.Read, na.rm=TRUE), by="1 day")
dailyPageRead <- rep(0, length(readingDays)) 
names(dailyPageRead) <- readingDays
for (idbook in 1:nrow(books)){ 
    book <- books[idbook,]
    if (!is.na(book$Date.Read)){
        bookDates <- as.character(seq(book$Date.Started, book$Date.Read, by="1 day"))
        for (i in seq_along(bookDates)) {
            curday <- bookDates[i]
            dailyPageRead[curday] = dailyPageRead[curday] + book$DailyPagesRead
        }
    }
    if(length(dailyPageRead[is.na(dailyPageRead)])){
        print(book)
        dailyPageRead = 0
    }
}

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
        acquired.count <- countByPeriod(acquired$Acquired, periods, dateformat)
        
        total_started <- nrow(started)
        started.count <- countByPeriod(started$Date.Started, periods, dateformat)
        
        total_finished <- nrow(finished)
        total_rating <- mean(finished$Rating)
        finished.count <- countByPeriod(finished$Date.Read, periods, dateformat)
        
        #output$distPlot <- renderPlot({
            bookscount <- matrix(c(acquired.count, started.count, finished.count), byrow=TRUE, nrow=3)
            barplot(bookscount, names.arg = periods, beside=TRUE) 
        #})
        
        
        # Plot average number of pages read
        if (dateformat == ""){
            pagecount = sum(dailyPageRead, na.rm=TRUE)
        } else {
            pagecount = sapply(periods, function(period){ 
                inPeriod = grep(paste("^", period, sep=""), names(dailyPageRead))
                sum(dailyPageRead[inPeriod]) 
            })
        }
        output$pagesPlot <- renderPlot({
            barplot(pagecount)
        })
        
        # Plot books count
        output$globalPlot <- renderPlot({
            counts <- c(total_acquired, total_started, total_finished)
            barplot(counts, col=c("red", "blue", "green"))
        })
        
        # Plot ratings
        output$ratingsPlot <- renderPlot({
            hist(finished$Rating)
        })

        output$total_acquired <- renderText({ total_acquired })
        output$total_started <- renderText({ total_started })
        output$total_finished <- renderText({ total_finished })
        output$total_rating <- renderText({ total_rating })
    })

})


