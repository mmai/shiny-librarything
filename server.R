library(shiny)
library(ggplot2)

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
        languages <- input$languages
        
        #Filter by languages
        books <- allbooks
        if ("Other" %in% languages){
            excludedLanguages <- mainlanguages[!mainlanguages %in% languages]
            books <- books[!books$Original.Languages %in% excludedLanguages,]
        } else {
            books <- books[books$Original.Languages %in% languages,]
        }

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
        total_pages <- sum(dailyPageRead[names(dailyPageRead) >= date_begin & names(dailyPageRead) <= date_end])
        
        #output$distPlot <- renderPlot({
            bookscount <- matrix(c(acquired.count, started.count, finished.count), byrow=TRUE, nrow=3)
            barplot(bookscount, names.arg = periods, beside=TRUE) 
        #})
        
        #Plot authors
        author_pages <- with(finished, aggregate(Page.Count, by = list(Primary.Author), sum))
        author_ratings <- with(finished, aggregate(Rating, by = list(Primary.Author), mean))
        authors <- data.frame(author = author_pages$Group.1, nbpages = author_pages$x, ratings = author_ratings$x)
        output$authorsPlot <- renderPlot({
            ggplot(authors, aes(y=nbpages, x=ratings, label=author)) + geom_point() + geom_text(aes(label=author), hjust=0, vjust=0)
        })
        
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
            #plot(dailyPageRead[names(dailyPageRead) >= date_begin & names(dailyPageRead) <= date_end], type = "l")
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
        output$total_pages <- renderText({ total_pages })
    })

})


