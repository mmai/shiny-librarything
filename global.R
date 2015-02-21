library(dplyr)

loadBooks <- function(tsvfile){
    librarything <- read.delim2(tsvfile)
    #Select variables
    books <- select(librarything, one_of(c("Book.Id", "Title", "Primary.Author", "Date","Rating", "Page.Count", "Acquired", "Date.Started", "Date.Read", "Languages", "Original.Languages", "Entry.Date")))
    
    #Set correct variable types
    books$Entry.Date <- as.Date(books$Entry.Date, "[%Y-%m-%d]")
    books$Acquired <- as.Date(books$Acquired, "[%Y-%m-%d]")
    books$Date.Started <- as.Date(books$Date.Started, "[%Y-%m-%d]")
    books$Date.Read <- as.Date(books$Date.Read, "[%Y-%m-%d]")
    books$Date <- as.Date(books$Date, "%Y")
    
    books$Rating <- as.numeric(levels(books$Rating))[books$Rating]
    
    books$Title <- as.character(books$Title)
    
    #Fill missing values
    missingAcquired <- is.na(books$Acquired)
    books$Acquired[missingAcquired] <- books$Entry.Date[missingAcquired]
    rm(missingAcquired)
    
    missingDateStarted <- is.na(books$Date.Started) & !is.na(books$Date.Read)
    books$Date.Started[missingDateStarted] <- min(books$Acquired[missingDateStarted], books$Date.Read[missingDateStarted])
    # XXX : approx 20 read books miss page counts, quick fix waiting for a manual correction in the data:
    books$Page.Count[is.na(books$Page.Count) & !is.na(books$Date.Read)] <- 300
    rm(missingDateStarted)
    
    missingLanguage <- books$Original.Languages == ""
    books$Original.Languages[missingLanguage] <- books$Languages[missingLanguage]
    rm(missingLanguage)
    
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
allbooks <- loadBooks("librarything_aipotu.tsv")
mainlanguages <- c("English", "French", "German", "Spanish")