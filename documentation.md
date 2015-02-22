## About

Since 2007, I use the Librarything website to track the books I purchase and read.
This application shows some statistics about my reading habits based on an export of this data.

Librarything give the possibility to enter many information about books, here I make use of the following :

* date acquired
* reading start date
* reading end date
* a rating between 0 and 5
* author
* original language

The source code of this application is available on Github: https://github.com/mmai/shiny-librarything

## Usage

### Input

You can display statistics for a given year between 2007 and 2015 or for all the period by selecting an entry in the "year" select box.

You can select a then refine the observation on a subset of the library by adding filters language, notes, books type.

### Ouput

Some general statistics are first provided :

* Number of books purshased
* number of books started
* number of books finished
* average note given to finished books

Plots explore the following aspects of the data

* the numbers of books acquired, started and finished each month (or each year if no year was selected)
* the repartition of the ratings
* authors are roughly ranked according to the ratings given to their books and the total number of pages read.

