##### functions Janet uses a lot...

# this includes 

# nuts and bolts r stuff
  # automatic package installation if needed!
  # detach a package if it is attached
  # close any and all graphical devices that may be open, without throwing an error if none is
# math functions 
  # geometric mean
  # percent difference
  # round percentage
  # coefficient of variation
# string functions
  # make a vector into a search string
  # Remove a bunch of formatting from chemical names 
  #     makes it easier to find matches between multiple data sets
  # find unique words in a string
  # fix casnos that got screwed up by Excel
  # Put hyphens back in "casnodash" fields
# other
  # Given a list of data frames, make one data frame with a column for which list
  #     element each row came from
  # find "empty" values - NAs, blank strings, nothing but spaces
  # given a data frame and a column name, find rows of the df with  non-unique values
  #     in that column
  # show the first few columns of the 1st few rows of a data frame or matrix  
  # show random rows of a data frame or matrix

options (stringsAsFactors = FALSE) 
# not strictly necessary for any these functions, but something I do at the 
# start of any script or session


################################################################################
# "nuts and bolts"

# function to install packages if necessary, then load
# differs from normal library function in that the package name should be quoted
# option to suppress messages makes this more complicated than it would be otherwise

library_careful <- function (packname, silent = TRUE) {
  # packname is name of package (quoted)
  # silent is boolean - suppress warning and other messages?
  do_it <- function () {
    if (!require (packname, character.only = TRUE)){
      install.packages (packname)
      library (packname, character.only = TRUE)
    }
  }  
  if(silent & packname %in% (.packages()))
    print("package already loaded")
  
  if (silent & !packname %in% (.packages())){
    suppressWarnings (suppressPackageStartupMessages (do_it()))
    print("package loaded")}
  
  else {
    do_it()}
}

# detach a package if it's  attached, without throwing an error if it isn't
detach_careful <- function (packname) {
  packsearch <- paste0 ("package:", packname)
  if( packsearch %in% search ()) {
    message ("detaching ", packname)
    detach (packsearch, unload  = TRUE, character.only = TRUE)
    }
  }

#detach all non-base R packages (helpful if you're sourcing a script that has 
#packages called at the top and you want to load packages in the correct order)
detach_all <- function(){
  for(i in names(sessionInfo()$otherPkgs)){
    package <- paste0("package:", i)
    suppressWarnings(detach(package, unload = TRUE, character.only = TRUE))
    message("detaching ", i)
  }
}

# close any and all graphical devices that may be open, without throwing an error if none is
dev.off_careful <- function()
  while (dev.cur() >1) dev.off ()
  


################################################################################
##### some basic math functions
geometric_mean <- function (x, na.rm = FALSE){
    if ( na.rm) 
        x <- na.omit (x)
    return (exp (mean (log (x))))
}
    
percent_difference <- function(initial, final)
    return (100* (final-initial)/initial)
    
percentage <- function (part, total, rounded = TRUE){
    x<-100*part/total
    if (rounded) 
        x <- round (x, 0)
    return (x)
    }

cv <- function (x, na.rm = FALSE) {
  if ( na.rm) x <- na.omit (x)
  sd (x)/mean (x)
}

################################################################################
##### functions dealing with strings

##### Convert a vector into a search string
make_search <- function (x, before = "", after = ""){
    # x is a character vector
    # Before is a string to append at the beginning of each search term
    # after is a term to attend at the end of each
    # output is a single string that can be used as a pattern in regex searches
    # E.g. make_search (c("A","B"), "^", ".$") = "^A.$|^B.$"
    separator = paste (after, "|", before, sep="")
    y <- paste (x, collapse = separator)
    y <- paste (before, y, after ,sep="")
    return (y)
    }
    

##### a function to clean chemical names;
    # this will make a lot of nonmatching names match
scrub <- function (x, extra="", remove.blanks = FALSE){
  # x is a character vector of chemical names
  # extra is search string of what to remove from names besides *,-, spaces
  #  (separated by |)
  #  often helpful to exclude commas, apostraphes, parentheses, but need to be 
  #  careful since these also have meaning... Similarly, sometimes I exclude
  #  "HCl" and other strings that are only going to differentiate between ions 
  #  and salts when I don't care
  # remove.blanks tells the function whether to convert empty strings into NA
  # output is character vector with "clean" chemical names -
  # all lowercase, troublesome characters/substrings gone
  
    pattern =paste ("\\*|-| ",extra, sep = "|")
    x <- ifelse( is.na (x), x,
        tolower (gsub(pattern, "",x)))
    if (remove.blanks)
        x[x == ""]<- NA
    return (x)
    }

    
##### get rid of repeated words in a string
unique_strings <- function (x, separator= " ", trim = FALSE){
    y <- strsplit( x,separator)
    y <- sapply (y, unique)
    if (trim)
        y <- sapply (y, str_trim)
    y <- sapply (y, paste, collapse = separator)
    return (y)
    }

    
##### fix broken cas (this happens when a CSV file with unprotected cas numbers
    # is opened in Excel, EVEN IF YOU DON'T SAVE IT) 
##### whenever using a different computer or different version of Excel, double
    # check that this works correctly; small changes in Excel behavior could 
    # break this function
fix_cas<- function (x) {
    # X is a vector of cas numbers
    # If they are formatted correctly(with 2 hyphens), leave alone
    # if they are formatted as a date (with 2 slashes), fix
    # NOT ADDRESSED: If they are formatted as the date without slashes (
      # days since 1900) -- this one is kind of hard, leave for later
    x <- ifelse(grepl ("\\d+-\\d+-\\d+",x),x,    # if formatted correctly, leave as is 
        ifelse( grepl("(\\d+)\\D(\\d+)\\D(\\d+)", x),   # if formatted as date...
            gsub("(\\d+)\\D(\\d+)\\D(\\d+)","\\3-\\2-\\1", x), # move around the order of the numbers
                x) 
            )
    x <- ifelse( grepl ("-\\d-",x), # fix dropped 0s in "middle section"
        gsub ("-(\\d)-", "-0\\1-",x),
            x)
    return (x)
}

# sometimes we "protect" CAS numbers by taking out the hyphens
# luckily, they follow a predictable pattern, so we can hyphens back in
# (variable#digits)-(2 digits)-(one digit)  
fix_cas_nodash <- function (x) {
  x <- gsub("^(\\d+)(\\d\\d)(\\d)$", "\\1-\\2-\\3", x)
  x
}   
  
################################################################################
##### messing around with vectors and data frames

##### find blanks
is_empty <- function (x, pattern ="^ *$")
    is.na( x) | grepl (pattern, x)

    
##### given a list of data frames
# add a column corresponding to the name of the data frame to each
# Then bind them all together
# there's a function to do something very like this in a package that I 
# use sometimes, but I can't remember which one...
bind_list <- function ( listin, newcol= "plate") {
    listout<- as.list ( 1: length ( listin ) )
    for(ii in 1:length( listin )){
        dat <- listin[[ii]]
        dat [, newcol]<- ""
        dat [, newcol]<- names (listin)[ii]
        listout[[ii]] <- dat
        }
    frameout <- do.call ( rbind.fill, listout )
    return ( frameout )
    }

    
##### for investigating duplicates in a data frame (based on one column)
find.duplicates<- function (df, column = 1, remove.NA = TRUE, scrub = FALSE, sort = TRUE){
#
# df -data.frame - is the data frame in which to find duplicates
# column -string or numeric- is a column name or number
# remove.NA -logical-  should we exclude rows with NA
#    in the column of interest from consideration
# scrub - logical -  should we do some cleaning on the column of interest
#    so that, e.g., slightly different names for the same chemical are more 
#    likely to show up as matches
# sort - logical - should we sort the output by the column of interest?
#
    if (remove.NA) 
        df <-  df [complete.cases (df [, column]),]
    matcher <- df [, column]
    if (scrub)
        matcher <- scrub (matcher)
    duplicates <- unique (matcher [duplicated (matcher)])
    out <- df [
        matcher %in% duplicates,
        ]
    if (sort)
      out <- out [order (out[,column]),]
    return (out)
}
    
# Show the first few rows and first few columns of a data frame or matrix
hh <- function(d, n1 = 6, n2 = n1) 
  d[1:min(n1, nrow(d)),
    1:min(n2, ncol(d))]

# Show random rows of a data frame or matrix
random_rows <- function(d, n1 = 6) {
  rows_show <- sample (nrow (d), min (n1, nrow (d)))
  d[rows_show, ]
  }


##### use next 2 functions are useful for condensing information in dplyr etc
##### pick the shortest chemical name or paste them all together
pick_shortest <- function (x){
  y <- x [which.min (nchar(x))]
  if (length (y) == 0 )
    y <- ""
  return (y)
  }
  
  
paste_condense <- function (x, sep = "; "){
  x <- str_trim (na.omit (x))
  y <- ifelse(
    all (x== ""),
    "",
    paste (unique (x [x != ""]), collapse = sep)
    )
  y
  }

sort_and_paste <- function (strings, collapse = "; ")
  strings %>% sort %>% paste (collapse = collapse)

    