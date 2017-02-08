# R Programming
# Problem Set 2: Functions 
# Author: Jonas Markgraf
# Date: Feb 2017
# =========================


### Benford's law
## 1) Calculating violations
## ========================

# generate random sample vector ------------------
prop_vote <- sample(1:1000000, size=100000) 


# create function for "m" and "d" statistic -------------------------
violations <- function(x, m = TRUE, d = TRUE) {
  # Function name: violations()
  # Purpose: function tests Benfords law by calculating Leemis m and ChoGains d
  # Function: 
  #   1) extracts first digit of the input vector "x" and generates an observed prop frequency vector
  #   2) applies  Leemis m function and/or Cho Gains d function to prop frequency vector
  #   3) returns list with test statistic and full digit distribution
  #   User option to only calculate m, d or both statistics
  # Args:
  #   x: random numeric vector
  #   m: logical vector; if TRUE, Leemis m is calculated; default value, TRUE
  #   d: logical vector; if TRUE, ChoGains d is calculated; default value, TRUE
  # Author: Jonas Markgraf
  first.digit <- substr(as.character(x), start = 1, stop = 1) # extract first digit of prop_vote vector
  first.digit <- as.integer(first.digit)
  Xi <- table(first.digit)/length(prop_vote) # generate observed proportional frequency vector
  if(m == T & d == F) {
    m = max(Xi - log10(1 + (1/c(1:9))))
    return(list("Leemis' m" = m, distribution = Xi))  # output1: if m == T & d == F, we only see "m" statistic
  }
  if(d == T & m == F) {
    d = sqrt(sum(Xi - log10(1 + (1/c(1:9)))^2))
    return(list("Cho-Gains' d" = d, distribution = Xi)) # output2: if m == F & d == T, we only see "d" statistic
  }
  if(d == T & m == T) {
    m = max(Xi - log10(1 + (1/c(1:9))))
    d = sqrt(sum(Xi - log10(1 + (1/c(1:9)))^2))
    return(list("Leemis' m" = m, "Cho-Gains' d" = d, distribution = Xi)) # output3: if m == T & d == T, we see both statistics
  }
}

# testing function
violations(prop_vote)
violations(prop_vote, m = F)

## 2) Critical Values
## ===================

# create function for critical values ------------------

print.benfords <- function(x, m = TRUE, d = TRUE) {
  # Function name: print.benfords()
  # Purpose: function tests Benfords law (Leemis m; ChoGains d) and defines critical values for "no fraud"
  # Function: 
  ##   1) extracts first digit of the input vector "x"; 
  ###       generates an observed prop frequency vector;
  ###       generates vector "significance" defining the levels of stat significance 
  ##   2) applies  Leemis m function and/or Cho Gains d function to prop frequency vector
  ##   3) defines critical values for Leemis m and ChoGains d for "no fraud" (no asterisks - no fraud)
  ##   4) returns list with test statistic and a definition of the stat. significance levels
  ##   User option to only calculate m, d or both statistics
  # Args:
  ##   x: random numeric vector
  ##   m: logical vector; if TRUE, Leemis m is calculated; default value, TRUE
  ##   d: logical vector; if TRUE, ChoGains d is calculated; default value, TRUE
  # Author: Jonas Markgraf
  first.digit <- substr(as.character(x), start = 1, stop = 1) # extract first digit of prop_vote vector
  first.digit <- as.integer(first.digit)
  Xi <- table(first.digit)/length(x) # generate observed proportional frequency vector
  significance <- "* 10%; ** 5%; *** 1%" # vector explaining asterisks

  if(m == T & d == F) {
    m = max(Xi - log10(1 + (1/c(1:9))))
    m <- if(m >= 1.212) { # add asterisks for critical values of "m" statistic
      paste0(m, "***")
    } else if(m >= .967) {
      paste0(m, "**")
    } else if(m >= .851) {
      paste0(m, "*")
    } else {
      m
    }
    values1 <- c(m, significance)
    rnames1 <- c("Leemis m", "Significance:")
    return(data.frame(values1, row.names = rnames1))  # output1: if m == T & d == F, we only see "m" statistic
    
  }
  if(d == T & m == F) {
    d = sqrt(sum(Xi - log10(1 + (1/c(1:9)))^2))
    d <- if(d >= 1.569) { # add asterisks for critical values of "d" statistic
      paste0(d, "***")
    } else if(d >= 1.33) {
      paste0(d, "**")
    } else if(d >= 1.212) {
      paste0(d, "*")
    } else {
      d
    }
    values2 <- c(d, significance)
    rnames2 <- c("Cho-Gains d", "Significance:")
    return(data.frame(values2, row.names = rnames2)) # output2: if m == F & d == T, we only see "d" statistic
  }
  
  if(d == T & m == T) {
    m = max(Xi - log10(1 + (1/c(1:9))))
    m <- if(m >= 1.212) { # add asterisks for critical values of "m" statistic
      paste0(m, "***")
    } else if(m >= .967) {
      paste0(m, "**")
    } else if(m >= .851) {
      paste0(m, "*")
    } else {
      m
    }
    
    d = sqrt(sum(Xi - log10(1 + (1/c(1:9)))^2))
    d <- if(d >= 1.569) { # add asterisks for criticl values of "d" statistic
      paste0(d, "***")
    } else if(d >= 1.33) {
      paste0(d, "**")
    } else if(d >= 1.212) {
      paste0(d, "*")
    } else {
      d
    }
    values3 <- c(m, d, significance)
    rnames3 <- c("Leemis m", "Cho-Gains d", "Significance:")
    return(data.frame(values3, row.names = rnames3)) # output3: if m == T & d == T, we see both statisitics
  }
}

# testing function
print.benfords(prop_vote)
print.benfords(prop_vote, d = F)

## 3) Create function that creates CSV file containing table -------------
## ===========================================================

export.benfords2 <- function(x, name = "benfords_output2.csv") {
  # Function name: export.benfords2()
  # Purpose: Write CSV file with result from "print.benfords" defined above
  # Function:
  ##  1) applies input "x" to "print.benfords" function;
  ##  2) writes output as csv file in the directory defined in "paste0"
  ##  Name of the file can be defined with the "name" option
  # Args:
  ##   x: any numeric vector
  ##   name: default name of output is "benfords_output2.csv"; can be changed
  # Author: Jonas Markgraf
  write.csv(print.benfords(x), paste0("~/Dropbox/Hertie School/(4) Applied Statistical Programming (WUSTL)/Repositories/PS2/", name))
} 

# testing function
export.benfords2(prop_vote)

## I tried to use the sink function (see below), but the output file is empty. ------

export.benfords <- function(x, name = "benfords_output.csv") {
  # Function name: export.benfords()
  # Purpose: Write CSV file with result from "print.benfords" defined above
  # Function steps:
  ##  1) defines where to divert the output of the function to; directory defined in "paste0"; 
  ##  2) applies input "x" to "print.benfords" function;
  ##  3) sinks output to defined directory
  ##  Name of the output file can be defined with the "name" option
  # Args:
  ##   x: any numeric vector
  ##   name: default name of output is "benfords_output.csv"
  # Author: Jonas Markgraf
  sink(paste0("~/Dropbox/Hertie School/(4) Applied Statistical Programming (WUSTL)/Repositories/PS2/", name))
  print.benfords(x)
  sink()
}

# testing function
export.benfords(prop_vote) # prints empty csv file...
