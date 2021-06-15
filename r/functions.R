# Functions for Analysis


# Function to pull relevant data based on reported zip code
zipState <- function(x, dataset) {
  if (!is.na(dataset$x)) {
    dataset$stateFromZip <- reverse_zipcode(x)$state
  }
  if (is.na(dataset$x)) {
    dataset$stateFromZip <- NA
  }
}



decode <- function(x, search, replace, default = NULL) {
  
  # build a nested ifelse function by recursion
  decode.fun <- function(search, replace, default = NULL)
    if (length(search) == 0L) {
      function(x) if (is.null(default)) x else rep(default, length(x))
    } else {
      function(x) ifelse(x == search[1L], replace[1L],
                         decode.fun(tail(search,  -1L),
                                    tail(replace, -1L),
                                    default)(x))
    }
  
  return(decode.fun(search, replace, default)(x))
}

sraFunction <- function(x) {
  as.numeric(decode(x, 
                    search = c("Never", "Once", "More than once", "Often", "Very often", "Not Applicable"),
                    replace = c("0", "1", "2", "3", "4", "0"))) 
}

sraNA <- function(x) {
  as.numeric(decode(x, 
                    search = c("Never", "Once", "More than once", "Often", "Very often", "Not Applicable"),
                    replace = c("0", "1", "2", "3", "4", "NA"))) 
}

agreeScale <- function(x) {
  tolower(x) -> x
  as.numeric(decode(x,
                    search = c("strongly agree", "agree", "neither agree nor disagree", "disagree", "strongly disagree"),
                    replace = c("5", "4", "3", "2", "1")))
}

agreeScaleR <- function(x) {
  tolower(x) -> x
  as.numeric(decode(x,
                    search = c("strongly agree", "agree", "neither agree nor disagree", "disagree", "strongly disagree"),
                    replace = c("1", "2", "3", "4", "")))
}

yesNoUnsure <- function(x) {
  tolower(x) -> x
  decode(x,
        search = c("yes", "no", "unsure"),
        replace = c("1", "0", "unsure"))
}

yesNo <- function(x) {
  tolower(x) -> x
  as.numeric(decode(x,
                    search = c("yes", "no"),
                    replace = c("1", "0")))
}

safe <- function(x) {
  x <- tolower(x)
  y <- decode(x,
        search = c("extremely safe", "somewhat safe", "somewhat unsafe", "extremely unsafe", "unsure"),
        replace = c("1", "2", "3", "4", "unsure"))
  z <- suppressWarnings(as.numeric(y))
  avg <- mean(as.numeric(z), na.rm = T)
  z <- ifelse(is.na(z), avg, z)  
  z
}

effective <- function(x) {
  tolower(x) -> x
  as.numeric(decode(x, 
                    search = c("extremely ineffective", "somewhat ineffective", "somewhat effective", 
                               "extremely effective", "unsure"),
                    replace = c(1, 2, 3, 4, NA)))
}

confidence <- function(x) {
  tolower(x) -> x
  as.numeric(decode(x,
                    search = c("no confidence at all", "not too much", "a fair amount", "a great deal"),
                    replace = c(1, 2, 3, 4)))
}

frequencyDays <- function(x) {
  tolower(x) -> x
  as.numeric(decode(x, 
                    search = c("not at all", "several days", "more than half the days", "nearly every day"),
                    replace = c(0, 1, 2, 3)))
}

oftenNever <- function(x) {
  tolower(x) -> x
  as.numeric(decode(x, 
                    search = c("never", "almost never", "sometimes", "fairly often", "very often"),
                    replace = c(0, 1, 2, 3, 4)))
}


lottery <- function(x) {
  x <- ifelse(grepl("Lottery A", x), 1, x)
  x <- ifelse(grepl("Lottery B", x), 2, x)
  as.numeric(x)
}

# Lottery analysis functions
first_switch = function(obs) {
  switch_point = 0
  for (i in 1:length(obs)) {
    if (obs[i]==2) {
      switch_point = i
      break
    }
  }
  return(switch_point) 
}

count_na = function(obs) {
  na_count = 0
  for (i in 1:length(obs)) {
    if (obs[i]==0) {
      na_count = na_count + 1
    }
  }
  return(na_count)
}

count_switches = function(obs) {
  switches = 0
  for (i in 2:length(obs)) {
    if (obs[i-1] != obs[i]) {
      switches = switches + 1
    }
  }
  return(switches)
}