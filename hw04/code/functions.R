a <- c(1, 4, 7, NA, 10)
# remove_missing() takes a vector, and returns the input vector without
# missing values
remove_missing <- function(vec) {
  for (i in 1:length(vec)) {
    vec <- vec[!is.na(vec)]
  }
  return(vec)
}

# get_minimum() takes a numeric vector, and an optional logical na.rm
# argument, and returns the minimum value.
get_minimum <- function(nvec, na.rm = FALSE) {
  if (!is.numeric(nvec)) {
    return("ERROR: non-numeric argument")
  }
  if (na.rm) {
    return(min(remove_missing(nvec)))
  }
  return(min(nvec))
}

# get_maximum() takes a numeric vector, and an optional logical na.rm 
# argument, and returns the maximum value
get_maximum <- function(nvec, na.rm = FALSE) {
  if (!is.numeric(nvec)) {
    return("ERROR: non-numeric argument")
  }
  if (na.rm) {
    return(max(remove_missing(nvec)))
  }
  return(max(nvec))
}

# get_range() takes a numeric vector, and an optional logical na.rm
# argument, and returns the overall range of the input vector.
get_range <- function(nvec, na.rm = FALSE) {
  if (!is.numeric(nvec)) {
    return("ERROR: non-numeric argument")
  }
  return(get_maximum(nvec, na.rm) - get_minimum(nvec, na.rm))
}

# get_percentile10() takes a numeric vector, and an optional logical na.rm 
# argument, and returns the 10th percentile of the input vector.
get_percentile10 <- function(nvec, na.rm = FALSE) {
  if (!is.numeric(nvec)) {
    return("ERROR: non-numeric argument")
  }
  return(quantile(remove_missing(nvec), probs = 0.1, na.rm = na.rm)[[1]])
}

# get_percentile90() takes a numeric vector, and an optional logical na.rm 
# argument, and returns the 90th percentiles of the input vector.
get_percentile90 <- function(nvec, na.rm = FALSE) {
  if (!is.numeric(nvec)) {
    return("ERROR: non-numeric argument")
  }
  return(quantile(remove_missing(nvec), probs = 0.9, na.rm = na.rm)[[1]])
}
get_percentile90(1:100, na.rm = FALSE)
# get_median() take a numeric vector, and an optional logical na.rm argument
# and returns the median of the input vector.
get_median <- function(nvec, na.rm = FALSE) {
  if (!is.numeric(nvec)) {
    return("ERROR: non-numeric argument")
  }
  if (na.rm) {
    sorted <- sort(remove_missing(nvec))
    if (length(sorted) == 1) {
      return(sorted[1])
    } else if (length(sorted) %% 2 == 0) {
      return((sorted[length(sorted) / 2] + sorted[(length(sorted) / 2) + 1]) / 2)
    } else if (length(sorted) %% 2 == 1) {
      return((sorted[round(length(sorted) / 2)]))
    } else {
      sorted <- sort(nvec)
      if (length(sorted) == 1) {
        return(sorted[1])
      } else if (length(sorted) %% 2 == 0) {
        return((sorted[length(sorted) / 2] + sorted[(length(sorted) / 2) + 1]) / 2)
      } else if (length(sorted) %% 2 == 1) {
        return((sorted[round(length(sorted) / 2)]))
      }
    }
  } else {
    sorted <- sort(nvec)
    if (length(sorted) == 1) {
      return(sorted[1])
    } else if (length(sorted) == 1) {
      return(sorted[1])
    } else if (length(sorted) %% 2 == 0) {
      return((sorted[length(sorted) / 2] + sorted[(length(sorted) / 2) + 1]) / 2)
    } else if (length(sorted) %% 2 == 1) {
      return((sorted[round(length(sorted) / 2)]))
    } else {
      sorted <- sort(nvec)
      if (length(sorted) %% 2 == 0) {
        return((sorted[length(sorted) / 2] + sorted[(length(sorted) / 2) + 1]) / 2)
      } else if (length(sorted) %% 2 == 1) {
        return((sorted[round(length(sorted) / 2)]))
      }
    }
  }
}

# get_average() takes a numeric vector, and an optional logical na.rm argument
# and returns the average of the input vector.
get_average <- function(nvec, na.rm = FALSE) {
  sum = 0
  if (!is.numeric(nvec)) {
    return("ERROR: non-numeric argument")
  }
  if (na.rm) {
    nvec <- remove_missing(nvec)
  }
  for(i in 1:length(nvec)) {
    sum = sum + nvec[i]
  }
  avg = sum / length(nvec)
  return(avg)
}

# get_stdev() takes a numeric vector, and an optional logical na.rm argument
# and returns the standard deviation of the input vector
get_stdev <- function(nvec, na.rm = FALSE) {
  sum = 0
  if (!is.numeric(nvec)) {
    return("ERROR: non-numeric argument")
  }
  if (na.rm) {
    nvec <- remove_missing(nvec)
  }
  for(i in 1:length(nvec)) {
    sum = sum + ((nvec[i] - get_average(nvec))**2)
  }
  sd = sqrt(sum / (length(nvec) - 1)) 
  return(sd)
}

# get_quartile1() takes a numeric vector, and an optional logical na.rm
# argument and returns the first quartile of the input vector.
get_quartile1 <- function(nvec, na.rm = FALSE) {
  if (!is.numeric(nvec)) {
    return("ERROR: non-numeric argument")
  }
  return(quantile(remove_missing(nvec), probs = 0.25, na.rm = na.rm)[[1]])
}

# get_quartile3() takes a numeric vector, and an optional logical na.rm
# argument and returns the third quartile of the input vector.
get_quartile3 <- function(nvec, na.rm = FALSE) {
  if (!is.numeric(nvec)) {
    return("ERROR: non-numeric argument")
  }
  return(quantile(remove_missing(nvec), probs = 0.75, na.rm = na.rm)[[1]])
}

# count_missing() takes a numeric vector, and returns the number of missing values.
count_missing <- function(nvec) {
  return(length(nvec) - length(remove_missing(nvec)))
}

# summary_stats() takes a numeric vector, and returns a list of summary statistics.
summary_statistics <- function(nvec) {
  stats <- list()
  stats[1] = (get_minimum(nvec, TRUE))
  stats[2] = (get_percentile10(nvec, TRUE))
  stats[3] = (get_quartile1(nvec, TRUE))
  stats[4] = (get_median(nvec, TRUE))
  stats[5] = (get_average(nvec, TRUE))
  stats[6] = (get_quartile3(nvec, TRUE))
  stats[7] = (get_percentile90(nvec, TRUE))
  stats[8] = (get_maximum(nvec, TRUE))
  stats[9] = (get_range(nvec, TRUE))
  stats[10] = (get_stdev(nvec, TRUE))
  stats[11] = (count_missing(nvec))
  names(stats) <- c('minimum', 'percent10', 'quartile1', 'median', 'mean', 'quartile3',
                    'percent90', 'maximum', 'range', 'stdev', 'missing')
  return(stats)
}


# print_stats() takes a list of summary statistics, and prints the values in a nice 
# format
print_stats <- function(stats) {
  for(i in 1:length(stats)) {
    a <- format(names(stats)[i], width = nchar("percent10"), justify = "left")
    print(paste(a, ": ", format(round(stats[[i]], 3), nsmall = 4), sep = ""), quote = FALSE)
  }
}
print_stats(summary_statistics(a))

b <- c(18, 15, 16, 4, 17, 9)
# rescale100() takes a numeric vector x, a minimum xmin, and a maximum xmax, and 
# returns a rescaled vector with a potential scale from 0 to 100
rescale100 <- function(x, xmin, xmax) {
  z <- 100 * (x - xmin) / (xmax - xmin)
  return(z)
}
rescale100(c(1, 2, 3, 4), xmin = 0, xmax = 10)

# drop_lowest() takes a numeric vector of length n, and returns a vector of length
# n - 1 by dropping the lowest value
drop_lowest <- function(vec) {
  if (!is.numeric(vec)) {
    return("ERROR: non-numeric argument")
  }
  for(i in 1:length(vec)) {
    if (vec[i] == min(vec)) {
      vec[i] <- NA
      return(remove_missing(vec))
    }
  }
}

# score_homework() takes a numeric vector of homework scores (of length n), and an
# optional logical argument drop, returning the average of the homework scores. If 
# drop = TRUE, the lowest HW is dropped.
score_homework <- function(hw, drop = FALSE) {
  if (!is.numeric(hw)) {
    return("ERROR: non-numeric argument")
  }
  if (drop) {
    hw <- drop_lowest(hw)
  }
  return(get_average(hw))
}

hws <- c(100, 80, 30, 70, 75, 85)
score_homework(hws, drop = TRUE)
# score_quiz() takes a numeric vector of quiz score (of length n), and an optional
# logical argument drop, returning the average of the quiz scores. If drop = TRUE,
# the lowest quiz score is dropped.
score_quiz <- function(quiz, drop = FALSE) {
  if (!is.numeric(quiz)) {
    return("ERROR: non-numeric argument")
  }
  if (drop) {
    quiz <- drop_lowest(quiz)
  }
  return(get_average(quiz))
}

quizzes <- c(100, 80, 70, 0)

# score_lab() takes a numeric value of lab attendance, and returns the lab score.
score_lab <- function(lab) {
  if (!is.numeric(lab)) {
    return("ERROR: non-numeric argument")
  }
  if (lab == 11 | lab == 12) {
    return(100)
  } else if (lab == 10) {
    return(80)
  } else if (lab == 9) {
    return(60)
  } else if (lab == 8) {
    return(40)
  } else if (lab == 7) {
    return(20)
  } else if (lab <= 6) {
    return(0)
  }
}

