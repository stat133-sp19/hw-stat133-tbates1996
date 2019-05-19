# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#Checks the probability to make sure it is valid
check_prob <- function(prob) {
  if (length(prob) == 1 & prob >= 0 & prob <= 1) {
    return(TRUE)
  } else {
    throw("Invalid probability")
  }
}

#Checks the trials to make sure it is valid
check_trials <- function(trials) {
  if(trials < 0 | length(trials) > 1){
    stop("Invalid number of trials")
  }
  return(TRUE)
}

# Checks the success to make sure it is valid
check_success <- function(success, trials) {
  if(success < 0 | success > trials ){
    stop("Invalid number of success")
  }
  return(TRUE)
}

# Calculates the mean given trials and prob
aux_mean <- function(trials, prob) {
  return(trials * prob)
}

# Calculates the variance given trials and prob
aux_variance <- function(trials, prob) {
  var <- trials*prob*(1-prob)
  return(var)
}

# Calculates the mode given trials and prob
aux_mode <- function(trials, prob) {
  return(as.integer(trials*prob + prob))
}

# Calculates the skewness given trials and prob
aux_skewness <- function(trials, prob) {
  numerator <- 1- 2*prob
  denominator <- sqrt((prob * trials) *(1 - prob))
  return(numerator/denominator)
}

# Calculates the kurtosis given trials and prob
aux_kurtosis <- function(trials, prob) {
  num <- 1 - (6*prob * (1-prob))
  den <- trials * prob * (1 - prob)
  return(num/den)
}
#' @title Choose function
#' @description computes n choose k
#' @param n number of elements to choose from
#' @param k number of elements to choose
#' @return choose(n,k)
#' @export
#' @examples
#' bin_choose(5,7)
bin_choose <- function(n,k) {
  if(k > n) {
    stop("Can't have k > n")
  }
  return(factorial(n)/ (factorial(k) * factorial(n - k)))
}

#' @title Binomial probability
#' @description computes the probability of a binomial distribution
#' @param success number of successes in the trials
#' @param trials number of trials to attempt
#' @param prob probability of success
#' @return The probability of success number of successes over trials attemps given prob probabiltiy
#' @export
#' @examples
#' bin_probability(5,7,0.5)
bin_probability <- function(success, trials, prob) {
  check_prob(prob)
  check_trials(trials)
  check_success(success, trials)
  return(bin_choose(trials,success) * prob^success * (1-prob)^(trials-success))
}

#' @title Binomial distribution
#' @description gives the binomial distribution of all number of successes over a certain number of trials
#' @param trials number of trials to attempt
#' @param prob probability of success
#' @return a data.frame with all probabilities of any success over n trials of type bindist
#' @export
#' @examples
#' bin_distribution(5,0.5)
bin_distribution <- function(trials, prob) {
  dist <- data.frame("success" = 0:trials, "probability" = rep(0,trials + 1))
  for (i in 0:trials) {
    dist[i + 1,]$probability <- bin_probability(i, trials, prob)
  }
  class(dist) <- c("bindis", "data.frame")
  return(dist)
}

#' @title Plot binomial distribution
#' @description Plots a barplot of the binomial distribution
#' @param dist a bindist variable
#' @export
#' @examples
#' dis1 <- bin_distribution(trials = 5, prob = 0.5)
#' plot(dis1)
plot.bindis <- function(dist) {
  barplot(dist$probability, names.arg=dist$success)
}

#' @title Cumulative binomial distribution
#' @description Computes the binomial distribution while keeping track of the cumulative probability
#' @param trials number of trials to attempt
#' @param prob probability of success
#' @return returns a bincum variable which is a data.frame containing the number of successes, the probability of that success, and the cumulative probability up to that point.
#' @export
#' @examples
#' bin_cumulative(trials = 5, prob = 0.5)
bin_cumulative <- function(trials, prob) {
  dist <- data.frame("success" = 0:trials, "probability" = rep(0,trials + 1),"cumulative" = rep(0,trials + 1))
  for (i in 0:trials) {
    dist[i + 1,]$probability <- bin_probability(i, trials, prob)
    if(i > 0) {
      dist[i + 1,]$cumulative <- dist[i,]$cumulative + dist[i + 1,]$probability
    } else {
      dist[i + 1,]$cumulative <- dist[i + 1,]$probability
    }
  }
  class(dist) <- c("bincum", "data.frame")
  return(dist)
}

#' @title Plot cumulative binomial distribution
#' @description Plots a line of the cumulative binomial distribution
#' @param cum the cumulative binomial variable bincum
#' @export
#' @examples
#' dis1 <- bin_cumulative(trials = 5, prob = 0.5)
#' plot(dis1)
plot.bincum <- function(cum) {
  plot(cum$success, cum$cumulative, type="o", xlab="successes", ylab="probability")
}

#' @title Binomial random variable
#' @description Creates a binomial random variable with trials and prob as parameters
#' @param trials number of trials to attempt
#' @param prob probability of success
#' @return returns a binvar variable that represents the binomial random variable
#' @export
#' @examples
#' bin_variable(10, 0.3)
bin_variable <- function(trials, prob) {
  var <- data.frame("trials" = trials, "prob" = prob)
  class(var) = c("binvar", "data.frame")
  return(var)
}

#' @title Print binomial random variable
#' @description The print representation of a binomial random variable
#' @param bin binvar that represents a binomial random variable
#' @export
#' @examples
#' bin1 <- bin_variable(trials = 10, p = 0.3)
#' bin1
print.binvar <- function(bin) {
  print("Binomial variable")
  print("", quote = FALSE)
  print("Parameters", quote = FALSE)
  num_trials <- paste('- number of trials:', bin$trials)
  print(num_trials , quote = FALSE)
  prob_succ <- paste("- prob of success :", bin$prob)
  print(prob_succ , quote = FALSE)
}

#' @title Binomial random variable summary
#' @description computes summary of the binomial random variable passed in.
#' @param bin_var binvar that represents a binomial random variable
#' @export
#' @examples
#' bin1 <- bin_variable(trials = 10, p = 0.3)
#' summary(bin1)
summary.binvar <- function(bin_var) {
  binvar_mean <- aux_mean(bin_var$trials, bin_var$prob)
  binvar_var <- aux_variance(bin_var$trials, bin_var$prob)
  binvar_mode <- aux_mode(bin_var$trials, bin_var$prob)
  binvar_skewness <- aux_skewness(bin_var$trials, bin_var$prob)
  binvar_kurtosis <- aux_kurtosis(bin_var$trials, bin_var$prob)
  sum_var <- list("trials"=bin_var$trials,
                  "prob" = bin_var$prob,
                  "mean" = binvar_mean,
                  "variance" = binvar_var,
                  "mode" = binvar_mode,
                  "skewness" = binvar_skewness,
                  "kurtosis" = binvar_kurtosis)
  class(sum_var) = c("summary.binvar", "list")
  return(sum_var)
}

#' @title Print binomial random variable summary
#' @description The print representation of a binomial random variable summart
#' @param sum_bin_var summary.binvar that represents a binomial random variable
#' @export
#' @examples
#' bin1 <- bin_variable(trials = 10, p = 0.3)
#' bin_sum <- summary(bin1)
#' bin_sum
print.summary.binvar <- function(sum_bin_var) {
  print("Summary Binomial")
  print("", quote = FALSE)
  print("Parameters", quote = FALSE)
  num_trials <- paste('- number of trials:', sum_bin_var$trials)
  print(num_trials , quote = FALSE)
  prob_succ <- paste("- prob of success :", sum_bin_var$prob)
  print(prob_succ , quote = FALSE)
  print("", quote = FALSE)
  print("Measures", quote = FALSE)
  binvar_mean <- paste("- mean    :", sum_bin_var$mean)
  print(binvar_mean, quote=F)
  binvar_variance <- paste("- variance:", sum_bin_var$variance)
  print(binvar_variance, quote=F)
  binvar_mode <- paste("- mode    :", sum_bin_var$mode)
  print(binvar_mode, quote=F)
  binvar_skew <- paste("- skewness:", sum_bin_var$skewness)
  print(binvar_skew, quote=F)
  binvar_kurtosis <- paste("- kurtosis:", sum_bin_var$kurtosis)
  print(binvar_kurtosis, quote=F)
}

#' @title Binomial mean
#' @description Computes the mean of the binomial distribution
#' @param trials number of trials to attempt
#' @param prob probability of success
#' @return returns a the mean of the binomial distribution
#' @export
#' @examples
#' bin_mean(10, 0.3)
bin_mean <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_mean(trials,prob))
}

#' @title Binomial variance
#' @description Computes the variance of the binomial distribution
#' @param trials number of trials to attempt
#' @param prob probability of success
#' @return returns a the variance of the binomial distribution
#' @export
#' @examples
#' bin_variance(10, 0.3)
bin_variance <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_variance(trials,prob))
}

#' @title Binomial mode
#' @description Computes the mode of the binomial distribution
#' @param trials number of trials to attempt
#' @param prob probability of success
#' @return returns a the mode of the binomial distribution
#' @export
#' @examples
#' bin_mode(10, 0.3)
bin_mode <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_mode(trials,prob))
}

#' @title Binomial skewness
#' @description Computes the skewness of the binomial distribution
#' @param trials number of trials to attempt
#' @param prob probability of success
#' @return returns a the skewness of the binomial distribution
#' @export
#' @examples
#' bin_skewness(10, 0.3)
bin_skewness <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_skewness(trials,prob))
}

#' @title Binomial kurtosis
#' @description Computes the kurtosis of the binomial distribution
#' @param trials number of trials to attempt
#' @param prob probability of success
#' @return returns a the kurtosis of the binomial distribution
#' @export
#' @examples
#' bin_kurtosis(10, 0.3)
bin_kurtosis <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_kurtosis(trials,prob))
}
