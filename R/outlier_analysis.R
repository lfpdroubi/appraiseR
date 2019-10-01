#' Sample outlier analysis
#'
#' Performs sample outlier analysis according to chosen criteria.
#'
#' @param x a vector of class numeric.
#' @param criterion criterion to be used.
#' @return data
#' @examples
#' outlier_analysis(x = trindade, criterion = "30_percent")
#' outlier_analysis(x = trindade, criterion = "2_sd")
#' outlier_analysis(x = trindade, criterion = "chauvenet")
#' @export
outlier_analysis <- function(x, criterion = c("30_percent",
                                              "2_sd",
                                              "chauvenet")) {

  #if ()  print("Nonexistent or not implemented criterion")
  y <- list()
  y[[1]] <- unlist(x, use.names = FALSE) %>% stats::na.omit()

  # Dados iniciais
  y_med <- mean(y[[1]])
  y_sd <- stats::sd(y[[1]])
  z <- stats::qnorm(1 - 1/(4 * length(y[[1]])))
  i <- 1

  if (criterion == "30_percent") {
    y[[i + 1]] <- y[[i]][ y[[i]] >= 0.7 * y_med & y[[i]] <= 1.3 * y_med ]
    out <- setdiff(y[[i]], y[[i + 1]])
    message(sprintf("removing values %s", paste0(out, collapse = ", ")))
    return(y[[i + 1]])
  } else if (criterion == "2_sd") {
    while (any(y[[i]] < y_med - 2 * y_sd | y[[i]] > y_med + 2 * y_sd)) {
      y[[i + 1]] <- y[[i]][ y[[i]] >= y_med - 2 * y_sd & y[[i]] <= y_med + 2 * y_sd ]
      out <- setdiff(y[[i]], y[[i + 1]])
      message(sprintf("Step %i: removing values %s", i, paste0(out, collapse = ", ")))
      y_med <- mean(y[[i + 1]])
      y_sd <- stats::sd(y[[i + 1]])
      i <- i + 1
    }
    return(y[[i]])

  } else if (criterion == "chauvenet") {
    while (any(y[[i]] < y_med - y_sd * z | y[[i]] > y_med + y_sd * z)) {
      y[[i + 1]] <- y[[i]][ y[[i]] >= y_med - y_sd * z & y[[i]] <= y_med + y_sd * z ]
      out <- setdiff(y[[i]], y[[i + 1]])
      message(sprintf("Step %i: removing values %s", i, paste0(out, collapse = ", ")))
      y_med <- mean(y[[i + 1]])
      y_sd <- stats::sd(y[[i + 1]])
      z <- stats::qnorm(1 - 1/(4 * length(y[[i + 1]])))
      i <- i + 1
    }
    return(y[[i]])
  }
}