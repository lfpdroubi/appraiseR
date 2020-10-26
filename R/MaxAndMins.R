#' MaxsAndMins
#'
#' Find out how much new data extrapolates the model data
#'
#' @param newdata a tibble with new data, *i.e.* prediction data
#' @param data a data frame
#'
#' @return a data frame with the newdata values above max or below min original data
#' @export
#'
#' @examples
#' dados <- st_drop_geometry(centro_2015)
#' fit <- lm(log(valor) ~ ., data = dados)
#' newdata <- new_data(fit)
#' MaxsAndMins(newdata, dados)
#' newdata[1, "area_total"] <- 600
#' newdata[2, "dist_b_mar"] <- 30
#' MaxsAndMins(newdata, dados)
#'
MaxsAndMins <- function(newdata, data) {
  maxs <-
    data %>%
    dplyr::select(colnames(newdata)) %>%
    dplyr::select_if(is.numeric) %>%
    dplyr::summarize_all(max, na.rm = TRUE) %>%
    unlist()
  mins <-
    data %>%
    dplyr::select(colnames(newdata)) %>%
    dplyr::select_if(is.numeric) %>%
    dplyr::summarize_all(min, na.rm = TRUE) %>%
    unlist()
  df <- list()
  for (i in seq_len(dim(newdata)[1])){
    x <-
      newdata %>%
      dplyr::select_if(is.numeric) %>%
      dplyr::slice(!!i) %>%
      unlist()
    df[[i]] <- x - pmin(x, maxs) + x - pmax(x, mins)
  }
  return(dplyr::bind_rows(!!!df))
}
