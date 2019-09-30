#' Computes all the chosen transformations to the input data frame in the chosen
#' parameters.
#'
#' @param data A data frame
#' @param subset a specification of the rows to be used: defaults to all rows.
#'   This can be any valid indexing vector (see \link{[.data.frame}) for the
#'   rows of data or if that is not supplied, a data frame made up of the
#'   variables used in \code{formula}.
#' @param select expression, indicating columns to select from a data frame.
#'   Defaults for all the variables in data. See \link{subset}.
#' @param transf The chosen transformations to be applied to the data
#' @return A list with all data transformed according to the arguments
#'   passed.help
#' @export
#' @examples
#'
#' vars <- colnames(centro_2015@data)
#' alltransf(centro_2015@data, select = vars)
#' alltransf(centro_2015@data, 1:10, c("valor", "area_total"))
#'

alltransf <- function(data, subset, select = colnames(data),
                      transf = c('rsqrt', 'log', 'sqrt')){

  if (missing(subset)) subset <- seq_len(nrow(data))
  df <- as.data.frame(data[subset, colnames(data) %in% select])
  df <- stats::na.omit(df)

  for (i in colnames(df)) if (is.character(df[,i])) df[,i] <- as.factor(df[,i])

  factors <- plyr::colwise(is.factor)(df)

  transformations <- list("rsqr"     = rsqr,
                          "rec"      = rec,
                          "rsqrt"    = rsqrt,
                          "log"      = log,
                          "sqrt"     = sqrt,
                          "identity" = identity,
                          "sqr"      = sqr)

  transfers <- c("identity", transf)
  transfs <- transformations[transfers]

  t_list <- lapply(transfs, function(f) f(df[,factors == FALSE]))
  return(t_list)
}