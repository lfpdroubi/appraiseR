#' Beautiful boxplots with ggplot2.
#'
#' Create boxplots with \link{ggplot2}.

#' @param y a numeric variable for which the boxplot is to be constructed.
#' @param \dots further arguments passed to \code{\link[ggplot2]{geom_boxplot}}.
#' @export
bboxplot <- function(y, ...) UseMethod("bboxplot")


#' @param g a grouping variable, usually a factor, for constructing parallel
#'   boxplots.
#' @param data a dataframe containing the variables.
#'
#' @return A boxplot of the variable \code{y} or the boxplot of the \code{y}
#'   against \code{g}.
#'
#' @examples
#' dados <- st_drop_geometry(centro_2015)
#' bboxplot("valor", data = dados)
#' bboxplot("valor", "padrao", dados)
#' @rdname bboxplot
#' @export

bboxplot.default <- function(y, g, data, ...) {
  y <- as.name(y)
  x <- 1
  if (missing(g)) g <- rlang::enquo(x) else g <- as.name(g)

  data %<>% tibble::rowid_to_column()

  is_outlier <- function(x) {
    low <- stats::quantile(x, 0.25) - 1.5 * stats::IQR(x)
    up <- stats::quantile(x, 0.75) + 1.5 * stats::IQR(x)
    return(x < low | x > up)
  }

  p <-
    data %>%
    stats::na.omit() %>%
    dplyr::group_by(!!g) %>%
    dplyr::mutate(outlier = ifelse(is_outlier(!!y), rowid, as.numeric(NA))) %>%
    ggplot(aes_(x = g, y = y)) +
    geom_boxplot(aes_(fill = g), outlier.colour = "red", ...) +
    geom_text(aes_(label = ~outlier), na.rm = TRUE, hjust = -0.3) +
    theme(axis.title.x = element_blank(), legend.position = "bottom")# + geom_jitter(width = 0.2)
  p
}


#' @param formula A model formula of the form ~ y to produce a boxplot for
#'   the variable y, or of the form y ~ g to produce parallel boxplots for y
#'   within levels of the grouping variable(s) g, etc., usually factors.
#' @examples
#' bboxplot(valor~padrao, data)
#' bboxplot(~valor, data)
#' @rdname bboxplot
#' @export
#'
bboxplot.formula <- function(formula, data, ...) {
  response <- attr(stats::terms.formula(formula, data = data),
                   "response")
  var <- attr(stats::terms.formula(formula, data = data),
              "term.labels")

  if (response == 0) {
    p <- bboxplot.default(y = var, data = data)
  } else {
    response <- colnames(data)[response]
    p <- bboxplot.default(y = response,
                          g = var,
                          data = data
    )
  }
  return(p)
}