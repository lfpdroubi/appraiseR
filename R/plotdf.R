#' Plot data frames
#'
#' \code{plotdf} creates standardized plots for easy data frames visualizations.
#'
#' @param formula a generic formula.
#' @param data a data frame.
#'
#' @export
#' @examples
#' dados <- st_drop_geometry(centro_2015)
#' plotdf(valor ~ ., dados)
#' plotdf(log(valor) ~ ., dados)
#' plotdf(log(valor) ~ area_total + quartos + suites + garagens +
#' log(dist_b_mar) + padrao , dados)

plotdf <- function(formula, data){
  data <- as.data.frame(data)
  mf <- stats::model.frame(formula = formula, data = data)

  predictors <- attr(stats::terms.formula(x = formula, data = data), "term.labels")
  response <-
    colnames(mf)[attr(stats::terms.formula(x = formula, data = data), "response")]
  parameters <- union(response, predictors)

  r <- length(predictors)
  par1 <- round(sqrt(r))
  par2 <- ceiling(r/par1)
  p <- list()

  for (i in predictors) {
    if (is.character(mf[, i]) | is.factor(mf[, i]))
      p[[i]] <- bboxplot(y = response, g = i, data = mf)+
        scale_y_continuous(labels = scales::label_number_si(accuracy = .01,
                                                            big.mark = ".",
                                                            decimal.mark = ","))
    else
      p[[i]] <- ggplot(mf, aes_(x = as.name(i), y = as.name(response))) +
                         geom_point() +
                         stat_smooth(method = "lm", se = F)+
        scale_y_continuous(labels = scales::label_number_si(accuracy = .01,
                                                            big.mark = ".",
                                                            decimal.mark = ",")) +
        scale_x_continuous(labels = scales::label_number_si(accuracy = .01,
                                                            big.mark = ".",
                                                            decimal.mark = ","))
  }

  est <- list(plots = p,
              par1 = par1,
              par2 = par2)
  class(est) <- "plotdf"
  est
}

#' @export
#'
print.plotdf <- function(x, ...){
  cowplot::plot_grid(plotlist = x$plots, labels = "auto", nrow = x$par1)
}