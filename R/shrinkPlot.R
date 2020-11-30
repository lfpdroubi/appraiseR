#' Create plots to show prediction shrinkage
#'
#' Computes fitted values from lm package and plots aside of observed values
#'
#' @param object object of class \code{\link{bestfit}}
#' @param func function used to transform the response (defaults to identity)
#' @return a shrinkage prediction plot
#' @examples
#' library(sf)
#' dados <- st_drop_geometry(centro_2015)
#' fit0 <- lm(valor ~ 1, dados)
#' powerPlot(fit0, axis = "inverted")
#' shrinkPlot(fit0)
#' dados$padrao <- as.numeric(dados$padrao)
#' fit <- lm(log(valor)~area_total + quartos + suites + garagens +
#' log(dist_b_mar) + I(1/padrao), data = dados, subset = -c(31, 39))
#' shrinkPlot(fit)
#' shrinkPlot(fit, func = "log")
#'@export
#'
shrinkPlot <- function(object, func = "identity", ...) {
  z <- object
  mf <- stats::model.frame(z)
  response <- colnames(mf)[attr(z$terms, "response")]
  dat <- broom::augment(z)
  dat <- reshape2::melt(dat,
                 measure.vars = c(response, ".fitted"),
                 value.name = "Valor")
  dat$variable <- factor(dat$variable, labels = c("Y", "Yajustado"))
  dat$Valor <- inverse(dat$Valor, func = func)
  p <- ggplot(dat, aes(x= variable, y = Valor))  +
    geom_violin(trim = FALSE) +
    geom_dotplot(binaxis='y', stackdir='center', dotsize = .8, alpha = 0.5) +
    # stat_summary(fun.y=median, geom="point", shape=18,
    #                size=3, color="red")
    # scale_fill_manual(values=c("#999999", "#1C00ff00")) +
    stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),
                 geom="pointrange", color="red") +
    theme(legend.position = "none")
  return(p)
}
