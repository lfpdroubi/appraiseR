#' @export
#'
print.bestfit <- function(x, n = 10, ...){
  est <- list()
  est$call <- x$call
  est$tab <- x$tab

  cat("Call:\n")
  print(est$call)

  cat("\nBest", n, "fits:\n")
  print(est$tab[1:n,])
  cat("...")
}