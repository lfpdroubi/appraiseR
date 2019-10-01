#' Computes the depreciation parameter according to the Ross-Heidecke method
#'
#' @param VU lifespan
#' @param IA apparent age
#' @param Conservacao Conservation state
#' @return The Ross-Heidecke depreciation parameter
#' @export
#' @examples
#' rossheidecke(30, 0, "Novo")
#' rossheidecke(30, 10, "N")
#' rossheidecke(30, 20, "N")
#' rossheidecke(30, 30, "N")
#'
#' rossheidecke(30, 0, "Reparos simples")
#' rossheidecke(30, 10, "Reparos simples")
#' rossheidecke(30, 20, "Reparos simples")
#' rossheidecke(30, 30, "Reparos simples")

rossheidecke <- function(VU, IA, Conservacao = c("Novo", "Entre novo e regular", "Regular",
                                                 "Entre regular e reparos simples", "Reparos simples",
                                                 "Entre reparos simples e importantes", "Reparos importantes",
                                                 "Entre reparos importantes e sem valor", "Sem valor")) {
  Conservacao <- match.arg(Conservacao)
  alpha <- .5*(IA/VU+IA^2/VU^2)
  x <- ifelse(Conservacao == "Novo", 0,
              ifelse(Conservacao == "Entre novo e regular", 0.32,
                     ifelse(Conservacao == "Regular", 2.52,
                            ifelse(Conservacao == "Entre regular e reparos simples", 8.09,
                                   ifelse(Conservacao == "Reparos simples", 18.10,
                                          ifelse(Conservacao == "Entre reparos simples e importantes", 33.20,
                                                 ifelse(Conservacao == "Reparos importantes", 52.60,
                                                        ifelse(Conservacao == "Entre reparos importantes e sem valor", 75.20,
                                                               100))))))))
  k <- alpha + (1- alpha)*x/100
  1 - k
}