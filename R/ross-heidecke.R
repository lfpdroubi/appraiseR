#' Computes Depreciation parameters according to several methods
#'
#' @name depreciation
#' @export
#' @examples
#' linhaReta(60, 25, .20)
#'
linhaReta <- function(VidaUtil, IdadeAparente, ValorResidual = 0){


  VU <- VidaUtil
  IA <- IdadeAparente
  VR <- ValorResidual
  PD <- 1 - VR
  k <- PD*(VU - IA)/VU + VR
  k
}

#' @name depreciation
#' @export
#' @examples
#' linhaRetaVariante(0, .07, 5)
#' linhaRetaVariante(5, .07, 5)
#' linhaRetaVariante(10, .07, 5)
#' linhaRetaVariante(15, .07, 5)
#' linhaRetaVariante(20, .07, 5)
#' linhaRetaVariante(25, .07, 5)
#'
linhaRetaVariante <- function(IdadeReal, taxa, periodo){

  IR <- IdadeReal
  k <- taxa*(1 + (IR - periodo)/periodo)
  1 - k
}

#' @name depreciation
#' @export
#' @examples
#'
#'
kuentzle <- function(VidaUtil, IdadeAparente){

  VU <- VidaUtil
  IA <- IdadeAparente
  k <- (VU^2 - IA^2)/VU^2
  k
}

#' @name depreciation
#' @param r depreciation rate
#' @export
#' @examples
#' # Residencias proletario rustico a medio comercial
#' valorDecrescente(25, r = 0.015)
#' valorDecrescente(67, r = 0.015)
#'
#' # Residencias medio superior a luxo
#' valorDecrescente(25, r = 0.02)
#' valorDecrescente(50, r = 0.02)
#' valorDecrescente(50, r = 0.10)
#'
valorDecrescente <- function(VidaUtil, r){

  VU <- VidaUtil
  k <- (1-r)^VU
  k
}

#' @name depreciation
#' @param VidaUtil lifespan
#' @param IdadeAparente apparent age
#' @return The Ross depreciation parameter
#' @export
#' @examples
#' ross(50, 10)
#'
ross <- function(VidaUtil, IdadeAparente){
  VU <- VidaUtil
  IA <- IdadeAparente
  alpha <- .5*(IA/VU+IA^2/VU^2)
  1 - alpha
}

#' @name depreciation
#' @param Conservacao Conservation state
#' @return The Heidecke depreciation parameter
#' @export
#' @examples
#' heidecke(Conservacao = "Novo")
#' heidecke("Entre novo e regular")
#' heidecke("Regular")
#' heidecke("Entre regular e reparos simples")
#' heidecke("Reparos simples")
#' heidecke("Entre reparos simples e importantes")
#' heidecke("Reparos importantes")
#' heidecke("Entre reparos importantes e sem valor")
#' heidecke("Sem valor")
#'
heidecke <- function(Conservacao = c("Novo", "Entre novo e regular", "Regular",
                                     "Entre regular e reparos simples",
                                     "Reparos simples",
                                     "Entre reparos simples e importantes",
                                     "Reparos importantes",
                                     "Entre reparos importantes e sem valor",
                                     "Sem valor")){
  Conservacao <- match.arg(Conservacao)
  C <- switch(Conservacao,
              "Novo" = 0,
              "Entre novo e regular" = 0.32,
              "Regular" = 2.52,
              "Entre regular e reparos simples" = 8.09,
              "Reparos simples" = 18.10,
              "Entre reparos simples e importantes" = 33.20,
              "Reparos importantes" = 52.60,
              "Entre reparos importantes e sem valor" = 75.20,
              "Sem valor" = 100
  )
  1 - C/100
}

#' @name depreciation
#' @param ValorResidual residual value, in percentage
#' @return The Ross-Heidecke depreciation parameter
#' @export
#' @examples
#' rossheidecke(30, 0, Conservacao = "Novo")
#' rossheidecke(30, 10,  Conservacao = "N")
#' rossheidecke(30, 20,  Conservacao = "N")
#' rossheidecke(30, 30,  Conservacao = "N")
#'
#' rossheidecke(30, 0, Conservacao = "Reparos simples")
#' rossheidecke(30, 10, Conservacao = "Reparos simples")
#' rossheidecke(30, 20, Conservacao = "Reparos simples")
#' rossheidecke(30, 30, Conservacao = "Reparos simples")
#'
#' rossheidecke(50, 20, Conservacao = "Reparos importantes")
#' rossheidecke(50, 20, .18, "Reparos importantes")
#'
#' rossheidecke(60, 25, Conservacao = "Reparos importantes")
#' rossheidecke(60, 25, .20, Conservacao = "Reparos importantes")
#'
#' # Curva de Depreciacao
#' plot(NULL, xlim = c(0, 50), ylim = c(0, 1), las = 2,
#' xlab = "Idade Aparente",
#' ylab = "Coeficiente de Depreciacao",
#' main = "Depreciação de um bem")
#' curve(linhaReta(50, x), from = 1, to = 50, add = T)
#'
#' curve(kuentzle(50, x), from = 1, to = 50, add = T, col = "green")
#'
#' curve(valorDecrescente(x, r = .10), from = 1, to = 50, add = T, col ="blue")
#'
#' curve(ross(50, x), add = T, col = "red")
#'
#' curve(rossheidecke(50, x, Conservacao = "Regular"),
#'        add = T, col = "orange")
#'
#' legend("topright", inset = .02,
#'       title = "Metodo",
#'       legend=c("Valor Decrescente", "Linha Reta", "Ross-Heideck (Regular)",
#'                "Ross", "Kuentzle"), text.font = 4,
#'       col=c("blue", "black", "orange", "red", "green"), cex=0.8, lty = 1)

rossheidecke <- function(VidaUtil, IdadeAparente, ValorResidual = 0,
                         Conservacao = c("Novo", "Entre novo e regular", "Regular",
                                         "Entre regular e reparos simples",
                                         "Reparos simples",
                                         "Entre reparos simples e importantes",
                                         "Reparos importantes",
                                         "Entre reparos importantes e sem valor",
                                         "Sem valor")) {
  VU <- VidaUtil
  IA <- IdadeAparente
  Vd <- 1 - ValorResidual
  Conservacao <- match.arg(Conservacao)
  alpha <- 1 - ross(VU, IA)
  C <- 1 - heidecke(Conservacao)
  k <- (alpha + (1- alpha)*C)*Vd
  1 - k
}

