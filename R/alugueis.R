#' Rent calculation
#'
#' Computes the rent of an entire building
#'
#' @param qt lot value per square meter
#' @param qb built-up value per square meter
#' @param S lot Area
#' @param A built-up area
#' @param FC factor to be applied to land and built-areas values in order to
#' obtain the real estate value
#' @param Fd depreciation multiplicator
#' @param CA Land Use Coefficient
#' @param it capitalization rate to be applied to the land value
#' @param ib capitalization rate to be applied to the built-up area value
#' @return The rent value for the real estate
#' @export
#' @examples
#' rent(qt = 2000, qb = 1200, S = 400, A = 450, FC = 1.1, Fd = 0.8, CA = 0.9375,
#'      it = 8/100, ib = 8/100)
rent <- function(qt, qb, S, A, FC, Fd, CA, it, ib){
  Vt <- qt*S*CA
  Vb <- qb*A*Fd
  Vi <- FC*(Vt+Vb)
  it <- (1+it)^(1/12)-1
  ib <- (1+ib)^(1/12)-1
  i <- (it*Vt + ib*Vb)/(Vt+Vb)
  Vi*i
}
#'
#' @rdname rent
#' Unity rent
#'
#' Computes the rent of a single unity
#'
#' @export
#' @examples
#' # Rent of the ground floor area
#'
#' urent(qt = 2000, A = 150, wi = 3, weights = c(3, 1), FAR = 0.5, CA = 1,
#'       qb = 1200, Fd = 0.8, FC = 1.1, it = 8/100, ib = 8/100)
#'
#' # Rent of the second floor
#'
#' urent(qt = 2000, A = 300, wi = 1, weights = c(3, 1), FAR = 0.5, CA = 1,
#'       qb = 1200, Fd = 0.8, FC = 1.1, it = 8/100, ib = 8/100)
#'
#' # Rent of single unity on the ground floor
#'
#' urent(qt = 2000, A = 50, wi = 3, weights = c(3, 1), FAR = 0.5, CA = 1,
#'       qb = 1200, Fd = 0.8, FC = 1.1, it = 8/100, ib = 8/100)
#'
#' # Rent of single unity on the second floor
#'
#' urent(qt = 2000, A = 150, wi = 1, weights = c(3, 1), FAR = 0.5, CA = 1,
#'       qb = 1200, Fd = 0.8, FC = 1.1, it = 8/100, ib = 8/100)
urent <- function(qt, A, wi, weights, FAR, CA, qb, Fd, FC, it, ib){

  w <- weights

  # Computes Capital invested in land and built-up area
  CTu <- qt*A*wi/(FAR*w[1]+(CA-FAR)*w[-1])
  Cb <- A*qb*Fd

  # Computes total capital invested in the real estate
  Ci <- FC*(CTu + Cb)

  # rates
  it <- (1+it)^(1/12)-1
  ib <- (1+ib)^(1/12)-1
  i <- (it*CTu + ib*Cb)/(CTu+Cb)

  # Computes the rent of the unity
  Ci*i
}
#'
#' @rdname rent
#' Land Use Coefficient
#'
#' @param GroundFloorArea The ground floor built-up area
#' @param FloorAreas The built-up area on other floors
#' @param weights A vector of the weights of each floor built-up area, including
#' the weight for the ground floor area
#' @param S Lot area
#' @param FAR the Floor area ratio
#' @param CA the maximum allowable built-up area
#' @return The effective land use coefficient of the built-up area
#' @export
#' @examples
#' GFArea <- 150
#' SecondFloorArea <- 300
#' w <- c(3, 1)
#' S <- 400
#' FAR <- 0.5
#' CA <- 1
#' LUC(GFArea, SecondFloorArea, w, S, FAR, CA)
LUC <- function(GroundFloorArea, FloorAreas, weights, S, FAR, CA) {
  GFArea <- GroundFloorArea
  w <- weights
  Ahe <- GFArea*w[1] + FloorAreas*w[-1]
  Ahp <- S*FAR*w[1] + (CA-FAR)*w[-1]*S
  Coef <- Ahe/Ahp
  Coef
}
