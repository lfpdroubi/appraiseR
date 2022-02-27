#' Real States appraisals based in the residual method
#'
#' @param vgv Overall Sales Value
#' @param cc Constructions Costs
#' @param bdi_c Constructors Budget Difference Income
#' @param bdi_i Developers Budget Difference Income
#' @param cor Brokerage fee
#' @export
involutivo_estatico <- function(vgv, cc, bdi_c, bdi_i, cor){
  valor_terreno <- vgv - cc - bdi_c*cc - bdi_i*vgv - cor*vgv
  valor_terreno
}

#' Building Area
#'
#' @param area_terreno land area
#' @param IA rate of land use
#' @param np floors
#' @return the maximum building area
#' @examples
#' ac <- AREA_CONSTRUIDA(630, 2.5, 5)
#' @export

AREA_CONSTRUIDA <- function(area_terreno, IA, np){
  area_construida <- area_terreno*IA/np*(np+1)
  area_construida
}


#' Overall Sales Value calculation
#'
#' @param area_construida Building area (sq. meters)
#' @param valor_unitario unit selling value ($/sq. meters)
#' @return The Overall Sales Value
#' @examples
#' vgv <- VGV(20*94.5, 7000)
#' @export

VGV <- function(area_construida, valor_unitario){
  vgv <- area_construida*valor_unitario
  vgv
}


#' Investment cash flow
#'
#' @param cc construction cost
#' @param wc vector of weights of the total constructions costs over periods
#' @param bdi_c Constructors Budget Difference Income
#' @param index a vector with the periods in which are made the expenses
#' @return a vector containing the expected investment cash flow
#' @examples
#' wc <- c(5.67, 6.63, 7.24, 7.55, 10.76, 13.26, 14.72, 13.16, 14.18, 6.84)/100
#' bdi_c <- 31.46/100
#' cc <-  3523496.76
#' fci_provavel <- FCI(cc = cc, wc = wc, bdi_c = bdi_c)
#' @export

FCI <- function(cc, wc, bdi_c, index = seq(0, length(wc) - 1)){
  fci <- tibble::tibble(Periodo = index, FCI = -wc*cc*(1 + bdi_c))
  fci
}


#' Sales Cash Flow
#'
#' @param vgv overall selling value
#' @param w vector of weights of the total expected sales over periods
#' @return a vector containing the expected sales cash flow
#' @examples
#' wv <- c(0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2)/20
#' fcv_provavel <- FCV(vgv = vgv, wv = wv)
#' @export

FCV <- function(vgv, wv, index = seq(0, length(wv) - 1)){
  vendas <- tibble::tibble(Periodo = index, FCV = wv*vgv)
  vendas
}


#' Net Cash Flow
#'
#' @param fcv the expected sales cash flow
#' @param fci the expected investment cash flow
#' @param bdi_i Developers Budget Difference Income
#' @param cor Brokerage fee
#' @return the expected Net Cash Flow
#' @examples
#' bdi_i <- 23.5223/100
#' cor <- 5/100
#' fcl_provavel <- FCL(fcv_provavel, fci_provavel, bdi_i = bdi_i, cor = cor)
#' @export

FCL <- function(fcv, fci, bdi_i, cor){
  fcl <- dplyr::full_join(fcv, fci, by = "Periodo")
  fcl[is.na(fcl)] <- 0
  fcl %<>%
    dplyr::mutate(COR = -FCV*cor, BDI = -FCV*bdi_i, FCL = FCV + COR + BDI + FCI)
  fcl %>% dplyr::select(Periodo, FCL)
}


#' Cash Flow table
#'
#' @examples
#' tma <- 1.91/100
#' FC(fcv_provavel, fci_provavel, bdi_i = bdi_i, cor = cor, tma = tma)
#' @export

FC <- function(fcv, fci, bdi_i, cor, tma){
  fc <- dplyr::full_join(fcv, fci, by = "Periodo")
  fc[is.na(fc)] <- 0
  fc %>%
    dplyr::mutate(Corretagem = -FCV*cor,
                  BDI_Incorporador = -bdi_i*FCV,
                  FCL = FCV + Corretagem + BDI_Incorporador + FCI,
                  fator_VP = 1/(1 + tma)^Periodo,
                  FCL_descontado = FCL*fator_VP)
}


#' Net Present Value
#'
#' @param fcl the Net Cash Flow of the project
#' @param tma the attractiveness rate
#' @return the net present value of the project
#' @examples
#' vpl_provavel <- VPL(fcl_provavel, tma = tma)
#' @export

VPL <- function(fcl, tma){
  fcl %<>%
    dplyr::mutate(fator_VP = 1/(1 + tma)^Periodo,
                  VP = FCL*fator_VP)
  sum(fcl$VP)
}

## Análise de sensibilidade


#' Sensibilidade aa TMA
#'
#' @examples
#' range_tma <- c(1.2, 2.6)/100
#' s_tma <- sensibilidade_tma(range_tma, fcl_provavel)
#' s_tma
#'
#' require(ggplot2)
#' ggplot(s_tma, aes(x = TMA, y = VPL)) +
#'   geom_line() + geom_point() +
#'   scale_x_continuous(labels = scales::percent) +
#'   scale_y_continuous(labels = scales::dollar) +
#'   labs(title = "Sensibilidade à TMA")
#' @export

sensibilidade_tma <- function(range, fcl){
  tma <- tibble::tibble(TMA = c(range[1], mean(range), range[2]))
  tma$VPL <- apply(tma, 1, VPL, fcl = fcl)
  tma <- dplyr::arrange(tma, VPL)
  tma$Situacao <- c("Pessimista", "Provavel", "Otimista")
  tma$Variacao <- c(tma$VPL[1]/tma$VPL[2] - 1, 0, tma$VPL[3]/tma$VPL[2] - 1)
  tma <- tma[c("Situacao", "TMA", "VPL", "Variacao")]
  tma
}


#' Sensibilidade-custo do VPL
#'
#' @examples
#' range_custos <- c(.9, 1.1)
#' s_custo <- sensibilidade_custo(range = range_custos,
#'                                cc = cc, wc = wc,
#'                                vgv = vgv, wv = wv,
#'                                bdi_i = bdi_i, bdi_c = bdi_c,
#'                                cor = cor, tma = tma)
#' s_custo
#'
#' ggplot(s_custo, aes(x = CC, y = VPL)) +
#'   geom_line() + geom_point() +
#'   scale_x_continuous(labels = scales::dollar) +
#'   scale_y_continuous(labels = scales::dollar) +
#'   labs(title = "Sensibilidade à variação dos Custos de Construção")
#' @export

sensibilidade_custo <- function(range, cc, wc, vgv, wv, bdi_i, bdi_c, cor, tma){
  cc <- tibble::tibble(CC = c(range[1], mean(range), range[2])*cc)
  fci <- apply(cc, 1, FCI,  wc, bdi_c)
  fcv <- FCV(vgv, wv)
  fcl <- lapply(fci, FCL, fcv = fcv, bdi_i = bdi_i, cor = cor)
  cc$VPL <- sapply(fcl, VPL, tma = tma, simplify = TRUE)
  cc$Situacao <- c("Pessimista", "Provavel", "Otimista")
  cc$Variacao <- c(cc$VPL[1]/cc$VPL[2] - 1, 0, cc$VPL[3]/cc$VPL[2] - 1)
  cc <- cc[c("Situacao", "CC", "VPL", "Variacao")]
  cc
}


#' Sensibilidade-venda do VPL
#'
#' @examples
#' range_vgv <- c(.9, 1.1)
#' s_vgv <- sensibilidade_venda(range = range_vgv,
#'                                cc = cc, wc = wc,
#'                                vgv = vgv, wv = wv,
#'                                bdi_i = bdi_i, bdi_c = bdi_c,
#'                                cor = cor, tma = tma)
#' s_vgv
#' ggplot(s_venda, aes(x = Vendas, y = VPL)) +
#'   geom_point() + geom_line() +
#'   scale_x_continuous(labels = scales::dollar) +
#'   scale_y_continuous(labels = scales::dollar) +
#'   labs(title = "Sensibilidade à variação dos Preços de Venda")
#' @export

sensibilidade_venda <- function(range, cc, wc, vgv, wv, bdi_i, bdi_c, cor, tma){
  v <- data.frame(Vendas = c(range[1], mean(range), range[2])*vgv)
  fcv <- apply(v, 1, FCV, w = wv)
  fci <- FCI(cc, wc, bdi_c = bdi_c)
  fcl <- lapply(fcv, FCL, fci = fci, bdi_i = bdi_i, cor = cor)
  v$VPL <- sapply(fcl, VPL, tma = tma, simplify = TRUE)
  v$Situacao <- c("Pessimista", "Provavel", "Otimista")
  v$Variacao <- c(v$VPL[1]/v$VPL[2] - 1, 0, v$VPL[3]/v$VPL[2] - 1)
  v <- v[c("Situacao", "Vendas", "VPL", "Variacao")]
  v
}


#' Sensibilidade-BDI do Incorporador
#'
#' @examples
#' range_bdi_i <- c(0.9, 1.1)
#'
#' s_bdi_i <- sensibilidade_bdi_i(range_bdi_i,
#'                              cc = cc, wc = wc,
#'                              vgv = vgv, wv = wv,
#'                              bdi_i = bdi_i, bdi_c = bdi_c,
#'                              cor = cor, tma = tma)
#' s_bdi_i
#' @export

sensibilidade_bdi_i <- function(range, cc, wc, vgv, wv, bdi_i, bdi_c, cor, tma){
  bdi <- tibble::tibble(BDI_Incorporador = c(range[1], mean(range), range[2])*bdi_i)
  fci <- FCI(cc, wc, bdi_c = bdi_c)
  fcv <- FCV(vgv, wv)
  fcl <- apply(bdi, 1, FCL, fcv = fcv, fci = fci, cor = cor)
  bdi$VPL <- sapply(fcl, VPL, tma = tma, simplify = TRUE)
  bdi <- dplyr::arrange(bdi, VPL)
  bdi$Situacao <- c("Pessimista", "Provavel", "Otimista")
  bdi$Variacao <- c(bdi$VPL[1]/bdi$VPL[2] - 1, 0, bdi$VPL[3]/bdi$VPL[2] - 1)
  bdi <- bdi[c("Situacao", "BDI_Incorporador", "VPL", "Variacao")]
  bdi
}


#' Sensibilidade-BDI do Construtor
#'
#' @examples
#' range_bdi_c <- c(0.9, 1.1)
#' s_bdi_c <- sensibilidade_bdi_c(range = range_bdi_c,
#'                                cc = cc, wc = wc,
#'                                vgv = vgv, wv = wv,
#'                                bdi_i = bdi_i, bdi_c = bdi_c,
#'                                cor = cor, tma = tma
#'                                )
#' s_bdi_c
#' @export

sensibilidade_bdi_c <- function(range, cc, wc, vgv, wv, bdi_i, bdi_c, cor, tma){
  bdi <- tibble::tibble(BDI_Construtor = c(range[1], mean(range), range[2])*bdi_c)
  fci <- apply(bdi, 1, FCI, cc = cc, wc = wc)
  fcv <- FCV(vgv, wv)
  fcl <- lapply(fci, FCL, fcv = fcv, bdi_i = bdi_i, cor = cor)
  bdi$VPL <- sapply(fcl, VPL, tma = tma, simplify = TRUE)
  bdi <- dplyr::arrange(bdi, VPL)
  bdi$Situacao <- c("Pessimista", "Provavel", "Otimista")
  bdi$Variacao <- c(bdi$VPL[1]/bdi$VPL[2] - 1, 0, bdi$VPL[3]/bdi$VPL[2] - 1)
  bdi <- bdi[c("Situacao", "BDI_Construtor", "VPL", "Variacao")]
  bdi
}


#' Sensibilidade-Fluxo de Vendas
#'
#' @examples
#' wv_otimista <- c(0, 0, rep(2, 10))/20
#' wv_pessimista <- c(0, 0, rep(1, 18))/20
#' range_vv <- list(Pessimista = wv_pessimista, Provavel = wv, Otimista = wv_otimista)
#' s_vv <- sensibilidade_vv(range_vv,
#'                          cc = cc, wc = wc,
#'                          vgv = vgv,
#'                          bdi_i = bdi_i, bdi_c = bdi_c,
#'                          cor = cor, tma = tma)
#' s_vv
#' ggplot(s_vv, aes(x = Situacao, y = vpl)) +
#'   geom_col() +
#'   scale_y_continuous(labels = scales::dollar) +
#'   labs(title = "Sensibilidade à variação da velocidade de vendas")
#' @export

sensibilidade_vv <- function(range, cc, wc, vgv, wv, bdi_i, bdi_c, cor, tma){
  vv <- range
  fci <- FCI(cc, wc, bdi_c)
  fcv <- lapply(vv, FCV, vgv = vgv)
  fcl <- lapply(fcv, FCL, fci = fci, bdi_i = bdi_i, cor = cor)
  VPL <- sapply(fcl, VPL, tma = tma)
  vpl <- as.data.frame(VPL)
  vpl$Situacao <- rownames(vpl)
  vpl$VV <- vpl$Situacao
  vpl$Variacao <- c(vpl$VPL[1]/vpl$VPL[2] - 1, 0, vpl$VPL[3]/vpl$VPL[2] - 1)
  vpl <- tibble::as_tibble(vpl)
  vpl <- vpl[c("Situacao", "VV", "VPL", "Variacao")]
  vpl
}

#' Simulação com o Método de Monte Carlo
#'
#' @param Nsim Número de simulações
#' @param ranges Intervalos de variação de cada variável
#' @param variables Variáveis utilizadas para o computo do VPL
#' @param distribution Distribuição a priori a ser utilizada para
#' gerar as  variáveis
#' @param params parâmtros a serem utilizados pelas distribuições a priori.
#' @param dependencia matriz de covariância entre as variáveis
#'
#' @examples
#' ## Simulacao de Monte Carlo com distribuição uniforme e dependencia total
#'
#' set.seed(1)
#' ranges <- list(vgv = c(min = 0.9, max = 1.1),
#'                cc = c(min = 0.9, max = 1.1),
#'                bdi_i = c(min = 0.9, max = 1.1),
#'                bdi_c = c(min = 0.9, max = 1.1)
#' )
#' variables <- list(vgv = vgv, wv = wv, cc = cc, wc = wc, bdi_i = bdi_i,
#'                   bdi_c = bdi_c, cor = cor, tma = tma)
#' dependencia100 <- matrix(data = c(1, -1, -1, -1,
#'                                   -1, 1, 1, 1,
#'                                   -1, 1, 1, 1,
#'                                   -1, 1, 1, 1),
#'                        nrow = 4, byrow = TRUE,
#'                        dimnames = list(names(ranges), names(ranges)))
#'
#' Nsim <- 500
#'
#' vpl_unif100 <- vpl_sim(Nsim, ranges = ranges, variables = variables,
#'                    distribution = "uniform", dependencia = dependencia100)
#' m_unif100 <- mean(vpl_unif100$vpl)
#' std_unif100 <- sd(vpl_unif100$vpl)
#' hist(vpl_unif100$vpl, freq = FALSE)
#' curve(dnorm(x, mean = m_unif100, sd = std_unif100),
#'         col = "darkblue", lwd = 2, add = TRUE, yaxt = "n")
#' summary(vpl_unif100$vpl)
#'
#' # Its possible to compute the probabilities based in the simulations:
#' mean(vpl_unif100$vpl < 0.85*m_unif100) # probability that the VPL is less
#' than 85% of the mean
#'
#' # Or compute the probabilities based on the normal curve with mean and sd
#' equals to that of the simulation.
#' pnorm(0.85*m_unif100, mean = m_unif100, sd = std_unif100)
#'
#' ## Simulacao de Monte Carlo com distribuição uniforme e dependencia 50%
#'
#' dependencia50 <- matrix(data = c(1, -.5, -.5, -.5,
#'                                 -.5, 1, .5, .5,
#'                                 -.5, .5, 1, .5,
#'                                 -.5, .5, .5, 1),
#'                          nrow = 4, byrow = TRUE,
#'                          dimnames = list(names(ranges), names(ranges)))
#'
#' vpl_unif50 <- vpl_sim(Nsim, ranges = ranges, variables = variables,
#'                    distribution = "uniform", dependencia = dependencia50)
#' m_unif50 <- mean(vpl_unif50$vpl)
#' std_unif50 <- sd(vpl_unif50$vpl)
#' hist(vpl_unif50$vpl, freq = FALSE)
#' curve(dnorm(x, mean = m_unif50, sd = std_unif50),
#'         col = "darkblue", lwd = 2, add = TRUE, yaxt = "n")
#' summary(vpl_unif50$vpl)
#'
#' ## Simulacao de Monte Carlo com distribuição uniforme e variáveis totalmente
#' independentes
#'
#' dependencia0 <- diag(4)
#' dimnames(dependencia0) <- list(names(ranges), names(ranges))
#'
#' vpl_unif <- vpl_sim(Nsim, ranges = ranges, variables = variables,
#'                    distribution = "uniform", dependencia = dependencia0)
#' m_unif <- mean(vpl_unif$vpl)
#' std_unif <- sd(vpl_unif$vpl)
#' hist(vpl_unif$vpl, freq = FALSE)
#' curve(dnorm(x, mean = m_unif, sd = std_unif),
#'           col = "darkblue", lwd = 2, add = TRUE, yaxt = "n")
#' summary(vpl_unif$vpl)
#'
#' ## Simulacao de Monte Carlo com distribuição beta e dependencia 100%
#' params <- list(vgv = c(shape1 = 2, shape2 = 2),
#'                cc = c(shape1 = 2, shape2 = 2),
#'                bdi_i = c(shape1 = 2, shape2 = 2),
#'                bdi_c = c(shape1 = 2, shape2 = 2)
#' )
#' vpl_beta <- vpl_sim(Nsim, ranges = ranges, variables = variables,
#'                      distribution = "beta", params = params,
#'                      dependencia = dependencia100)
#'
#' m_beta <- mean(vpl_beta$vpl)
#' std_beta <- sd(vpl_beta$vpl)
#' ggplot(as.data.frame(vpl_beta), aes(vpl_beta)) +
#'  geom_histogram(aes(y =..density..), col="red", fill="green", alpha=.2) +
#'   stat_function(
#'     fun = dnorm,
#'     args = list(mean = m_beta, sd = std_beta)
#'   )

#' @export

vpl_sim <- function(Nsim, ranges, variables, distribution = "uniform",
                    params , dependencia = diag(length(ranges))){

  n <- length(ranges)
  mu <- rep(0, n)
  if (is.null(names(dependencia))) {
    dimnames(dependencia) <- list(names(ranges), names(ranges))
  }
  Sigma <- dependencia

  rawvars <- MASS::mvrnorm(n = Nsim, mu = mu, Sigma = Sigma)
  pvars <- as.data.frame(pnorm(rawvars))

  if (distribution == "uniform"){
    qvars <- list()
    for (nam in names(ranges)){
      qvars[[nam]] <- do.call("qunif",
                              list(pvars[[nam]],
                                   ranges[[nam]]["min"]*variables[[nam]],
                                   ranges[[nam]]["max"]*variables[[nam]]))
    }
    qvars <- tibble::as_tibble(qvars)
  } else if (distribution == "beta"){
    qvars <- list()
    for (nam in names(ranges)){
      qvars[[nam]] <- do.call("qbeta",
                              list(pvars[[nam]],
                                   params[[nam]]["shape1"],
                                   params[[nam]]["shape2"]))
      qvars[[nam]] <-
        (ranges[[nam]]["max"]*variables[[nam]] -
           ranges[[nam]]["min"]*variables[[nam]])*qvars[[nam]] +
        ranges[[nam]]["min"]*variables[[nam]]
    }
    qvars <- tibble::as_tibble(qvars)
  }

  qvars %<>% dplyr::mutate(fcv = purrr::map(vgv, FCV, wv = variables$wv),
                           fci = purrr::map2(cc, bdi_c, FCI, wc = variables$wc),
                           fcl = purrr::pmap(list(fcv, fci, bdi_i), FCL, cor = variables$cor),
                           vpl = purrr::map_dbl(fcl, VPL, tma = variables$tma))

  qvars
}