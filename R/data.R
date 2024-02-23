library(magrittr)

centro_2015 <- suppressWarnings(readr::read_csv2("./inst/centro_2015.csv"))

## Tidying data

# centro_2015
centro_2015 <- centro_2015[,-1]
colnames(centro_2015) <- c("valor", "area_total", "quartos", "suites",
                           "garagens", "dist_b_mar", "padrao", "E", "N")

centro_2015$padrao <-
  stringi::stri_trans_general(centro_2015$padrao, "latin-ascii")

padrao_levels <- c("baixo", "medio", "alto")

centro_2015$padrao %<>% readr::parse_factor(padrao_levels)

## Transform data.frames to SpatialPointsDataFrame

centro_2015 <- sf::st_as_sf(centro_2015,
                            coords = c("E", "N"), crs = 31982)

#' Prices of 50 Florianopolis' downtown apartaments
#'
#' A SpatialPointsDataFrame containing a sample of 50 apartaments with
#' prices and other attributes in Florianopolis' downtown
#'
#' @format A tibble with 53 rows (50 samples and 3 apartments to be
#'   appraised) and 7 variables:
#' \itemize{
#'   \item valor: price, in brazilian Reais
#'   \item area_total: Total Area, in squared meters
#'   \item quartos: Rooms
#'   \item suites: Ensuites
#'   \item garagens: Garages
#'   \item dist_b_mar: Distance to the beach
#'   \item padrao: Building Standard - baixo, medio, alto
#'   (i.e. low, normal, high)
#' }
#' @source \strong{HOCHHEIM, Norberto}. \emph{Engenharia de avaliacoes
#' imobiliarias: Modulo Basico}. Florianopolis: IBAPE/SC, 2015, p.21-22
#' @examples
#' data(centro_2015)
#' centro_2015$padrao <- as.numeric(centro_2015$padrao)
#' fit <- lm(log(valor) ~ area_total + quartos + suites + garagens +
#'               log(dist_b_mar) + I(1/padrao), data = centro_2015)
#' # Look for outliers
#' library(car)
#' qqPlot(fit)
#' fit1 <- update(fit, subset = -c(31, 39))
#' qqPlot(fit1)
#' summary(fit1)
"centro_2015"

zilli_2020 <- suppressWarnings(readr::read_csv2("./inst/zilli.csv"))

zilli_2020 <- within(zilli_2020, {
  MO <- factor(MO, levels = c("N", "SM", "MO"))
  PSN <- as.numeric(as.factor(PSN)) - 1
  CH <- as.numeric(as.factor(CH)) - 1
  PC <- factor(PC, levels = c("B", "M", "A"))
  BRO <- factor(BRO, levels = c("Centro", "Agronomica", "Trindade"))
  })

zilli_2020 <- sf::st_as_sf(zilli_2020,
                           coords = c("COORD_E", "COORD_N"), crs = 31982)

#' Prices of 225 Florianopolis' apartaments in 3 neighborhoods
#'
#' A SpatialPointsDataFrame containing a sample of 225 apartaments with
#' prices and other attributes in 3 different Florianopolis' neighbourhoods.
#'
#' @format A tibble with 53 rows (50 samples and 3 apartments to be
#'   appraised) and 7 variables:
#' \itemize{
#'   \item VT: price, in brazilian Reais
#'   \item VU: price per sq. meter
#'   \item AP: Private Area, in squared meters
#'   \item DPXV: Distance to Praça XV
#'   \item DSBM: Distance to Beira Mar Mall
#'   \item DSIG: Distance to Iguatemi Mall
#'   \item DCTC: Distance to  CTC/UFSC
#'   \item DABM: Distance to Beira Mar Avenue
#'   \item ND: Number of rooms
#'   \item NB: Number of bathrooms
#'   \item NS: Number of ensuites
#'   \item NG: Number of garages
#'   \item MO: Furnishes - N (none), SM (some), MO (full)
#'   \item PSN: Swimming pool?
#'   \item CH: Barbecue grill?
#'   \item PC: Building Standard - B, M, A
#'   (i.e. low, normal, high)
#'   \item BRO: Neighborhood
#' }
#' @source \strong{ZILLI, Carlos Augusto}. \emph{Regressão geograficamente
#' ponderada aplicada na avaliação em massa de imóveis urbanos.}. 2020.
#' Dissertação de Mestrado em Engenharia de Transportes e Gestão Territorial.
#' Centro Tecnológico da UFSC. Florianópolis/SC.
#' @examples
#' data(zilli_2020)
#' zilli_2020$PC <- as.numeric(zilli_2020$PC)
#' fit <- lm(log(VU) ~ log(AP) + log(DABM) + ND + NB + NG + PSN + PC,
#' data = zilli_2020[1:190, ], subset = -c(86, 115))
#' summary(fit)
#'
#' fefit <- lm(log(VU) ~ log(AP) + log(DABM) + ND + NB + NG + PSN + PC + BRO,
#' data = zilli_2020[1:190, ], subset = -c(86, 115))
#' summary(fefit)
#'
"zilli_2020"

trindade <- tibble::tibble(VU = c(427, 458, 510, 511, 528, 545,
                                  564, 574, 574, 590, 601, 602,
                                  602, 609, 620))

#' Prices of 15 Florianopolis' apartaments with 2 rooms in Trindade neighbourhood
#'
#' A tibble containing a sample of 15 unitary values of apartment sales prices
#' per squared meters.
#' @format a tibble with 15 rows and 1 variable:
#' \itemize{
#'   \item VU unitary value per squared meter.
#' }
#' @source \strong{HOCHHEIM, Norberto}. \emph{Engenharia de Avaliacoes I}.
#' Florianopolis: IBAPE/SC, 2005, p.18
"trindade"

jungles <- tibble::tribble(
  ~Meses,  ~a, ~b, ~c, ~d, ~e, ~f, ~g, ~h, ~i, ~j, ~k, ~l, ~m, ~n, ~o, ~p, ~q, ~r, ~s, ~t, ~u, ~v, ~x, ~y,
  1, 100, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  2,  47, 53, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  3,  23, 50, 27, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  4,  14, 33, 37, 16, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  5,   9, 24, 30, 28,  9, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  6,   7, 17, 23, 26, 21,  6, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  7,   5, 12, 19, 23, 20 ,17,  4, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  8,   4, 10, 15, 17, 20, 18, 13,  3, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  9,   3,  8, 13, 16, 16, 18, 14,  9,  3, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  10,   2,  6, 11, 14, 14, 15, 14, 14,  8,  2, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  11,   2,  5,  8, 12, 13, 13, 14, 15, 12,  4,  2, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  12,   2,  5,  6, 11, 11, 11, 14, 12, 11, 11,  4,  2, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  13,   2,  4,  6,  8, 11, 11, 11, 11, 13,  9,  8,  5,  1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  14,   2,  3,  5,  6, 10, 10, 11, 11, 12,  9,  9,  8,  3,  1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  15,   2,  2,  4,  7,  8, 10, 10, 10, 10, 10,  9,  8,  6,  3,  1, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  16,   2,  2,  3,  6,  7,  8,  9,  9, 10, 10,  9,  9,  8,  4,  3,  1, NA, NA, NA, NA, NA, NA, NA, NA,
  17,   2,  2,  3,  5,  5,  8,  9,  9,  9, 10, 10,  8,  7,  6,  4,  2,  1, NA, NA, NA, NA, NA, NA, NA,
  18,   2,  2,  3,  4,  4,  8,  8,  8,  8,  9,  9,  9,  8,  6,  6,  3,  2,  1, NA, NA, NA, NA, NA, NA,
  19,   1,  2,  3,  4,  4,  7,  7,  8,  8,  8,  7,  7,  7,  7,  6,  6,  4,  3,  1, NA, NA, NA, NA, NA,
  20,   1,  2,  3,  3,  3,  4,  6,  7,  9,  9,  8,  8,  7,  6,  6,  6,  6,  3,  2,  1, NA, NA, NA, NA,
  21,   1,  1,  2,  2,  4,  6,  6,  6,  6,  7,  7,  8,  7,  7,  6,  6,  6,  5,  4,  2,  1, NA, NA, NA,
  22,   1,  2,  2,  2,  4,  4,  6,  6,  6,  6,  7,  8,  8,  7,  6,  6,  5,  5,  4,  2,  2,  1, NA, NA,
  23,   1,  2,  2,  2,  4,  4,  5,  5,  6,  6,  7,  6,  6,  6,  6,  6,  6,  6,  5,  4,  2,  2,  1, NA,
  24,   1,  1,  2,  2,  3,  3,  4,  6,  6,  6,  6,  6,  7,  7,  6,  6,  6,  5,  5,  5,  3,  2,  1,  1
)

#' Parametrized optimals costs percentages per period according to Jungles and
#' Avila (2009)
#'
#' A tibble containing optimal costs percentages for each period for projects
#' with 1 to 24 periods.
#' @format a tibble with 24 rows and 25 columns
#'
#' @source \strong{JUNGLES, A. E.; AVILA, A. V.}. \emph{Gestao do controle e
#' planejamento de empreendimentos}. 2009.
"jungles"

loteamento <- readr::read_csv("./inst/loteamento_residencial.csv")

#' Land division data
#'
#' A tibble containing a sample of 20 plots in subdivision in Florianopolis.
#' Paradigm situation: dry, flat, 15m front width and 30~60m length.
#'
#' @format A tibble with 20 rows and 8 variables:
#' \itemize{
#'   \item valor: price, in brazilian Reais
#'   \item area: land area in squared meters
#'   \item tipo: type: offer or sale -
#'   venda, oferta (i.e. sale, offer)
#'   \item frente: front width of the land in meters
#'   \item profundidade: length of the land in meters
#'   \item topo: topography -
#'   plano, aclive (i.e. flat, slope)
#'   \item inclinacao: slope
#'   \item pedologia: pedology -
#'   seco, pantanoso (i.e. dry, marshy)
#' }
#' @source \strong{HOCHHEIM, Norberto}. \emph{Engenharia de Avaliacoes I}.
#' Florianopolis: IBAPE/SC, 2005, p.74
#' @examples
#' data(loteamento)
#'
#' # Fatores do IBAPE/SP 2005 (aditivo), cf. Hochheim (2005 , p.82)
#' loteamento <- within(loteamento, {
#'                Coferta <- ifelse(tipo == "oferta", 1.11, 1)
#'                Cfrente <- (frente/15)^0.15
#'                Ctopo <- ifelse(topo == "plano", 1,
#'                           ifelse(inclinacao/100 >= .20, 0.85,
#'                             ifelse(inclinacao/100 > .10,  0.90,
#'                               ifelse(inclinacao/100 > 0, .95,
#'                                 ifelse(inclinacao/100 >= -.05, .95,
#'                                   ifelse(inclinacao/100 >= -.10, .9,
#'                                     ifelse(inclinacao/100 >= -.20, .80, .70)))))))
#'                Cpedo <- ifelse(pedologia == "seco", 1, .6)
#'                Chom <-  (1 + ((Coferta - 1) + (Cfrente - 1) +
#'                                       (Ctopo - 1) + (Cpedo - 1)))
#'                PU <- valor/area
#'                PUhom <- PU/Chom
#' }
#' )
#'
#' # Saneamento da amostra
#' outlier_analysis(loteamento$PUhom)
#' outlier_analysis(loteamento$PUhom, "2_sd")
#' outlier_analysis(loteamento$PUhom, "chauvenet")
#'
#' # Avaliacao final
#' PUmedio <- mean(loteamento$PUhom[-c(7, 19)])
#' sdPU <- sd(loteamento$PUhom[-c(7, 19)])
#'
#' # Poder de predicao
#' loteamento <- within(loteamento, P <- PUmedio*area*Chom)
#' powerPlot(y = loteamento$valor[-c(7, 19)], yhat = loteamento$P[-c(7, 19)],
#'            axis = "inverted")
#'
#' # Fatores IBAPE/SP 2011 (misto)
#'
#' loteamento <- within(loteamento, {
#'                Coferta <- ifelse(tipo == "oferta", 1.11, 1)
#'                Cfrente <- (frente/15)^0.15
#'                Ctopo <- ifelse(topo == "plano", 1,
#'                           ifelse(inclinacao/100 >= .20, 0.85,
#'                             ifelse(inclinacao/100 > .10,  0.90,
#'                               ifelse(inclinacao/100 > 0, .95,
#'                                 ifelse(inclinacao/100 >= -.05, .95,
#'                                   ifelse(inclinacao/100 >= -.10, .9,
#'                                     ifelse(inclinacao/100 >= -.20, .80, .70)))))))
#'                Cpedo <- ifelse(pedologia == "seco", 1, .6)
#'                Chom <-  Coferta*(1 + ((Cfrente - 1) + (Ctopo - 1) +
#'                                   (Cpedo - 1)))
#'                PU <- valor/area
#'                PUhom <- PU/Chom
#' }
#' )
#'
#' # Saneamento da amostra
#' outlier_analysis(loteamento$PUhom)
#' outlier_analysis(loteamento$PUhom, "2_sd")
#' outlier_analysis(loteamento$PUhom, "chauvenet")
#'
#' # Avaliacao final
#' PUmedio <- mean(loteamento$PUhom[-c(7, 19)])
#' sdPU <- sd(loteamento$PUhom[-c(7, 19)])
#'
#' # Poder de predicao
#' loteamento <- within(loteamento, P <- PUmedio*area*Chom)
#' powerPlot(y = loteamento$valor[-c(7, 19)], yhat = loteamento$P[-c(7, 19)],
#'            axis = "inverted")
#'
#' # Fatores multiplicativos
#' loteamento <- within(loteamento, {
#'                Coferta <- ifelse(tipo == "oferta", 1.11, 1)
#'                Cfrente <- (frente/15)^0.25
#'                Chom <-  Coferta*Cfrente*Ctopo*Cpedo
#'                PU <- valor/area
#'                PUhom <- PU/Chom
#' }
#' )
#'
#' # Saneamento da amostra
#' outlier_analysis(loteamento$PUhom)
#' outlier_analysis(loteamento$PUhom, "2_sd")
#' outlier_analysis(loteamento$PUhom, "chauvenet")
#'
#' # Avaliacao final
#' PUmedio <- mean(loteamento$PUhom[-c(7, 19)])
#' sdPU <- sd(loteamento$PUhom[-c(7, 19)])
#'
#' # Poder de predicao
#' loteamento <- within(loteamento, P <- PUmedio*area*Chom)
#' powerPlot(y = loteamento$valor[-c(7, 19)], yhat = loteamento$P[-c(7, 19)],
#'            axis = "inverted")
#'
#' # Regressao Linear
#' fit <- lm(log(PU) ~ log(frente/15) + tipo + poly(inclinacao, 2) + pedologia,
#'            data = loteamento, subset = -c(7, 19))
#' powerPlot(fit, axis = "inverted", scale = "original", func = "log")
#' p <- predict(fit, newdata = list(frente = 15, tipo = "venda", inclinacao = 0,
#'                               pedologia = "seco"))
#' exp(p)

"loteamento"

jurere_2017 <- readr::read_csv2("./inst/jurere.csv")

jurere_2017$PAVIMENTOS <- as.factor(jurere_2017$PAVIMENTOS)
jurere_2017$TOPOGRAFIA <- as.factor(jurere_2017$TOPOGRAFIA)

jurere_2017 <- sf::st_as_sf(jurere_2017,
                            coords = c("E", "N"), crs = 31982)

#' Land division data
#'
#' A tibble containing a sample of 20 plots in subdivision in Florianopolis.
#' Paradigm situation: dry, flat, 15m front width and 30~60m length.
#'
#' @format A tibble with 20 rows and 8 variables:
#' \itemize{
#'   \item VALOR TOTAL: price, in brazilian Reais
#'   \item VU: unitary value per sq. meters
#'   \item AREA: area, in sq. meters
#'   \item TESTADA: front width of the land in meters
#'   \item ESQUINA: corner?
#'   0 = no; 1 = yes
#'   \item FRENTES: number of lot fronts
#'   \item DIST_MAR: distance to the sea
#'   \item PAVIMENTOS: number of floors
#'   \item DIST_MAR: distance to the sea
#'   \item CONDOMINIO_FECHADO: gated community?
#'   0 = no, 1 = yes.
#'   \item PAVIMENTO: paved?
#'   NAO = no, SIM = yes.
#'   \item TOPOGRAFIA: topography
#'   PLANO = flat
#'   \item DATA: date
#'   \item FONTE: source
#' }
#' @examples
#' data(jurere_2017)
#' fit <- lm(log(VU) ~ log(AREA)*log(TESTADA) + log(DIST_MAR) + PAVIMENTOS,
#'           data = jurere_2017)
#' library(effects)
#' plot(predictorEffects(fit, residuals = T))
#' # Centering
#' library(dplyr)
#' jurere_2017 <- mutate(jurere_2017,
#'                        AREA = AREA/450,
#'                        TESTADA = TESTADA/15,
#'                        DIST_MAR = DIST_MAR/33)
#' fit1 <- lm(log(VU) ~ log(AREA) + log(TESTADA) + log(DIST_MAR) + PAVIMENTOS,
#'           data = jurere_2017, subset = -c(11, 27, 29))
#'
"jurere_2017"

trivelloni_2005 <- readr::read_csv2("./inst/trivelloni_2005.csv")

# trivelloni_2005
colnames(trivelloni_2005) <- c("Obs", "E", "N", "valor", "tipo", "area_total",
                               "area_terreno", "garagens", "novo",
                               "P_2", "P_3", "P_4")

trivelloni_2005$novo %<>% as.factor()

trivelloni_2005$novo %<>%
  forcats::fct_recode(usado = "0", novo = "1")

trivelloni_2005$garagens %<>% as.factor()

trivelloni_2005$garagens %<>%
  forcats::fct_recode(nao = "0", sim = "1")

trivelloni_2005$padrao <-
  with(trivelloni_2005, ifelse(P_2 == 1, "alto",
                               ifelse(P_3 == 1, "medio",
                                      ifelse(P_4 == 1, "baixo", NA))))

trivelloni_2005$padrao %<>%
  readr::parse_factor(levels = padrao_levels)

trivelloni_2005 %<>% dplyr::select(-c(P_2, P_3, P_4))

trivelloni_2005$tipo %<>%
  readr::parse_factor(levels = c("Apartame", "Kitinete",
                                 "Comercia", "terreno",
                                 "casa")
  )

trivelloni_2005$tipo %<>%
  forcats::fct_recode(casa = "casa",
                      apartamento = "Apartame", kitinete = "Kitinete",
                      comercial = "Comercia", terreno = "terreno")

trivelloni_2005 %<>%
  dplyr::mutate_at(dplyr::vars(area_terreno), function(x) ifelse(x == 0, 0.1, x))

trivelloni_2005 <- trivelloni_2005[, -1]

trivelloni_2005 <- sf::st_as_sf(trivelloni_2005,
                                coords = c("E", "N"),
                                crs = 5527)

#' Urban large parcels data
#'
#' A tibble containing a sample of 19 large parcels.
#'
#' @format A tibble with 19 rows and 4 variables:
#' \itemize{
#'   \item R: id
#'   \item Ficha: another id (unused)
#'   \item VU: unitary value per sq. meters
#'   \item Area: area, in sq. meters
#' }
#' @examples
#' data(glebas)
#' fit <- lm(rec(VU) ~ log(Area), data = glebas)
#' library(effects)
#' plot(predictorEffects(fit, residuals = T), id = T,
#'       axes = list(
#'         grid = TRUE,
#'         y=list(transform=list(trans=rec, inverse=rec), lab = "VU")
#'         )
#'      )
#' # Issue: Influential Point 4 (see also plot(fit))
#'
#' # Solution:
#' fit1 <- update(fit, rec(VU)~log(Area), subset = -4)
#' plot(predictorEffects(fit1, residuals = T), id = T,
#'       axes = list(
#'         grid = TRUE,
#'         y=list(transform=list(trans=rec, inverse=rec), lab = "VU")
#'       )
#'     )
"glebas"

glebas <- readr::read_csv2("./inst/glebas.csv")

#' Urban large parcels with built area data
#'
#' A tibble containing a sample of 20 large parcels with different built areas.
#'
#' @format A tibble with 20 rows and 5 variables:
#' \itemize{
#'   \item R: id
#'   \item Ficha: another id (unused)
#'   \item VI: sale price
#'   \item AT: land area, in sq. meters
#'   \item AC: Built area, in sq. meters
#' }
#' @examples
#' data(glebas2)
#' fit <- lm(VI ~ log(AT) + AC, data = glebas2)
#' library(effects)
#' plot(predictorEffects(fit, residuals = T), id = T,
#'       axes = list(
#'         grid = TRUE,
#'         x = list(rotate=30)
#' ))
#' powerPlot(fit, axis="inverted", smooth = TRUE, methods = c("lm", "loess"))
#' # Issue: Influential Points 5 and 10 (see also plot(fit))
#'
#' # Solution 1 (better to interpret):
#' fit1 <- update(fit, VI ~ AT + AC, subset = -c(2, 5,10))
#' plot(predictorEffects(fit1, residuals = T), id = T,
#'       axes = list(
#'         grid = TRUE,
#'         x = list(rotate=30)
#' ))
#' powerPlot(fit1, axis = "inverted", smooth = TRUE, methods = c("lm", "loess"))
#' predict(fit1, newdata = list(AT = 9123.50, AC = 2272.47),
#'              interval = 'confidence', level = .80)
#' # + 30% higher value than predicted with the original fit
#'
#' # Solution 2 (just to add some nonlinear relationship between the original
#'                variables)
#' fit2 <- update(fit, sqrt(VI) ~ sqrt(AT) + sqrt(AC), subset = -c(2, 10))
#' plot(predictorEffects(fit2, residuals = T), id = T,
#'       axes = list(
#'         grid = TRUE,
#'         x = list(rotate=30),
#'         y = list(transform=list(trans=sqrt, inverse=sqr), lab = "VI")
#' ))
#' powerPlot(fit2, axis = "inverted", smooth = TRUE, methods = c("lm", "loess"),
#'             func="sqrt") # note bias and nonlinearity
#'
#' predict(fit2, newdata = list(AT = 9123.50, AC = 2272.47),
#'              interval = 'confidence', level = .80)
#' # Almost 50% higher value than predicted with the original fit
"glebas2"

glebas2 <- readr::read_csv2("./inst/glebas2.csv")

#' Urban large parcels in different urban contexts
#'
#' A tibble containing a sample of 17 large parcels within differents urban
#' contexts.
#'
#' @format A tibble with 17 rows and 5 variables:
#' \itemize{
#'   \item R: id
#'   \item VU: unitary value per sq. meters
#'   \item AT: land area, in sq. meters
#'   \item ACESSO: dummy variable that indicates if the area is direct reachble
#'   or not
#'   \item SUP: dummy variable that indicates if the area was landfilled
#' }
#' @examples
#' data(glebas3)
#' fit <- lm(log(VU) ~ I(AT^-1) + ACESSO + SUP, data = glebas3)
#' library(effects)
#' plot(predictorEffects(fit, residuals = T), id = T,
#'       axes = list(
#'         grid = TRUE,
#'         x = list(rotate=30),
#'         y = list(transform=list(trans=log, inverse=exp), lab = "VU")
#' ))
#' powerPlot(fit, axis="inverted", smooth = TRUE, methods = c("lm", "loess"))
#' p <- predict(fit, newdata = list(AT = 60000, ACESSO = factor(0),
#'                                   SUP = factor(1)),
#'                interval = "confidence", level = .80
#'             )
#' exp(p)
#' amplitude(exp(p)) # very good!

"glebas3"

glebas3 <- readr::read_csv2("./inst/glebas3.csv")
glebas3 <- within(glebas3,{
  ACESSO <- factor(ACESSO)
  SUP <- factor(SUP)
})

usethis::use_data(centro_2015, zilli_2020, trindade, jungles, loteamento,
                  jurere_2017, trivelloni_2005, glebas, glebas2, glebas3,
                  overwrite = TRUE)
