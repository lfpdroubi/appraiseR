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

centro_2015 <-
  sp::SpatialPointsDataFrame(coords = centro_2015[c("E", "N")],
                             data = subset(centro_2015, select = -c(E,N)),
                             proj4string = sp::CRS("+proj=utm +zone=22 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

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
"centro_2015"

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
"loteamento"

jurere_2017 <- readr::read_csv2("./inst/jurere.csv")

jurere_2017$PAVIMENTO <- as.factor(jurere_2017$PAVIMENTO)
jurere_2017$TOPOGRAFIA <- as.factor(jurere_2017$TOPOGRAFIA)

jurere_2017 <-
  sp::SpatialPointsDataFrame(coords = jurere_2017[c("E", "N")],
                             data = subset(jurere_2017, select = -c(E,N)),
                             proj4string = sp::CRS("+proj=utm +zone=22 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

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
"jurere_2017"

usethis::use_data(centro_2015, trindade, jungles, loteamento, jurere_2017, overwrite = TRUE)
