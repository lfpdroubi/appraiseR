#' Computes all the combinations of the transformed parameters
#'
#' @param data A data frame
#' @param subset a specification of the rows to be used: defaults to all rows.
#'   This can be any valid indexing vector (see \link{[.data.frame}) for the
#'   rows of data or if that is not supplied, a data frame made up of the
#'   variables used in \code{formula}.
#' @param select a character vector containing selected colnames from a data frame.
#'   Defaults for all the variables in data. See \link{subset}.
#' @param transf The transformations to be applied to each parameter
#' @return A matrix with all the working combinations of supplied
#'   transformations
#' @export
#' @examples
#' data(centro_2015)
#' dados <- centro_2015
#' vars <- colnames(dados)
#' perms <- allperm(dados, select = vars)
#' head(perms)

allperm <- function(data, subset, select = colnames(data),
                    transf = c('rsqrt', 'log', 'sqrt')){

  df <- as.data.frame(data)
  if (missing(subset)) subset <- seq_len(nrow(df))
  select <- setdiff(select, "geometry")
  df <- df[subset, colnames(data) %in% select]
  df <- stats::na.omit(df)

  for (i in colnames(df)) if (is.character(df[,i])) df[,i] <- as.factor(df[,i])

  factors <- plyr::colwise(is.factor)(df)

  any_zero <- function(x) any(x == 0, na.rm = TRUE)
  zeros <- plyr::colwise(any_zero)(df)

  # Nome das funcoes de transformacao
  nam_t <- c("identity", transf)
  # Nomes das transformacoes que admitem como argumento o valor zero
  nam_t2 <- setdiff(nam_t, c("rsqr", "rec", "rsqrt", "log"))

  # permutacao de todas as transformacoes pelas variaveis de nam_t
  if (sum(zeros == FALSE & factors == FALSE) > 0)
    perm1 <- gtools::permutations(n = length(nam_t),
                                  r = sum(zeros == FALSE & factors == FALSE),
                                  v = nam_t, repeats.allowed=T)

  # Insere os nomes das variaveis de df no data.frame perm1
  colnames(perm1) <- intersect(colnames(df[which(zeros == FALSE)]),
                               colnames(df[which(factors == FALSE)]))

  # permutacao de algumas transformacoes pelas variaveis de nam_t2
  if (sum(zeros == TRUE) > 0) {
    perm2 <- gtools::permutations(n=length(nam_t2),
                                  r = sum(zeros == TRUE),
                                  v = nam_t2, repeats.allowed=T)
    colnames(perm2) <- colnames(df[which(zeros == TRUE)])
    p <- merge(perm1, perm2)
  } else {
    p <- perm1
  }

  # reordena a matriz p de acordo com  a sequencia das variaveis de df
  p <- p[, colnames(df[which(factors == FALSE)])]

  return(p)
}