#' Kolgomorov-Smirnov
#' @export
KS <- function(fit) {
  # Ver https://rpubs.com/mharris/KSplot
  sample1 <- rnorm(10000, 0, 1)
  sample2 <- rstandard(fit)
  group <- c(rep("Normal", length(sample1)),
             rep("Resíduos Padronizados", length(sample2)))
  dat <- data.frame(KSD = c(sample1,sample2), group = group)
  # create ECDF of data
  cdf1 <- ecdf(sample1)
  cdf2 <- ecdf(sample2)
  # find min and max statistics to draw line between points of greatest distance
  minMax <- seq(min(sample1, sample2), max(sample1, sample2), length.out=length(sample1))
  x0 <- minMax[which( abs(cdf1(minMax) - cdf2(minMax)) == max(abs(cdf1(minMax) - cdf2(minMax))) )]
  y0 <- cdf1(x0)
  y1 <- cdf2(x0)
  p <- ggplot(dat, aes(x = KSD, group = group, color = group))+
    stat_ecdf(size=1) +
    xlab("Resíduos") +
    ylab("ECDF") +
    #geom_line(size=1) +
    geom_segment(aes(x = x0[1], y = y0[1], xend = x0[1], yend = y1[1]),
                 linetype = "dashed", color = "red") +
    geom_point(aes(x = x0[1] , y= y0[1]), color = "red", size = 2) +
    geom_point(aes(x = x0[1] , y= y1[1]), color = "red", size = 2) +
    ggtitle("Teste K-S (Kolgomorov-Smirnov)") +
    theme(legend.title=element_blank(),
          legend.position = "bottom")
  print(ks.test(rstandard(fit), "pnorm"))
  return(p)
}
