
MetComp_Kappa <- function(x,
                          include.ci = TRUE,
                          ci.level = .95,
                          include.weighted=TRUE,
                          include.unweighted=TRUE,
                          weights = "Equal-Spacing",# c("Equal-Spacing", "Fleiss-Cohen")
                          x_kapa = vcd::Kappa(x, weights=weights)
) {

  tab <-
    rbind(Unweighted = x_kapa$Unweighted,
          Weighted = x_kapa$Weighted)

  z <- tab[, 1] / tab[, 2]
  p <-  2 * pnorm(-abs(z))

  if (include.ci) {
    q <- qnorm((1 + ci.level) / 2)
    lower <- tab[, 1] - q * tab[, 2]
    upper <- tab[, 1] + q * tab[, 2]
    ci <- cbind(lower, upper)
  }

  res <- data.frame(
    Source = c("Unweighted", "Weighted"),
    Kappa = stp25stat2:::render_f(tab[[1]], digits=2),
    CI = stp25stat2:::rndr_CI(ci, digits=2),
    ASE = stp25stat2:::render_f(tab[, 2], digits=2),

    "z-Test" = stp25stat2:::render_f(z, digits=2),
    p.value = stp25stat2:::rndr_P(p, FALSE),
    stringsAsFactors = FALSE
  )

  if (all(dim(x) == c(2, 2))) res[1,]
  else if (include.weighted & include.unweighted ) res
  else if(include.unweighted) res[1,]
  else if(include.weighted)   res[2,]
  else  res
}



#' Cohens Kappa fuer k Urteilsauspraegungen
#'
#'  Kategorisierung von Cohens-Kappa-Werten
#' Wert von Kappa Ausmass der Uebereinstimmung
#' < 0.20  =  nicht ausreichend (poor)
#' 0.21 bis 0.40  =  hinreichend (fair)
#' 0.41 bis 0.60  =  moderat (moderat)
#' 0.61 bis 0.80  =  gut (good)
#' 0.81 bis 1.0  =  sehr gut (very good)
#'
#'
#'
#' @param x Objekt Kappa aus VCD
#' @param weights an vcd::Kappa   c("Equal-Spacing", "Fleiss-Cohen")
#' @param include.weighted,include.unweighted  kappa statistic
#' @export
#'
#' @examples
#'
#' Botulinum <- data.frame(
#' A= factor(c(rep(1, 14), rep(1, 3),
#'             rep(0, 5),rep(0, 18)),
#'           1:0, c("+", "-")),
#' B= factor(c(rep(1, 14), rep(0, 3),
#'             rep(1, 5),rep(0, 18)),
#'           1:0, c("+", "-")))
#' 
#' 
#' vcd::Kappa(xtabs(~A+B, Botulinum))
#' Tbll_kappa(xtabs(~A+B, Botulinum))
#' 
#' Tbll_kappa(~A+B, Botulinum)
#'
#'
#'
Tbll_kappa <- function(x, ...,
                  include.ci = TRUE,
                  ci.level = 0.95,
                  include.weighted = TRUE,
                  include.unweighted = TRUE,
                  weights = "Equal-Spacing" # c("Equal-Spacing", "Fleiss-Cohen")
                  )
{
  if(!inherits(x, "table")) {
    X <- stp25tools::prepare_data2(x, ...)
    x <- xtabs(X$formula , X$data )

  }
  x_kapa = vcd::Kappa(x, weights=weights)

  tab <- MetComp_Kappa(
    NULL,
    include.ci,
    ci.level,
    include.weighted = include.weighted,
    include.unweighted = include.unweighted,
    x_kapa = x_kapa
  )
 stp25stat2::prepare_output(tab,
                            caption = "Cohens Kappa",
                            note = "")

}

# require(vcd)
# ## ----sachs-627-data ---------------------------------------
# Botulinum <- data.frame(
#   A= factor(c(rep(1, 14), rep(1, 3),
#               rep(0, 5),rep(0, 18)),
#             1:0, c("+", "-")),
#   B= factor(c(rep(1, 14), rep(0, 3),
#               rep(1, 5),rep(0, 18)),
#             1:0, c("+", "-")))
# 
# 
# vcd::Kappa(xtabs(~A+B, Botulinum))
# Tbll_kappa(xtabs(~A+B, Botulinum))
# 
# Tbll_kappa(~A+B, Botulinum)
