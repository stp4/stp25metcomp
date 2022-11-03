#' MetComp:  Uebereinstimmung und Praezision von Messwerten
#'
#' Tukey Mean Difference oder auch Bland Altman Methode. Oft interessiert die Zuverlaessigkeit und Reproduzierbarkeit ein einer Diagnose. Die Beurteilung kann dabei durch einen Bewerter (Messverfahren) in wiederholter Form erfolgen und wird dann als Intra-Rater bezeichnet oder die Beurteilung eines Merkmals erfolgt durch mehrere Bewerter (Messverfahren). und hier spricht man von Inter-Rater.
#' Die Methode der Beurteilung der uebereinstimmung haengt von den jeweiligen Datentype ab.
#' Bei Nominalen wird abgezaehlt und die Rate der uebereinstimmung bewertet (Cohen-Koeffizient) Bei Ordinalen-Daten werden die gewichteten uebereinstimmungen ausgezaehlt (gewichteter Cohen-Koeffizient). Bei metrischen(stetigen) Daten werden die Differenzen beurteilt (Bland-Altman-Methode).
#'
#' Bland-Altman-Methode Bias (d) systematische Abweichung Messfehler (s) Standardabweichung der Differenz Limits of agreement (LOA) Intervall von 95 (entspricht d+-1.96 -> es wird eine Normalverteilung unterstellt).
#' Methoden Die generische Funktion MetComp() kann sowohl Kappa als auch Tukey-means berechnen. Kappa kann aber auch ueber die xtab() und APA2 berechnet werden. Wobei hier nur 2x2-Tabellen untersucht werden und bei Kappa() sind hingegen auch mehrere ordinale Kategorien erlaubt sind.
#' aehnliche Methode ist ICC die aber eher zur Reliabilitaetsanalyse gehoert.
#' @param data Daten
#' @param x Formula Objekt
#' @return Ein bland_altman-Objekt mit den Daten (data) und der Statistik (stat).
#' @export
#' @examples
#'
#' #require(stp25stat)
#' #require(stp25plot)
#' #require(stp25output)
#' ### Verschiedene Situationen Im folgenden habe ich eine fiktive Messung mit simulierten Daten
#'
#' set.seed(0815)
#'
#' n <- 100
#' DF <- data.frame(
#'   A = rnorm(n, 100, 50),
#'   B = rnorm(n, 100, 50),
#'   C = NA,
#'   D = NA,
#'   E = NA,
#'   F = NA,
#'   group = sample(gl(2, n / 2, labels = c("Control", "Treat")))
#' )
#'
#' cutA <- mean(DF$A)
#' DF <- transform(
#'   DF,
#'   C = round(A + rnorm(n,-5, 20)),
#'   D = round(A + rnorm(n, 0, 10) + A / 10),
#'   E = A + ifelse(A < cutA, A / 5,-A / 5) + rnorm(n, 0, 10),
#'   F = A +  rnorm(n, 50, 10)
#' )
#'
#'
#'
#' #### Methoden messen das Selbe
#'
#' x<- MetComp(~A+C, DF)
#' #plot(x)
#' tab<- x$stat[,1:2]
#' names(tab)   <- c("Parameter", "Methoden messen das Selbe_M=0" )
#'
#' x<- MetComp(~A+F, DF)
#' tab<- cbind(tab, "Methoden messen das Selbe_Fehler M=50"= x$stat$Unit)
#' #plot(x)
#'
#'
#'
#' #### Methoden messen unterschiedlich Werte
#' x<- MetComp(~A+B, DF)
#' tab<- cbind(tab, "Methoden unterschiedliche_Fehler M=0"= x$stat$Unit)
#' #plot(x)
#'
#'
#'
#'
#' Output(tab, caption="BA" )
#'
#' t1 <-
#'   APA2(with(DF, t.test( A, C,  paired = TRUE)), output=FALSE)
#' t2 <-
#'   APA2(with(DF, t.test( A, F,  paired = TRUE)), output=FALSE)
#' t3 <-
#'   APA2(with(DF, t.test( A, B,  paired = TRUE)), output=FALSE)
#'
#'
#' #Output(rbind( t1, t2, t3), caption="T-Test")
#'
#'
#' #### Methoden haben systematische Abweichungen
#'
#' x<- MetComp(~A+D, DF)
#' #plot(x)
#'
#'
#'
#' x<- MetComp(~A+E, DF)
#' #plot(x)
#'
MetComp <- function(...,
                    include.ci = TRUE,
                    ci.level = .95,
                    include.weighted=TRUE,
                    include.unweighted =TRUE,

                    digits = 2
                ) {

  X <- stp25tools::prepare_data2(...)
  res <- NULL

  if (all(X$measure.class == "numeric") |
      all(X$measure.class == "integer")) {

    res <-
      MetComp_BAP(
        X = X,
        include.ci = include.ci,
        ci.level = ci.level,
        digits = digits
      )

    res$stat <-
        stp25stat2::prepare_output(
        res$stat,
        caption = paste0("Difference (", res$name.diff, "), Mean (",  res$name, ")"),
        note = ""
      )
  }
  else if (all(X$measure.class == "factor")) {

    xtb <- xtabs(X$formula, X$data[X$measure.vars])
    res <-
      MetComp_Kappa(xtb,
                    include.ci = include.ci,
                    ci.level = ci.level,
                    include.weighted=include.weighted,
                    include.unweighted =include.unweighted
                    )

    res$stat <-  stp25stat2::prepare_output(res,
                        caption = "Cohen's Kappa-Koeffizient",
                        note = "")
  }
  else{
    print(X$measure.class)
    stop("Unbekannte measure.variablen!")
  }


   res
}









