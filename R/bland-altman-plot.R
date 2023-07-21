#' Uebereinstimmung und Praezision von Messwerten
#'
#' Tukey Mean Difference oder auch Bland Altman Metode. Oft iteressiert
#' die Zuverlässigkeit und Reproduzierbarkeit ein einer Diagnose.
#' Die Beurteikung kann dabei durch einen Bewerter (Messverfahren) in wiederholter Form erfolgen
#' und wird dan als Intra-Raterbezeichnet oder die Beurteilung eines Merkmals erfolgt durch mehere Bewerter (Messverfahren).
#' und hier spricht man von Inter-Rater.
#'
#' Die Methode der Burteilung der Uebereinstimmung haengt vom jeweiligen Datentype ab.
#'
#' @param x  bland_altman-Objekt , par=TRUE
#' @param main   default =  name.diff
#' @param main1,main2,main3  default "regression", "differences", "precentage"
#' @param ylab1,ylab2,ylab3  default met_B, "Differences" "Differences/Average"
#' @param xlab1,xlab2,xlab3  default met_A, name, x$name
#' @param type    type = c("r", "d", "p") regression, differences, precentage
#' @param pch,col  default = 19 farbe()

#' @param x.var,y.var   met_A,  met_B
#' @param pos  topleft
#' @param legend  levels groups
#' @param fil weiss nicht
#' @param abline.col Farbe c("black","black","black"),
#' @param abline.lty  Lineie abline.lty=c(3,1,3),
#' @param par par
#' @param ...  nicht Benutzt
#' @param abline.lwd  Strischstaerke abline.lwd=c(1,1,1)
#'
#' @return Ein bland_altman-Objekt mit den Daten (data) und der Statistik (stat).
#' @export
#' @examples
#'
#' #- Understanding Bland Altman analysis
#' # Davide Giavarina
#' # Biochemia medica 2015;25(2) 141-51
#' # http://dx.doi.org/10.11613/BM.2015.015
#'
#' set.seed(0815)
#'
#' n<-100
#' DF<- data.frame(
#'   A=rnorm(n, 100,50),
#'   B=rnorm(n, 100,50),
#'   C=NA,  D=NA,  E=NA,
#'   group= sample(gl(2, n/2, labels = c("Control", "Treat")))
#' )
#'
#' cutA<-mean(DF$A)
#' DF <- transform(DF, C = round( A + rnorm(n, -5, 20)),
#'                 D = round( A + rnorm(n,0,10) + A/10 ),
#'                 #E = round( A + rnorm(n,5,10) + (100-A/10) )
#'                 E = A + ifelse(A<cutA, A/5, -A/5 )+ rnorm(n, 0, 10)
#' )
#'
#'
#' x<- MetComp(~A+C, DF)
#' #windows(8,3.2)
#' plot(x, type=c("r", "d"))
#' #SaveData(caption="A und C Messen das gleiche mit SD=20")
#'
#' # x<- MetComp(~A+B, DF)
#' # #windows(8,3.2)
#' # plot(x)
#' # #SaveData(caption="A und B Messen unterschiedliche Parameter")
#' #
#' #
#' # x<- MetComp(~A+D, DF)
#' # #windows(8,3.2)
#' # plot(x)
#' # #SaveData(caption="A und D Messen das unterschiedlich D hat im unteren
#' #  #        Wertevereich deutlich geringere Werte")
#' # x<- MetComp(~A+E, DF)
#' # #windows(8,3.2)
#' # plot(x)
#' # #SaveData(caption="A und E Messen das unterschiedlich es esistiert ein knik im Wertebereich 100")
#' #
plot.bland_altman <- function(x,
                              par = TRUE,
                              main = x$name.diff,
                              main1 = "regression",
                              main2 = "differences",
                              main3 = "precentages",

                              ylab = c( x$met_B,
                                         "Differences",
                                         "Differences/Average [%]"),

                              ylab1 = ylab[1],
                              ylab2 = ylab[2],
                              ylab3 = ylab[3],
                              xlab= c(x$met_A,
                                         paste0("Means (" , x$name, ")"),
                                         paste0("Means (" , x$name, ")")),
                              xlab1 = xlab[1],
                              xlab2 = xlab[2],
                              xlab3 = xlab[3],


                              type = c("r", "d", "p"),
                              pch = 19,
                              col = if (is.null(x$groups)) 1 else 3 + as.numeric(x$groups[[1]]),
                              x.var = x$met_A,
                              y.var = x$met_B,
                              pos = "topleft",
                              legend = levels(x$groups[[1]]),
                              fil = 3 + (1:nlevels(x$groups[[1]])),
                              abline.col = c("black", "black", "black"),
                              abline.lty = c(3, 1, 3),
                              abline.lwd = c(1, 1, 1),
                              lim1 =NULL,
                              lim2 = NULL,
                              lim3 =NULL,
                              
                              ...) {
  # print(x$lines) # print(x$CI.lines) # print(x$lines.percent) # print(x$CI.lines.percent)

  if (par)
    par(mfrow = c(1, length(type)), oma = c(0.4, 1, 1.6, 1))
  
  xy_plot_range <- if (is.null(lim1))  range(c(x$data[, x.var], x$data[, y.var]), na.rm = TRUE) else lim1
  

  if("r" %in% type) {

   plot(
    x$data[, x.var],
    x$data[, y.var],
    xlab = xlab1,
    ylab = ylab1,
    xlim=xy_plot_range, 
    ylim=xy_plot_range,
    main = main1,
    pch = pch,
    col = col,
    bty = 'l'
  )
  abline(lm(x$data[, y.var] ~ x$data[, x.var]),
         lty = 1,
         lwd = 1,
         col = abline.col[2])
  if (!is.null(x$groups))
    legend(
      x = pos,
      legend = legend,
      fill = fil,
      bty = "n"
    )
  title(main = main, outer = TRUE)
  }



  # plot(x$data$means, x$data$diffs,
  #      ylim= range(c(x$data$diffs, x$lines), finite = TRUE),
  #      xlab=xlab2, ylab=ylab2, main=main2, pch=pch, col=col )
  #     abline(h = x$lines, lty=abline.lty, col=abline.col, lwd=abline.lwd)
  #     abline(h = x$CI.lines)


  rgn<-range(x$data$means, finite = TRUE) * c(1, 1.15)
   
  ba_range <-  if(is.null(lim2)) range(c(x$data$diffs, x$lines), finite = TRUE) else lim2
  if("d" %in% type) {
  plot(
    x$data$means,
    x$data$diffs,
    type = "n",
    axes = FALSE,
    xlim = rgn,
    ylim = ba_range,
    xlab = xlab2,
    ylab = ylab2,
    main = main2
  )
  lim <- par("usr")
  rect(lim[1],
       x$CI.lines[3],
       lim[2],
       x$CI.lines[4],
       border = "gray80",
       col = "gray95")
  abline(
    h = x$lines,
    lty = abline.lty,
    col = abline.col,
    lwd = abline.lwd
  )
  points(x$data$means, x$data$diffs, pch = pch, col = col)

  text(x=c(rgn[2],rgn[2]),
       y=x$lines ,
       c("-1.96 SD", "Mean", "+1.96 SD"), adj=c(1,0), cex=.7)
  text(x=c(rgn[2],rgn[2]),
       y=x$lines,
       signif(x$lines, 2), adj=c(1,1), cex=.7)

  
  
  axis(1) ## add axes back
  axis(2)
  box()



  if (!is.null(x$groups))
    legend(
      x = pos,
      legend = legend,
      fill = fil,
      bty = "n"
    )

  }
  #- Prozent ---------------------------------
  ## Fehler wenn Inf
  #range(x$data$means )


  if("p" %in% type) {
  plot(
    x$data$means,
    x$data$diffs.percent,
    type = "n",
    axes = FALSE,
    xlim = rgn,
    ylim = range(c(x$lines.percent,x$data$diffs.percent), finite = TRUE),
    ##x$data$diffs.percent,
    xlab = xlab3,
    ylab = ylab3,
    main = main3
  )


  lim <- par("usr")
  rect(
    lim[1],
    x$CI.lines.percent[3],
    lim[2],
    x$CI.lines.percent[4],
    border = "gray80",
    col = "gray95"
  )
  abline(
    h = x$lines.percent,
    lty = abline.lty,
    col = abline.col,
    lwd = abline.lwd
  )
  points(x$data$means,
         x$data$diffs.percent,
         pch = pch,
         col = col)
  text(x=c(rgn[2],rgn[2]),
       y=x$lines.percent ,
       c("-1.96 SD", "Mean", "+1.96 SD"), adj=c(1,0), cex=.7)
  text(x=c(rgn[2],rgn[2]),
       y=x$lines.percent,
       signif(x$lines.percent, 2)  , adj=c(1,1), cex=.7)

  axis(1) ## add axes back
  axis(2)
  box()
  if (!is.null(x$groups))
    legend(
      x = pos,
      legend = legend,
      fill = fil,
      bty = "n"
    )
  }
}



