% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bland-altman-plot.R
\name{plot.bland_altman}
\alias{plot.bland_altman}
\title{Uebereinstimmung und Praezision von Messwerten}
\usage{
\method{plot}{bland_altman}(
  x,
  par = TRUE,
  main = x$name.diff,
  main1 = "regression",
  main2 = "differences",
  main3 = "precentages",
  ylab = c(x$met_B, "Differences", "Differences/Average [\%]"),
  ylab1 = ylab[1],
  ylab2 = ylab[2],
  ylab3 = ylab[3],
  xlab = c(x$met_A, paste0("Means (", x$name, ")"), paste0("Means (", x$name, ")")),
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
  lim1 = NULL,
  lim2 = NULL,
  lim3 = NULL,
  ...
)
}
\arguments{
\item{x}{bland_altman-Objekt , par=TRUE}

\item{par}{par}

\item{main}{default =  name.diff}

\item{main1, main2, main3}{default "regression", "differences", "precentage"}

\item{ylab1, ylab2, ylab3}{default met_B, "Differences" "Differences/Average"}

\item{xlab1, xlab2, xlab3}{default met_A, name, x$name}

\item{type}{type = c("r", "d", "p") regression, differences, precentage}

\item{pch, col}{default = 19 farbe()}

\item{x.var, y.var}{met_A,  met_B}

\item{pos}{topleft}

\item{legend}{levels groups}

\item{fil}{weiss nicht}

\item{abline.col}{Farbe c("black","black","black"),}

\item{abline.lty}{Lineie abline.lty=c(3,1,3),}

\item{abline.lwd}{Strischstaerke abline.lwd=c(1,1,1)}

\item{...}{nicht Benutzt}
}
\value{
Ein bland_altman-Objekt mit den Daten (data) und der Statistik (stat).
}
\description{
Tukey Mean Difference oder auch Bland Altman Metode. Oft iteressiert
die Zuverlässigkeit und Reproduzierbarkeit ein einer Diagnose.
Die Beurteikung kann dabei durch einen Bewerter (Messverfahren) in wiederholter Form erfolgen
und wird dan als Intra-Raterbezeichnet oder die Beurteilung eines Merkmals erfolgt durch mehere Bewerter (Messverfahren).
und hier spricht man von Inter-Rater.
}
\details{
Die Methode der Burteilung der Uebereinstimmung haengt vom jeweiligen Datentype ab.
}
\examples{

#- Understanding Bland Altman analysis
# Davide Giavarina
# Biochemia medica 2015;25(2) 141-51
# http://dx.doi.org/10.11613/BM.2015.015

set.seed(0815)

n<-100
DF<- data.frame(
  A=rnorm(n, 100,50),
  B=rnorm(n, 100,50),
  C=NA,  D=NA,  E=NA,
  group= sample(gl(2, n/2, labels = c("Control", "Treat")))
)

cutA<-mean(DF$A)
DF <- transform(DF, C = round( A + rnorm(n, -5, 20)),
                D = round( A + rnorm(n,0,10) + A/10 ),
                #E = round( A + rnorm(n,5,10) + (100-A/10) )
                E = A + ifelse(A<cutA, A/5, -A/5 )+ rnorm(n, 0, 10)
)


x<- MetComp(~A+C, DF)
#windows(8,3.2)
plot(x, type=c("r", "d"))
#SaveData(caption="A und C Messen das gleiche mit SD=20")

# x<- MetComp(~A+B, DF)
# #windows(8,3.2)
# plot(x)
# #SaveData(caption="A und B Messen unterschiedliche Parameter")
#
#
# x<- MetComp(~A+D, DF)
# #windows(8,3.2)
# plot(x)
# #SaveData(caption="A und D Messen das unterschiedlich D hat im unteren
#  #        Wertevereich deutlich geringere Werte")
# x<- MetComp(~A+E, DF)
# #windows(8,3.2)
# plot(x)
# #SaveData(caption="A und E Messen das unterschiedlich es esistiert ein knik im Wertebereich 100")
#
}
