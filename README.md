stp25metcomp
================
Wolfgang Peter
2022-11-03

## Methodenvergleich

Oft interessiert die Zuverlässigkeit und Reproduzierbarkeit einer
Diagnose. Die Beurteilung kann dabei durch einen Bewerter
(Messverfahren) in wiederholter Form erfolgen und wird dann als
Intra-Rater bezeichnet oder die Beurteilung eines Merkmals erfolgt durch
mehrere Bewerter (Messverfahren). und hier spricht man von Inter-Rater.
Die Methode der Beurteilung der Übereinstimmung hängt von den jeweiligen
Verteilungseigenschaften ab.

Bei Nominalen wird abgezählt und die Rate der Übereinstimmung bewertet
(Cohen-Koeffizient) Bei Ordinalen-Daten werden die gewichteten
Übereinstimmungen ausgezählt (gewichteter Cohen-Koeffizient). Bei
metrischen(stetigen) Daten werden die Differenzen beurteilt
(Bland-Altman-Methode oder auch Tukey Mean Difference).

Bland-Altman-Methode Bias (d) systematische Abweichung Messfehler (s)
Standardabweichung der Differenz Limits of agreement (LOA) Intervall von
95 (entspricht d+-1.96 -\> es wird eine Normalverteilung unterstellt).

### MetComp

Die generische Funktion MetComp() kann sowohl Kappa als auch Tukey-means
berechnen. Kappa kann aber auch über die xtab() berechnet werden. Wobei
hier nur 2x2-Tabellen untersucht werden können, hingegen sind bei
Kappa() auch mehrere ordinale Kategorien erlaubt.

Ähnliche Methode ist ICC die aber diese zählt eher zur
Reliabilitätsanalyse.

Funktionen: MetComp, BAP, und Kappa

Beispiel Altman and Bland \[@Giavarina2015\]

``` r
 kable ( MetComp(~A+B, Giavarina)$stat )
```

| Parameter              | Unit   | CI                  | SE    | Percent |
|:-----------------------|:-------|:--------------------|:------|:--------|
| df (n-1)               | 29     | NA                  |       |         |
| difference mean (d)    | -27.50 | \[-40.17, -14.83\]  | 6.20  | \<0.1%  |
| standard deviation (s) | 33.94  | NA                  |       | 39.8%   |
| critical.diff (1.96s)  | 66.52  | NA                  |       | 78.0%   |
| d-1.96s                | -94.02 | \[-115.97, -72.07\] | 10.73 | \<0.1%  |
| d+1.96s                | 39.02  | \[17.07, 60.97\]    | 10.73 | 117.8%  |

#### Beispiel Botulinum A

Sachs Angewandte Statistik Seite 627

``` r
MetComp(~A+B, Botulinum)
```

    ## Warning in format.data.frame(if (omit) x[seq_len(n0), , drop = FALSE] else x, :
    ## corrupt data frame: columns will be truncated or padded with NAs

    ##                Source Kappa            CI  ASE z.Test p.value              stat
    ## Unweighted Unweighted  0.60 [0.35,  0.85] 0.13   4.71   <.001 # A tibble: 1 × 6

``` r
 xt <-xtabs(~A+B, Botulinum)
 Klassifikation(xt)$statistic[c(1,3,4), ]
```

# A tibble: 3 × 2

Statistic estimate <chr> <chr>  
1 Accuracy 0.80  
2 No Information Rate 0.52  
3 P-Value \[Acc \> NIR\] p\<.001

``` r
set.seed(0815)
DF<- data.frame(
  A=c(1, 5,10,20,50,40,50,60,70,80, 90,100,150,200,250,300,350,400,450,500,550,600,650,700,750,800,850,900, 950,1000),
  B=c(8,16,30,14,39,54,40,68,72,62,122, 80,181,259,275,380,320,434,479,587,626,648,738,766,793,851,871,957,1001, 980),
  Therapie= sample(gl(2, 15, labels = c("Control", "Treat")))
)

rslt <- DF %>% MetComp(A,B)
head(rslt$data)
```

    ##    A  B means diffs diffs.percent
    ## 1  1  8   4.5    -7    -155.55556
    ## 2  5 16  10.5   -11    -104.76190
    ## 3 10 30  20.0   -20    -100.00000
    ## 4 20 14  17.0     6      35.29412
    ## 5 50 39  44.5    11      24.71910
    ## 6 40 54  47.0   -14     -29.78723

``` r
rslt <- DF %>% MetComp(A,B,  by=~Therapie)
head(rslt$data)
```

    ##    A  B means diffs diffs.percent Therapie
    ## 1  1  8   4.5    -7    -155.55556    Treat
    ## 2  5 16  10.5   -11    -104.76190  Control
    ## 3 10 30  20.0   -20    -100.00000    Treat
    ## 4 20 14  17.0     6      35.29412    Treat
    ## 5 50 39  44.5    11      24.71910  Control
    ## 6 40 54  47.0   -14     -29.78723    Treat

``` r
plot(rslt)
```

![](README_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
Giavarina <- transform(Giavarina, C = round( A + rnorm(30,0,20)),
                D = round( A + rnorm(30,0,10) + A/10 ),
                E = round( A + rnorm(30,5,10) + (100-A/10) ))


#ICC2(~A+E, Giavarina, caption="ICC (Korrelationen)")
```

``` r
# A - Goldstandart

x <- MetComp_BAP(~A+B+E, Giavarina)
```

    ## Warning: Warning in bland.altman.stats:Mehr als 2 Methoden.

``` r
# x %>% Output("BA-Analyse der Messwertreihe")
plot(x)
```

![Bland Altman](README_files/figure-gfm/fig-BlandAltman3-1.png)

``` r
x <- MetComp_BAP(~A+E+B, Giavarina)
```

    ## Warning: Warning in bland.altman.stats:Mehr als 2 Methoden.

``` r
# x %>% Output("BA-Analyse der Messwertreihe")
plot(x)
```

![Bland Altman](README_files/figure-gfm/fig-BlandAltman4-1.png)

### Verschiedene Situationen

``` r
x<- MetComp_BAP(~A+C, DF)
plot(x)
```

![A und C Messen das gleiche mit
SD=20](README_files/figure-gfm/fig-BAx1-1.png)

``` r
x<- MetComp_BAP(~A+B, DF)
plot(x)
```

![A und B Messen unterschiedliche
Parameter](README_files/figure-gfm/fig-BAx2-1.png)

``` r
x<- MetComp_BAP(~A+D, DF)
plot(x)
```

![A und D Messen das unterschiedlich D hat im unteren Wertevereich
deutlich geringere Werte](README_files/figure-gfm/fig-BAx3-1.png)

``` r
x<- MetComp_BAP(~A+E, DF)
plot(x)
```

![A und E Messen das unterschiedlich es esistiert ein Knick im
Wertebereich 100](README_files/figure-gfm/fig-BAx4-1.png)
