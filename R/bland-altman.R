#' @rdname MetComp
#' @param ... an prepare_data2
#' @param X  Aufbereitete Daten aus prepare_data2
#'
#' @return list(stats, name, name.dif. met_A, met_B, groups)
#' @export
#'
#' @examples
#'
#'
#' #- Understanding Bland Altman analysis
#' #Davide Giavarina
#' #Biochemia medica 2015;25(2) 141-51
#' #http://dx.doi.org/10.11613/BM.2015.015
#'
#' set.seed(0815)
#' DF<- data.frame(
#'   A=c(1, 5,10,20,50,40,50,60,70,80, 90,100,150,200,250,300,350,400,450,500,550,600,650,700,750,800,850,900, 950,1000),
#'   B=c(8,16,30,14,39,54,40,68,72,62,122, 80,181,259,275,380,320,434,479,587,626,648,738,766,793,851,871,957,1001, 980),
#'   group= sample(gl(2, 15, labels = c("Control", "Treat")))
#' )
#'
#' MetComp(~A+B, DF, caption = "Giavarina")
#'
#'





MetComp_BAP <-
  function(...,
           X = NULL,
           include.ci = TRUE,
           ci.level = .95,
           digits = 2) {
    if (is.null(X))
      X <- stp25tools::prepare_data2(...)

     X$data <- na.omit(X$data)

    ba.stats <- bland.altman.stats(
      X$data[X$measure.vars],
      X$data[X$group.vars],
      include.ci = include.ci,
      ci.level = ci.level,
      digits = digits
    )

    #  print(str(X))
    ba.stats$name <-  paste(X$measure.vars[1:2], collapse = ", ")
    ba.stats$name.diff <-  paste(X$measure.vars[1:2], collapse = " - ")
    ba.stats$met_A <- X$measure.vars[1]
    ba.stats$met_B <- X$measure.vars[2]
    ba.stats$groups <-
     if(is.null(X$group.vars)) NULL  else  X$data[X$group.vars]
    #print( names( X))

    ba.stats
  }



#-- Helper Bland Altman
bland.altman.stats <- function (dfr,
                                groups=NULL,
                                two = 1.96,
                                include.ci = TRUE,
                                ci.level = .95,
                                digits = 2) {
  # called.with <- nrow(dfr)

  based.on <- nrow(dfr)
  if (based.on < 2)
    warning("Warning in bland.altman.stats:less than 2 data pairs after deleting NAs.",
            call. = FALSE)
  if (ncol(dfr) > 2)
    warning("Warning in bland.altman.stats:Mehr als 2 Methoden.",
            call. = FALSE)
  diffs <- dfr[[1]] - dfr[[2]]
  means <-  rowMeans(dfr)
  diffs.percent <- diffs / means * 100
  diffs.percent[is.infinite(diffs.percent)] <- 0

  critical.diff <- two * sd(diffs)
  mean.diffs <- mean(diffs)
  sd.diffs <- sd(diffs)
  lower.limit <- mean.diffs - critical.diff
  upper.limit <- mean.diffs + critical.diff
  lines <- c(
    lower.limit = lower.limit,
    mean.diffs = mean.diffs,
    upper.limit = upper.limit
  )
  t1 <- qt((1 - ci.level) / 2, df = based.on - 1)
  t2 <- qt((ci.level + 1) / 2, df = based.on - 1)

  se.ci <- sqrt(sd(diffs) ^ 2 * 3 / based.on)
  se.mean <- sd(diffs) / sqrt(based.on)
  CI.lines <- c(
    lower.limit.ci.lower = lower.limit + t1 * se.ci,
    lower.limit.ci.upper = lower.limit + t2 * se.ci,
    mean.diff.ci.lower = mean.diffs + t1 * se.mean,
    mean.diff.ci.upper = mean.diffs + t2 * se.mean,
    upper.limit.ci.lower = upper.limit +  t1 * se.ci,
    upper.limit.ci.upper = upper.limit +  t2 * se.ci
  )
  #--- Prozent


  mean.percent <- mean(diffs.percent)
  ssd.percent <- sd(diffs.percent)
  critical.diff.percent <- two * ssd.percent
  se.ci.percent <- sqrt(ssd.percent ^ 2 * 3 / based.on)
  se.mean.percent <- ssd.percent / sqrt(based.on)
  lower.limit.percent = mean.percent - critical.diff.percent
  upper.limit.percent = ssd.percent + critical.diff.percent

  CI.lines.percent <-
    c(
      lower.limit.ci.lower = lower.limit.percent + t1 * se.ci.percent,
      lower.limit.ci.upper = lower.limit.percent + t2 * se.ci.percent,
      mean.diff.ci.lower = mean.percent + t1 * se.mean.percent,
      mean.diff.ci.upper = mean.percent + t2 * se.mean.percent,
      upper.limit.ci.lower = upper.limit.percent +  t1 * se.ci.percent,
      upper.limit.ci.upper = upper.limit.percent +  t2 * se.ci.percent
    )

  ci_format <-   stp25rndr::rndr_CI(cbind(
    low = c(CI.lines[3], CI.lines[1], CI.lines[5]),
    up = c(CI.lines[4], CI.lines[2], CI.lines[6])
  ) ,
  digits = digits)



  stat <- data.frame(
    Parameter = c(
      "df (n-1)",
      "difference mean (d)",
      "standard deviation (s)",
      "critical.diff (1.96s)",
      "d-1.96s",
      "d+1.96s"
    ),
    Unit = c(
      stp25rndr::Format2(based.on - 1, 0),
      stp25rndr::Format2(
        c(
          mean.diffs,
          sd.diffs,
          critical.diff,
          lower.limit,
          upper.limit
        ),
        digits
      )
    ),
    CI = c(NA, ci_format[1], NA, NA, ci_format[2], ci_format[3]) ,


    SE = stp25rndr::Format2(c(NA, se.mean, NA, NA, se.ci, se.ci), digits),
    Percent = c("",
                stp25rndr::rndr_percent(
                  c(
                    mean.percent,
                    ssd.percent,
                    critical.diff.percent,
                    lower.limit.percent,
                    upper.limit.percent
                  )
                  ,
                  digits = 1
                ))

    ,
    stringsAsFactors = FALSE
  )
  if (!include.ci) {
    stat <- stat[,-3]
  }

  res <- list(
    lines = lines,
    # wie oben ll mean ul
    CI.lines = CI.lines,
    lines.percent = c(
      mean.percent - critical.diff.percent,
      mean.percent,
      mean.percent + critical.diff.percent
    ),
    CI.lines.percent = CI.lines.percent,

    stat = stat,
    data = cbind(dfr,
                 means,
                 diffs,
                 diffs.percent = diffs.percent,
                 groups)

  )
  class(res) <- c("bland_altman")

  res
}



#'
#' MetComp_BAP <-
#'   function(...,
#'            X = NULL,
#'            include.ci = TRUE,
#'            ci.level = .95,
#'            digits = 2) {
#'     if (is.null(X))
#'       X <- stp25tools::prepare_data2(...)
#'
#'     ba.stats <- bland.altman.stats(
#'       X$data[X$measure.vars],
#'       include.ci = include.ci,
#'       ci.level = ci.level,
#'       digits = digits
#'     )
#'
#'     #  print(str(X))
#'     ba.stats$name <-  paste(X$measure.vars[1:2], collapse = ", ")
#'     ba.stats$name.diff <-  paste(X$measure.vars[1:2], collapse = " - ")
#'     ba.stats$met_A <- X$measure.vars[1]
#'     ba.stats$met_B <- X$measure.vars[2]
#'     #' grops sind auch in data vorhanden
#'     ba.stats$groups <-  X$X_data
#'
#'
#'     ba.stats
#'   }
