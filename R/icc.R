#' Interklass Korrelationskoeffizient
#'
#'intraclass correlation coefficient (ICC)
#'
#' Der Intraklass-Korrelationskoeffizient (ICC) beurteilt die Stärke und Richtung der
#' Korrelation (Zusammenhang) zwischen den wiederholten Messungen eines Merkmals am selben Individuum.

#'
#' @param x,... an prepare_data2
#' @param missing,alpha,lmer,check.keys an psych::ICC
#' @param type Single_raters_absolute
#'
#' @return data.frame   tibble: x × 8
#'  type  ICC   lower bound upper bound F       df1   df2 p
#' @export
#'
#' @examples
#' #'
#' #'# Less than 0.50: Poor reliability
#' # Between 0.5 and 0.75: Moderate reliability
#' # Between 0.75 and 0.9: Good reliability
#' # Greater than 0.9: Excellent reliability
#'
#' #Suppose four different judges were asked to rate the quality of 10 different college entrance exams.
#' # The results are shown below:
#' data <- data.frame(
#'   A = c(1, 1, 3, 6, 6, 7, 8, 9, 8, 7),
#'   B = c(2, 3, 8, 4, 5, 5, 7, 9, 8, 8),
#'   C = c(0, 4, 1, 5, 5, 6, 6, 9, 8, 8),
#'   D = c(1, 2, 3, 3, 6, 4, 6, 8, 8, 9)
#' )
#' data
#'
#' Tbll_icc(data)
#'
#' # The intraclass correlation coefficient (ICC) turns out to be 0.782.
#' #
#' # Based on the rules of thumb for interpreting ICC,
#' # we would conclude that an ICC of 0.782 indicates that the exams
#' # can be rated with “good” reliability by different raters.
#'

Tbll_icc <- function(x, ...) {
  UseMethod("Tbll_icc")
}




#' @rdname Tbll_icc
#' @export
Tbll_icc.matrix <-
  function(x,
           ...,
           missing = TRUE,
           alpha = .05,
           lmer = TRUE,
           check.keys = FALSE,
           type = c(1, 4)) {
    #  if (!is.matrix(x)) {
    #    X <- stp25tools::prepare_data2(x, ...)
    #    x <- as.matrix(X$data[X$measure.vars])
    #  }

    # rslt <-
    #   psych::ICC(
    #     x,
    #     missing = missing,
    #     alpha = alpha,
    #     lmer = lmer,
    #     check.keys = check.keys
    #   )
    #
    # n <- paste0("obs=", rslt$n.obs, ", judge=", rslt$n.judge)
    #
    # ans <-  rslt$results[type, ]
    # ans$type <-  paste(ans$type,  gsub("_", " ", rownames(ans)))
    # ans$p <- stp25stat2:::rndr_P(ans$p, FALSE)
    # ans$ICC <-
    #   (paste(
    #     stp25stat2:::render_f(ans$ICC, digits = 2),
    #     stp25stat2:::rndr_CI2(ans[7:8], digits = 2)
    #   ))
    #
    # stp25stat2::prepare_output(ans[1:6],
    #                            caption = "ICC",
    #                            note = paste("Number of subjects: ", n))

    calc_icc(
      data,
      type = type,
      missing = missing,
      alpha = alpha,
      lmer = lmer,
      check.keys = check.keys
    )

  }


#' @rdname Tbll_icc
#'
#' @param data data.frame
#' @param rater  Messinatrument oder Rater
#' @param obs eindeutige Id kann auch aus meheren Parametern bestehen
#' @param value Messwert
#' @param type Number 1 bis 6
#' @param ... an psych::ICC
#'
#' @export
#'
#' @examples
#'
#'
#' DF <- stp25tools::get_data("
#' nr   ratings obs    rate
#' 1        9   obj1  rater1
#' 2        6   obj2  rater1
#' 3        8   obj3  rater1
#' 4        7   obj4  rater1
#' 5       10   obj5  rater1
#' 6        6   obj6  rater1
#' 7        2   obj1  rater2
#' 8        1   obj2  rater2
#' 9        4   obj3  rater2
#' 10       1   obj4  rater2
#' 11       5   obj5  rater2
#' 12       2   obj6  rater2
#' 13       5   obj1  rater3
#' 14       3   obj2  rater3
#' 15       6   obj3  rater3
#' 16       2   obj4  rater3
#' 17       6   obj5  rater3
#' 18       4   obj6  rater3
#' 19       8   obj1  rater4
#' 20       2   obj2  rater4
#' 21       8   obj3  rater4
#' 22       6   obj4  rater4
#' 23       9   obj5  rater4
#' 24       7   obj6  rater4")
#'
#' Tbll_icc(DF,
#'      value = "ratings",
#'      obs = "obs",
#'      rater = "rate")

Tbll_icc.data.frame <- function(data,
                                ...,
                                rater = NULL,
                                obs = NULL,
                                value = NULL,
                                type = 2 + 3,
                                missing = TRUE,
                                alpha = .05,
                                lmer = TRUE,
                                check.keys = FALSE) {
  # Workaraund
  # rater <- data[rater ]
  # obs <- data[obs]
  # value <- data[ value ]
  #cat("\nin Tbll_icc.data.frame \n")

  if (is.null(rater)) {
    X <-  stp25tools::prepare_data2(data, ...)

    data <- as.matrix(X$data)
    rownames(data)<- paste0("S", 1:nrow(data))
    # print(head(data))
    calc_icc(
      data,
      type = type,
      missing = missing,
      alpha = alpha,
      lmer = lmer,
      check.keys = check.keys
    )

  }

  else{
    data <- data[c(rater, obs, value)]
    if (length(obs) > 1) {
      data <- tidyr::unite(data, obs, obs)
      obs <- "obs"
    }
    if (length(rater) > 1) {
      data <- tidyr::unite(data, rater, rater)
      rater <- "rater"
    }


    data <- as.data.frame(stp25tools::Wide(data, rater, value))
    row.names(data) <- data[[obs]]
    data <- data[-1]

    #return(data)
    calc_icc(
      data,
      type = type,
      missing = missing,
      alpha = alpha,
      lmer = lmer,
      check.keys = check.keys
    )
  }
}

#' @noRd
calc_icc <- function(data, type = 2, ...) {
  rslt <- psych::ICC(data, ...)


  rslt$results[[2]] <- stp25stat2::render_f(rslt$results[[2]], 2)
  rslt$results[[3]] <- stp25stat2::render_f(rslt$results[[3]], 1)
  rslt$results[[6]] <- stp25stat2:::rndr_P(rslt$results[[6]], FALSE)
  rslt$results[[7]] <- stp25stat2::render_f(rslt$results[[7]], 2)
  rslt$results[[8]] <- stp25stat2::render_f(rslt$results[[8]], 2)


  n <- paste0("obs=", rslt$n.obs, ", judge=", rslt$n.judge)

  stp25stat2::prepare_output(
    rslt$results[type, c(1, 2, 7, 8, 3, 4, 5, 6)],
    caption = "Intraclass Correlations",
    note = paste("Number of subjects: ", n),
    N = n
  )

}


#' @importFrom psych ICC
#' @export
psych::ICC
