# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello, world!")
}


#' @rdname MetComp
#' @export
print.bland_altman <- function(x) {
  cat(  "\n",  attr(x$stat,"caption"), "\n")
  print(x$stat)
}


#' @rdname MetComp
#' @export
Output.bland_altman <- function(x, ...) {
  Output(x$stat, ...)
}





#' @rdname MetComp
#' @export
APA.bland_altman <-
  function(x, ...) {
    paste0("m = ",
           x$stat$Unit[2],
           ", d = [",
           x$stat$Unit[5],
           ", ",
           x$stat$Unit[5],
           "]")
  }
