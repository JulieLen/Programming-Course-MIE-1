#### Session 5 - Homework ####

# Create a function to put into the package

#' An lindirect_utility function
#'
#' This function outpus the indirect utility for a log-linear utility (alog(X) + blog(Y))
#' @export
#' @examples
#' lindirect_utility()


lindirect_utility <- function(x, y, px, py, a, b){
  R <- px*x + py*y
  mdx = (a*R)/(px*(a+b))
  mdy = (b*R)/(py*(a+b))
  U <- a*log(mdx) + b*log(mdy)
  return(U)
}

lindirect_utility(12,5, 1, 2, 1, 1)





