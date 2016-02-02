#' Mode (modus)
#'
#' Calculate the most frequent value (modus) of a vector.
#' Missing values (\code{NA}) are ignored.
#'
#' @param x a vector containing the values whose modus is to be
#'  computed.
#' @return A single value containing the most frequent value
#'  (modus) of \code{x}. For bi- or multimodal distributions
#'  with equal peaks, the first one is returned (see examples).
#' @author Gerhard Nachtmann \email{kpm.nachtmann@@gmail.com}
#' @seealso \code{\link{median}}, \code{\link{mean}}
#' @keywords mode modus
#' @export
#' @examples
#'
#' x <- round(rnorm(1000), 1)
#' modus(x)
#' y <- sample(letters, 100, replace = TRUE)
#' modus(y)
#' modus(c(1, 1, 2, 2))
#' modus(c("b", "b", "a", "a"))

modus <- function(x){
  names(which.max(table(x)))
}
