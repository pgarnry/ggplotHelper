#' Converts a 1D array into a data.frame with a "names" column
#' @param array.in one-dimensional array
#' @details Returns a dataframe with a "names" column and a "data" column
#' @export

array2df <- function(array.in) {

  if (1 < length(dim(array.in))) stop("Input array must be one-dimensional")

  df <- data.frame(names = names(array.in),
                   data = array.in)

  return(df)
}
