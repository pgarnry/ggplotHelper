#' Converts an array into a data.frame for plotting with ggplotHelper
#' @param array.in array with data for plotting. If the array contains 
#' multiple series, these must be ordered column-wise. 
#' @details Returns a dataframe with a "names" column and a "data" column.
#' If the array.in contains multiple columns, these are labeled by a legend
#' in the new data frame corresponding to the column name.
#' Test:
#' array1 <- array(c(1:30), dim = c(10,3), 
#'                 dimnames = list(paste0("Date", 1:10), paste0("Var", 1:3)))
#' ggplotHelper::line_chart(array2df(array1), x = "names", y = "data", 
#'                          group = "legend", legend.names = colnames(array1),
#'                          legend.position = "top")                 
#' @export

array2df <- function(array.in, x.yearmon = T) {

  if (length(dim(as.array(array.in))) == 1) {
    
    if (is.null(names(array.in))) {
      df <- data.frame(names = 1:length(array.in), data = array.in)
    } else {
      if (x.yearmon) {
        df <- data.frame(names = as.yearmon(names(array.in)), data = array.in)
      } else df <- data.frame(names = names(array.in), data = array.in)
      
    }
      

  } else {

    for (n.col in 1:ncol(array.in)) {
      
      df.temp <- data.frame(names = rownames(array.in),
                            data = array.in[, n.col],
                            legend = colnames(array.in)[n.col],
                            row.names = NULL)
      
      if (n.col == 1) df <- df.temp else df <- rbind(df, df.temp)
      
    }
    
  }

  return(df)
}
