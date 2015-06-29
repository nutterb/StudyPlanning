#' @name expand.grid.df
#' 
#' @title Expand grid
#' @description Expand grid of data frames
#' 
#' @param ... list of data frames (first varies fastest)
#' @param unique only use unique rows?
#' 
#' @details Creates new data frame containing all combination of rows from data.frames in ...
#' 
#' This is an exact copy of the \code{expand.grid.df} function in the \code{reshape}
#' package.  I chose to include it as a function within this package to avoid using
#' a dependency to a package that is no longer maintained.
#' 
#' @author Hadley Wickham 
#' 
#' @examples
#' \dontrun{
#' expand.grid.df(data.frame(a=1,b=1:2))
#' expand.grid.df(data.frame(a=1,b=1:2), data.frame())
#' expand.grid.df(data.frame(a=1,b=1:2), data.frame(c=1:2, d=1:2))
#' expand.grid.df(data.frame(a=1,b=1:2), data.frame(c=1:2, d=1:2), data.frame(e=c("a","b")))
#' }

expand.grid.df <- function (..., unique = TRUE){  
  dfs <- list(...)
  notempty <- sapply(dfs, ncol) != 0
  if (sum(notempty) == 1) 
    return(dfs[notempty][[1]])
  if (unique) 
    dfs <- lapply(dfs, unique)
  indexes <- lapply(dfs, function(x) 1:nrow(x))
  grid <- do.call(expand.grid, indexes)
  df <- do.call(data.frame, mapply(function(df, index) df[index, 
                                                          , drop = FALSE], dfs, grid))
  colnames(df) <- unlist(lapply(dfs, colnames))
  rownames(df) <- 1:nrow(df)
  return(df)
}