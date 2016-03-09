tablify <- function(items, col_names=NULL, row_names=NULL, n_col, n_row){
  ### takes a vector of numbers and turns them into a nice
  ### printed out matrix, looks nice in r markdown
  tmp <- matrix(items, ncol=n_col, nrow=n_row, byrow=T)
  if(is.null(col_names) == FALSE){
    colnames(tmp) <- col_names
  }
  if(is.null(row_names) == FALSE){
    row.names(tmp) <- row_names
  }

  return(tmp)
}



