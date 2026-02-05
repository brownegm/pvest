#' Split Data Frame by Vector
#' @param df A data frame to be split.
#' @param split_vector A vector of two integers indicating the number of rows to take from the head and tail of the data frame.
#' @return A list containing two data frames: one with the specified number of rows from the head and another with the specified number of rows from the tail.                              
split_df <- function(df, split_vector) {
  h <- split_vector[1]
  t <- split_vector[2]
  
  # Get rows from head
  head_rows <- head(df, h)
  
  # Get rows from tail
  tail_rows <- tail(df, t)
  
  # Combine the rows
  result <- list(above = head_rows, below = tail_rows)
  
  return(result)
}

#' Split Data Frame by Vector
#' @param df_list A list of data frames to be split.
#' @param split_vectors A list of vectors indicating the number of rows to take from the head and tail of each data frame.
#' @return A list of lists, where each inner list contains the split data frames.
split_all_dfs<- function(df_list, split_vectors) {
  mapply(split_df, df_list, split_vectors, SIMPLIFY = FALSE)
}
