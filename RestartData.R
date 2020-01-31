RestartData <- function() {
  final_data <- setNames(data.frame(matrix(ncol = NUMBER_OF_COLUMNS_IN_FINAL_DATA_FRAME, nrow = 0))
                         , c("State", "County", "Year", "Type", "Month", "Day", "Item", "Vegetarian", "Sodium"))
  return(final_data)
}
