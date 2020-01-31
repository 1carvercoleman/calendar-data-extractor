findDayIndex <- function(mydata) {
  indices <- c()
  for (i in 1:length(mydata)) {
    if (!(is.na(as.numeric(mydata[i])))) {
      indices <- c(indices, i)
    }
  }
  return(indices)
}
