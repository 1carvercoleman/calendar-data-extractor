splitAt <- function(x, pos) { 
  unname(split(x, cumsum(seq_along(x) %in% pos)))
}
