DetermineVariables <- function(file_name_vector) {
  
  pop <- function(file_name_vec, index1) {
    if (length(index1) != 0) {
      file_name_vec <- file_name_vec[-index1]
    }
    return(file_name_vec)
  }

  states <- c(
  "Alabama",        "Alaska",         "Arizona",        "Arkansas",       "California",     "Colorado",      
  "Connecticut",    "Delaware",       "Florida",        "Georgia",        "Hawaii",         "Idaho",         
   "Illinois",       "Indiana",        "Iowa",           "Kansas",         "Kentucky",       "Louisiana",     
   "Maine",          "Maryland",       "Massachusetts",  "Michigan",       "Minnesota",      "Mississippi",   
   "Missouri",       "Montana",        "Nebraska",       "Nevada",         "Hampshire",  "Jersey",    
   "Mexico",     "York",       "Carolina", "Dakota",   "Ohio",           "Oklahoma",      
   "Oregon",         "Pennsylvania",   "Rhode", "Tennessee",     
   "Texas",          "Utah",           "Vermont",        "Virginia",       "Washington", 
  "Wisconsin",      "Wyoming"  )
  
  months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  
  state <- tolower(file_name_vector[tolower(file_name_vector) %in% tolower(states)])
  index <- match(tolower(state), tolower(file_name_vector))
  if (state %in% c("mexico", "york", "jersey", "hampshire", "dakota", "carolina", "virginia")) {
    state <- tolower(c(paste(file_name_vector[index - 1], state)))
    file_name_vector <- file_name_vector[-index]
    file_name_vector <- file_name_vector[-(index - 1)]
  } else if (state == "rhode") {
    state <- "rhode island"
    file_name_vector <- file_name_vector[-index]
    file_name_vector <- file_name_vector[-index]
  } else {
    file_name_vector <- file_name_vector[-index]
  }
  
  year <- as.integer(file_name_vector[!is.na(as.integer(file_name_vector))])
  if (length(year) == 2) {
    index <- match(as.character(year[2]), file_name_vector)
    file_name_vector <- pop(file_name_vector, index)
    year <- year[1]
  }
  index <- match(as.character(year), file_name_vector)
  file_name_vector <- pop(file_name_vector, index)
  
  type <- file_name_vector[tolower(file_name_vector) %in% c("breakfast", "lunch")]
  index <- match(type, file_name_vector)
  file_name_vector <- pop(file_name_vector, index)
  
  month <- file_name_vector[tolower(file_name_vector) %in% tolower(months)]
  if (length(month) == 0 & "feb" %in% file_name_vector) {
    month <- "February"
  }
  index <- match(month, file_name_vector)
  file_name_vector <- pop(file_name_vector, index)
  
  #index <- match("elementary", tolower(file_name_vector))
  #file_name_vector <- pop(file_name_vector, index)
  type <- "Lunch"
  county <- paste(file_name_vector, collapse = ' ')
  
  return(c("state" = state, "county" = county, "year" = year,"type" = type,"month" = month))
}
