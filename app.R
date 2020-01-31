# Title: Menu Data Extraction
# Author: Carver Coleman
# Purpose: Extract tables from any image file

INSTALL_PACKAGES <- FALSE
NUMBER_OF_COLUMNS_IN_FINAL_DATA_FRAME <- 9
CAN_SUBMIT <<- FALSE

# Will install packages if INSTALL_PACKAGES == TRUE
if (INSTALL_PACKAGES) {
  install.packages("shiny", dependencies = TRUE)
  install.packages("imager", dependencies = TRUE)
  install.packages("MASS", dependencies = TRUE)
  install.packages("dequer", dependencies = TRUE)
  install.packages("magick", dependencies = TRUE)
  install.packages("tesseract", dependencies = TRUE)
  install.packages("DT", dependencies = TRUE)
}


library(shiny)
library(imager)
library(MASS)
library(dequer)
library(magick)
library(tesseract)
library(DT)
source('DetermineVariables.R')
options(warn=-1)


# Finds the indices of rows with only integers in a dataframe
findDayIndex <- function(mydata) {
  indices <- c()
  for (i in 1:length(mydata)) {
    if (!(is.na(as.numeric(mydata[i])))) {
      indices <- c(indices, i)
    }
  }
  return(indices)
}


# Splits a vector into lists by a vector of indices
splitAt <- function(x, pos) { 
  unname(split(x, cumsum(seq_along(x) %in% pos)))
}

# Clears the data
RestartData <- function() {
  final_data <- setNames(data.frame(matrix(ncol = NUMBER_OF_COLUMNS_IN_FINAL_DATA_FRAME, nrow = 0))
                         , c("State", "County", "Year", "Type", "Month", "Day", "Item", "Vegetarian", "Sodium"))
  return(final_data)
}

# Configure tesseract
valid_chars <- tesseract(language = "eng", options = list(tessedit_char_whitelist = " 0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ)(%/"))

# Create list of files in path
myfiles <- list.files(path = getwd(), pattern = ".png|.jpg")

# Prepare removing stack and dataset
rows_to_delete <<- stack()
final_data <- RestartData()
final_data_index <- 1





ui <- fluidPage(
  tags$h1("Menu Data Extractor: Developed by BYU Economics"),
  mainPanel(
    tabsetPanel(
      tabPanel("Setup",
          sidebarLayout(
            sidebarPanel(
              tags$a(href = "https://carver-coleman.shinyapps.io/menu-data-extractor-instructions/", "INSTRUCTIONS"),
              tags$h3("Variable Guesses"),
              textAreaInput("state", "State", ""),
              textAreaInput("county", "County", ""),
              textAreaInput("year", "Year", ""),
              textAreaInput("month", "Month", ""),
              textAreaInput("type", "Type (Lunch or Breakfast)", ""),
              checkboxInput("vegetarian", "Vegetarian Options", FALSE),
              checkboxInput("sodium", "Sodium Included", FALSE),
              checkboxInput("calories", "Calories Included", FALSE)
              ),
            mainPanel(
              fluidRow(
                column(6, tags$h3("Select a File for Extraction")),
                HTML('&nbsp;'),
                column(6, actionButton("next_button", "Next Menu"))
              ),
              selectInput("current_file", "My Files", myfiles, width = "700px"),
              #plotOutput("pdfImage"),
              imageOutput("image1", height = 300)
              ))),
      tabPanel("Extraction",
          sidebarLayout(
            sidebarPanel(
              textAreaInput("intermediate_data", "Extracted Data", "", height = '600px')
            ),
            mainPanel(
              fluidRow(
                HTML('&nbsp;')
              ),
              fluidRow(
                column(2, actionButton("intermediate_button", "Extract!")),
                column(2, actionButton("goButton", "Tabulize!"))
              ),
              fluidRow(
                column(12,
                     plotOutput("plot1",
                                click = "plot_click",
                                dblclick = "plot_dblclick",
                                hover = "plot_hover",
                                brush = "plot_brush"
      )))))),
      tabPanel("Extracted Data", 
        sidebarLayout(
          sidebarPanel(
            fluidRow(
              column(2),
              column(4, downloadButton("downloadData", "Download"))),
            fluidRow(actionButton("clearData", "Clear All Data"),
              actionButton("clearLast", "Clear Last Extraction"))
          ),
          mainPanel(
            dataTableOutput("table")
          )
        )
      )
    )
  )
)

server <- function(session, input, output) {
  output$image1 <- renderImage({
    img <- magick::image_read(input$current_file)
    width <- image_info(img)$width
    height <- image_info(img)$height
    list(src = input$current_file,
         contentType = "image/png",
         width = width / 2.5,
         height = height / 2.5,
         alt = "This is alternate text")
  }, deleteFile = FALSE)
  
  observeEvent(input$next_button, {
    myfiles <<- c(myfiles[2:length(myfiles)], myfiles[1])
    updateSelectInput(session, "current_file", "My Files", myfiles)
  })
  
  observeEvent(input$intermediate_button, {
    if (!is.null(input$plot_brush$xmin)) {
      myxmin <- round(input$plot_brush$xmin,1)
      myymin <- round(input$plot_brush$ymin,1)
      myxmax <- round(input$plot_brush$xmax,1)
      myymax <- round(input$plot_brush$ymax,1)
      img <- magick::image_read(input$current_file)
      image_height <- image_info(img)$height
      format_type <- substr(input$current_file, nchar(input$current_file) - 2, nchar(input$current_file))
      new_image <- image_crop(img, paste0(myxmax - myxmin, "x", myymax - myymin, "+", myxmin, "+", image_height - myymax))
      cut_file <- paste0(substr(input$current_file, 1, nchar(input$current_file) - 4), "_cut.", format_type)
      new_image <- image_convert(new_image, colorspace = "gray")
      image_write(new_image, cut_file , format = format_type)
      
      text <- tesseract::ocr(cut_file, engine = valid_chars)
      final <- strsplit(text, '\n')[[1]]

      final <- final[final != ""]
      final <- paste('\n', final)
      final <- gsub('Sunday|Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|January|
                    February|March|April|May|June|July|August|September|October|November|December|
                    â|€|¢|Ã|©|â|œ|â', "", final)
      updateTextAreaInput(session, "intermediate_data", "Extracted Data", c(input$intermediate_data, final))
      CAN_SUBMIT <<- TRUE
    }
  })
  
  observeEvent(input$current_file, {
    file_name_data <- strsplit(gsub('.{4}$', '', input$current_file), "_")[[1]]
    variables <- DetermineVariables(file_name_data)
    updateTextAreaInput(session, "state", "State", as.vector(variables['state']))
    updateTextAreaInput(session, "county", "County", as.vector(variables['county']))
    updateTextAreaInput(session, "year", "Year", as.vector(variables['year']))
    updateTextAreaInput(session, "month", "Month", as.vector(variables['month']))
    updateTextAreaInput(session, "type", "Type", as.vector(variables['type']))
    
  })
  
  # plot_image <- reactive({
  #   input$current_file
  #   img <- magick::image_read(input$current_file, density = 600)
  #   plot(img)
  # })
  # 
  # output$pdfImage <- renderPlot({
  #    plot_image()
  # }, height = 500, execOnResize = FALSE)
  
  plot_extractor <- reactive({
    input$current_file
    img <- magick::image_read(input$current_file)
    plot(img)
  })
  
  output$plot1 <- renderPlot({
    plot_extractor()
  }, height = 500, execOnResize=FALSE)

  observeEvent(input$clearData, {
    final_data <<- RestartData()
  })
  
  observeEvent(input$clearLast, {
    if (length(rows_to_delete) > 0) {
      to_clear <- pop(rows_to_delete)
      final_data <<- final_data[-c((nrow(final_data) - to_clear + 1):nrow(final_data)),]
    }
  })
  
  observeEvent(input$goButton, {
    if (CAN_SUBMIT) {
      junk <- dir(path=getwd(), pattern="cut.png|cut.jpg")
      file.remove(junk)
      CAN_SUBMIT <<- FALSE
      final <- input$intermediate_data
      updateTextAreaInput(session, "intermediate_data", "Extracted Data", "")
      
      final <- gsub(",", "", final)
      final <- strsplit(final, "\n | \n|\n")[[1]]
      
      final <- final[final != ""]
      
      # Split extracted data into a list with each level as a different day
      split_data <- splitAt(final, findDayIndex(final))
      start_index <- final_data_index
      
      rows_inserted <- 0
      
      
      # Loop through each date
      for (i in 1:length(split_data)) {
        
        # If only a date is in the list, skip it
        if (length(split_data[[i]]) == 1) {
          next
        }
        
        # The date is the first item in the list
        current_date <- split_data[[i]][1]
        
        
        # Loop through each item in the date, except the date
        for (j in 2:length(split_data[[i]])) {
          
          # Insert the item into final_data with all varaibles
          
          # Vegetarian
          temp <- strsplit(split_data[[i]][j], " ")
          if (input$vegetarian == TRUE & "V" %in% temp[[1]]) {
            vegetarian <- 1
            V_index <- match("V", temp[[1]])
            temp[[1]] <- temp[[1]][-V_index]
            split_data[[i]][j] <- paste(temp[[1]], collapse = " ")
          } else if (input$vegetarian == TRUE & "(V)" %in% temp[[1]]) {
            vegetarian <- 1
            V_index <- match("(V)", temp[[1]])
            temp[[1]] <- temp[[1]][-V_index]
            split_data[[i]][j] <- paste(temp[[1]], collapse = " ")
          } else {
            vegetarian <- 0
          }
          
          # Sodium
          temp <- split_data[[i]][j]
          temp <- gsub("\\(", " ", temp)
          temp <- gsub("\\)", " ", temp)
          temp <- gsub("mg ", " ", temp)
          temp <- strsplit(temp, " ")
          if (input$sodium == TRUE & "Sodium" %in% temp[[1]]) {
            sod_index <- match("Sodium", temp[[1]])
            temp[[1]] <- temp[[1]][-sod_index]
            sodium <- as.integer(temp[[1]][!is.na(as.integer(temp[[1]]))])
            sod_index <- match(as.character(sodium), temp)
            temp[[1]] <- temp[[1]][-sod_index]
            split_data[[i]][j] <- paste(temp[[1]], collapse = " ")
          } else if (input$sodium == TRUE) {
            temp <- split_data[[i]][j]
            sod <- regmatches(temp, gregexpr("\\(.*?\\)", temp))
            within_parenthases <- gsub("[\\(\\)]", "", sod[[1]])
            sodium <- as.integer(within_parenthases[!is.na(as.integer(within_parenthases))])
            temp <- gsub("\\(", " \\(", temp)
            temp1 <- strsplit(temp, " ")
            sod_index <- match(sod[[1]], temp1[[1]])
            temp1[[1]] <- temp1[[1]][-sod_index]
            if (length(sod[[1]]) == 0) {
              sodium <- " "
            } else {
              split_data[[i]][j] <- paste(temp1[[1]], collapse = " ")
            }
          } else {
            sodium <- " "
          }
          
          inserted <- nrow(final_data)
          insert_data <- c(paste(input$state, collapse = " "), paste(input$county, collapse = " "), input$year, input$type, input$month, as.numeric(current_date), split_data[[i]][j], vegetarian, sodium)
          insert_data <- as.data.frame(rbind(insert_data))
          colnames(insert_data) <- colnames(final_data)
          rownames(insert_data) <- c()
          # FIX THIS (TRYING TO AVOID ANY ROWS WITH JUST SODIUM)
          if (nchar(as.character(insert_data$Sodium)) > 0 & nchar(as.character(insert_data$Item)) < 3) {
            insert_data$Item <- final_data$Item[inserted]
            final_data <- final_data[-inserted,]
            final_data <- as.data.frame(rbind(final_data, insert_data))
            next
          }
          final_data <- as.data.frame(rbind(final_data, insert_data))
          final_data_index <- final_data_index + 1
          rows_inserted <- rows_inserted + 1
        }
      }
      final_data <<- final_data
      push(rows_to_delete, rows_inserted)
    }
  })
  
  
  data <- reactive({
    input$goButton | input$clearData | input$clearLast
    final_data
  })
  
  output$table <- DT::renderDataTable({
    data()
  })
  
  
  output$downloadData <- downloadHandler(filename = function(){"Full_Data.csv"},
                                         content = function(file){write.csv(data(), file, row.names = FALSE)})
}

shinyApp(ui, server)

