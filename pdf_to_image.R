install.packages(pdftools, dependencies = TRUE)
library(pdftools)
myfiles <- list.files(path = getwd(), pattern = "*.pdf")
for (i in 1:length(myfiles)) {
  pdf_convert(myfiles[i], dpi = 300)
}
