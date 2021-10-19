## Testing tesseract package for extracting text from images
 library(tesseract)
 
 
 eng <- tesseract("eng")
 text <- tesseract::ocr("http://jeroen.github.io/images/testocr.png", engine = eng)
 cat(text)
 
 text <- tesseract::ocr("C:/Users/cmcneil/Downloads/Longevity Bar - Almond Lemon (Single Serving) Label.jpg", engine = eng)
 text <- tesseract::ocr("C:/Users/cmcneil/Downloads/Longevity Bar - Almond Lemon (Single Serving) Label.jpg", engine = eng, HOCR = T)
 cat(text)
 
 strings <- strsplit(text, "\n")
 
 
 testdat <- ocr_data("C:/Users/cmcneil/Downloads/Longevity Bar - Almond Lemon (Single Serving) Label.jpg")
 
 
# End script
 