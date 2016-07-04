# pattern
# numbers_space_leftBracket_numbers_space_stringA_rightBracket
# stringA (parse ^2): objects/mm^2

library(EBImage)

# read fonts -------------------------------------------------------

# for (single_file in list.files("./fonts/png/")) {
#   img <- readImage(paste0("./fonts/png/", single_file))
#   img_name <- paste0("font_", strsplit(single_file, ".", fixed = TRUE)[[1]][1])
#   assign(img_name, img)
# }

fonts <- lapply(list.files("./fonts/png/"), function(single_file) {
  img <- readImage(paste0("./fonts/png/", single_file))
  slot(img, ".Data")[, , 1]
})
  
names(fonts) <- paste0("f_", unlist(strsplit(list.files("./fonts/png/"), ".png", fixed = TRUE)))
