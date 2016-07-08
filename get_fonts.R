# fonts are saved inside read_pic.R

# read fonts -------------------------------------------------------

# for (single_file in list.files("./fonts/png/")) {
#   img <- readImage(paste0("./fonts/png/", single_file))
#   img_name <- paste0("font_", strsplit(single_file, ".", fixed = TRUE)[[1]][1])
#   assign(img_name, img)
# }

get_fonts <- function(fonts_directory) {
  fonts <- lapply(list.files(fonts_directory), function(single_file) {
    img <- readImage(paste0(fonts_directory, single_file))
    slot(img, ".Data")[, , 1]
  })
  
  names(fonts) <- paste0("f_", unlist(strsplit(list.files("./fonts/png/"), ".png", fixed = TRUE)))
  
  # 1 is not properly save, need to add one row
  fonts[[2]] <- rbind(0, fonts[[2]])
  fonts_final <- lapply(fonts, function(i) 
    which(i == 1))
  fonts_final[[2]] <- fonts_final[[2]] - 1
  fonts_final
}

fonts_VD <- get_fonts("./fonts/png/")