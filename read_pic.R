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

# read image -------------------------------------------------------

read_VideoScan <- function(file) {
  output_file <- paste0(tempfile(tmpdir = getwd()), ".png")
  im_cmd <- paste0('convert ', file, ' -channel rgba -alpha set -fuzz 10% -fill none -opaque "#ff42ff" -write mpr:orig +delete mpr:orig -channel rgba -alpha set -fuzz 10% -fill none -opaque "#ff68ff" -write mpr:orig +delete mpr:orig -channel R -separate -crop 350x10+110+27\\! ', output_file)
  system(im_cmd)
  img <- readImage(output_file)
  unlink(output_file)
  slot(img, ".Data")
}

read_VideoScan("/home/michal/Dropbox/Zdjecia/Img_B2_000_Composed.bmp")
