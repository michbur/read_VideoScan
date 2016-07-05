# pattern
# numbers_space_leftBracket_numbers_space_stringA_rightBracket
# stringA (parse ^2): objects/mm^2

library(dplyr)
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
  im_cmd <- paste0('convert ', file, ' -channel rgba -alpha set -fuzz 10% -fill none -opaque "#ff42ff" -write mpr:orig +delete mpr:orig -channel rgba -alpha set -fuzz 10% -fill none -opaque "#ff68ff" -write mpr:orig +delete mpr:orig -channel R -separate -crop 360x7+118+28\\! ', output_file)
  system(im_cmd)
  img <- readImage(output_file)
  unlink(output_file)
  slot(img, ".Data")
}

img_dat <- read_VideoScan("/home/michal/Dropbox/Zdjecia/Img_B2_000_Composed.bmp")

char_pos <- matrix(c(1L:60*6 - 5, 1L:60*6 - 1), ncol = 2)

all_chars <- lapply(1L:nrow(char_pos), function(pos_id) {
  # start and end of a character
  char_se <- char_pos[pos_id, ]
  img_dat[char_se[1]:char_se[2], ]
})


