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

# read image -------------------------------------------------------

# read characters or noise
read_VideoScan_single <- function(file, what = "char") {
  output_file <- paste0(tempfile(tmpdir = getwd()), ".png")
  if(what == "char")
    im_cmd <- paste0('convert ', 
                     file, 
                     ' -fuzz 20% -fill white -opaque "#ff0300" -threshold 99.99% -crop 360x7+118+28\\! ', 
                     output_file)
  
  if(what == "noise")
    im_cmd <- paste0('convert ', 
                     file, 
                     ' -fuzz 10% -fill white -opaque "#ff39ff" -threshold 99.99% -crop 360x7+118+28\\! ', 
                     output_file)
  
  system(im_cmd)
  img <- readImage(output_file)
  unlink(output_file)
  slot(img, ".Data")
}


read_VideoScan <- function(file) {
  read_VideoScan_single(file, what = "char")- read_VideoScan_single(file, what = "noise")
}

# extract characters from image ----------------------------------

get_characters <- function(x) {
  char_pos <- matrix(c(1L:60*6 - 5, 1L:60*6 - 1), ncol = 2)
  
  lapply(1L:nrow(char_pos), function(pos_id) {
    # start and end of a character
    char_se <- char_pos[pos_id, ]
    list(char = which(x[char_se[1]:char_se[2], ] == 1),
         noise = which(x[char_se[1]:char_se[2], ] == -1)
    )
  })
}

# identify characters by comparing them to pattern --------------

identify_characters <- function(x, fonts)
  sapply(fonts[1L:10], function(single_font) 
    sapply(x, function(single_img) {
      clear_read <- length(intersect(single_img[["char"]], single_font))
      exceeding_font <- length(setdiff(single_img[["char"]], single_font))
      if(clear_read == 0 | exceeding_font > 0) {
        0
      } else {
        maybe_noise <- length(intersect(setdiff(single_font, single_img[["char"]]), single_img[["noise"]]))
        (clear_read + maybe_noise)/length(single_font)
      }
    })
  )

# get character and its confidence ------------------------------

get_readout <- function(x, indices) {
  readout <- apply(x[indices, ], 1, function(single_char) 
    names(which.max(single_char))) %>% 
    substr(3, 4)
  
  conf <- apply(x[indices, ], 1, function(single_char) 
    single_char[which.max(single_char)])
  
  data.frame(readout = readout, conf = conf)
}

# get two sets of numbers -----------------------------------
# pattern: number1_space_leftBracket_number2_space
# we treat leftBracket as space, it's id is spaces[1] + 1

read_characters <- function(x) {
  spaces <- which(rowSums(x) == 0)[c(1, 3)]
  rbind(data.frame(type = "number1", get_readout(x, 1:(spaces[1] - 1))),
        data.frame(type = "number2", get_readout(x, (spaces[1] + 2):(spaces[2] - 1)))
  )
}

# process whole VideoScan image -----------------------------

process_VideoScan <- function(img_name, thr = 0.9, fonts = fonts_VD) {
  img_dat <- read_VideoScan(img_name) %>% 
    get_characters() %>% 
    identify_characters(fonts = fonts) %>% 
    read_characters() %>% 
    group_by(type) %>% 
    mutate(final = ifelse(conf > thr, as.character(readout), "X"))
  
  c(name = img_name,
    number1 = paste0(filter(img_dat, type == "number1")[["final"]], collapse = ""),
    number2 = paste0(filter(img_dat, type == "number2")[["final"]], collapse = "")
  ) 
}

fonts_VD <- get_fonts("./fonts/png/")

images <- list.files("/home/michal/Dropbox/Zdjecia/")[grep(".bmp", list.files("/home/michal/Dropbox/Zdjecia/"))]

res09 <- t(sapply(images, function(i)
  process_VideoScan(paste0("/home/michal/Dropbox/Zdjecia/", i), fonts = fonts_VD)
)) %>% 
  data.frame() %>% 
  mutate(ratio = as.numeric(as.character(number1))/as.numeric(as.character(number2)))

res05 <- t(sapply(images, function(i)
  process_VideoScan(paste0("/home/michal/Dropbox/Zdjecia/", i), 0.5, fonts = fonts_VD)
)) %>% 
  data.frame() %>% 
  mutate(ratio = as.numeric(as.character(number1))/as.numeric(as.character(number2)))

write.csv(res09, file = "first_readout_09.csv", quote = FALSE, row.names = FALSE)
write.csv(res05, file = "first_readout_05.csv", quote = FALSE, row.names = FALSE)
