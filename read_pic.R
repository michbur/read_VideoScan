# pattern
# numbers_space_leftBracket_numbers_space_stringA_rightBracket
# stringA (parse ^2): objects/mm^2

process_VideoScan <- function(pathway, thr = 0.5) {
  library(dplyr)
  library(EBImage)
  library(pbapply)
  
  # read image -------------------------------------------------------
  
  # read characters or noise
  read_VideoScan_single <- function(file, what = "char") {
    
    file_name <- tempfile(tmpdir = getwd())
    input_file <- paste0(file_name, ".bmp")
    output_file <- paste0(file_name, ".png")
    
    file.copy(file, input_file)
    
    if(what == "char")
      im_cmd <- paste0('convert ', 
                       input_file, 
                       ' -fuzz 20% -fill white -opaque "#ff0300" -threshold 99.99% -crop 360x7+118+28\\! ', 
                       output_file)
    
    if(what == "noise")
      im_cmd <- paste0('convert ', 
                       file, 
                       ' -fuzz 10% -fill white -opaque "#ff39ff" -threshold 99.99% -crop 360x7+118+28\\! ', 
                       output_file)
    
    system(im_cmd)
    img <- readImage(output_file)
    unlink(c(input_file, output_file))
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
  
  process_VideoScan_image <- function(img_name, thr = 0.9, fonts = fonts_VD) {
    img_dat <- read_VideoScan(img_name) %>% 
      get_characters() %>% 
      identify_characters(fonts = fonts) %>% 
      read_characters() %>% 
      group_by(type) %>% 
      mutate(final = ifelse(conf > thr, as.character(readout), "X"))
    
    c(path = img_name,
      number1 = paste0(filter(img_dat, type == "number1")[["final"]], collapse = ""),
      number2 = paste0(filter(img_dat, type == "number2")[["final"]], collapse = "")
    ) 
  }
  
  get_filename <- function(path) {
    splitted_path <- strsplit(as.character(path), "/", fixed = TRUE)
    sapply(splitted_path, function(i)
      i[length(i)])
  }
  
  fonts_VD <- structure(list(f_0 = c(2L, 3L, 4L, 6L, 10L, 11L, 14L, 15L, 16L, 18L, 
                                     20L, 21L, 22L, 25L, 26L, 30L, 32L, 33L, 34L), 
                             f_1 = c(4, 8, 9, 12, 14, 19, 24, 29, 34), 
                             f_2 = c(2L, 3L, 4L, 6L, 10L, 14L, 18L, 22L, 26L, 31L, 
                                     32L, 33L, 34L, 35L), 
                             f_3 = c(2L, 3L, 4L, 6L, 10L, 15L, 18L, 19L, 25L, 26L, 
                                     30L, 32L, 33L, 34L), 
                             f_4 = c(3L, 7L, 11L, 16L, 18L, 21L, 22L, 23L, 24L, 25L, 
                                     28L, 33L), 
                             f_5 = c(1L, 2L, 3L, 4L, 5L, 6L, 11L, 16L, 17L, 18L, 19L, 
                                     25L, 26L, 30L, 32L, 33L, 34L), 
                             f_6 = c(2L, 3L, 4L, 6L, 10L, 11L, 16L, 17L, 18L, 19L, 
                                     21L, 25L, 26L, 30L, 32L, 33L, 34L), 
                             f_7 = c(1L, 2L, 3L, 4L, 5L, 10L, 15L, 19L, 24L, 28L, 33L), 
                             f_8 = c(2L, 3L, 4L, 6L, 10L, 11L, 15L, 17L, 18L, 19L, 21L, 
                                     25L, 26L, 30L, 32L, 33L, 34L), 
                             f_9 = c(2L, 3L, 4L, 6L, 10L, 11L, 15L, 17L, 18L, 19L, 20L, 
                                     25L, 30L, 32L, 33L, 34L)), 
                        .Names = c("f_0", "f_1", "f_2", "f_3", "f_4", "f_5", "f_6", "f_7", "f_8", "f_9"))
  
  images_names <- list.files(pathway)[grep(".bmp", list.files(pathway))]
  
  readout <- t(pbsapply(images_names, function(i)
    process_VideoScan_image(paste0(pathway, i), thr = thr, fonts = fonts_VD)
  )) %>% 
    data.frame() %>% 
    mutate(ratio = as.numeric(as.character(number1))/as.numeric(as.character(number2)),
           doubt = ifelse(round(ratio, 4) == 0.1485, "", "doubtful"),
           name = get_filename(path)) %>% 
    select(name, number1, number2, ratio, doubt, path)
  
  doubtful_images <- paste0(filter(readout, doubt == "doubtful")[["name"]], collapse = ", ")
  write.csv2(readout, file = paste0(pathway, "readout.csv"), quote = FALSE, row.names = FALSE)
  message("Doubtful images: ", doubtful_images)
  invisible(readout)
}

#process_VideoScan("/home/michal/Dropbox/Zdjecia/")
