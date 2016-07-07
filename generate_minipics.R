# minipics are pictures of the characters as seen by the VSReader

pathway <- "/home/michal/Dropbox/Zdjecia/"
images_names <- list.files(pathway)[grep(".bmp", list.files(pathway))]

sapply(images_names, function(file) {
  output_file <- paste0("./minipics/mini_", file)
  im_cmd <- paste0('convert ', 
         paste0(pathway, file), 
         ' -fuzz 20% -fill white -opaque "#ff0300" -threshold 99.99% -crop 360x7+118+28\\! ', 
         output_file)
  system(im_cmd)
  print(paste0(file, "done"))
})
