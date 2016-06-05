convert Img_B2_000_Composed.bmp -channel rgba -alpha set -fuzz 10% -fill none -opaque "#ff42ff" -write mpr:orig +delete mpr:orig -channel rgba -alpha set -fuzz 10% -fill none -opaque "#ff68ff" -write mpr:orig +delete mpr:orig -channel R -separate -crop 350x10+110+27\! Img_B2_000_Composed2.bmp
tesseract Img_B2_000_Composed2.bmp tmp
