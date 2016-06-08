for file in /home/michal/Dropbox/Zdjecia/*.bmp
do
        name=${file##*/}
        base=${name%}
        convert "$file" -channel rgba -alpha set -fuzz 10% -fill none -opaque "#ff42ff" -write mpr:orig +delete mpr:orig -channel rgba -alpha set -fuzz 10% -fill none -opaque "#ff68ff" -write mpr:orig +delete mpr:orig -channel R -separate -crop 350x10+110+27\! "./small_pics/$name"
done