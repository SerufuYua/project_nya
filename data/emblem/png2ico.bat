@echo off
echo Using ImageMagick (https://imagemagick.org) to convert PNGs to ICO
magick emblem_512.png emblem_256.png emblem_128.png emblem_064.png emblem_048.png emblem_032.png emblem_024.png emblem_016.png -define icon emblem.ico
pause
