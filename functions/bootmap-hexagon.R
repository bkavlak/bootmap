library(ggplot2)
library(hexSticker)
require(magick)

imgurl <- image_read("../images/icons/plants-vector-free-icon-set-15.png")
sticker(imgurl, package="bootmap",
        p_size=14,
        p_x =1,
        p_y = 1.4,
        p_color = "#FFFFFF",
        s_x=1, s_y=0.8,
        s_width=1, s_height = 1,
        h_fill = "#577042",
        h_color = "#FFFFFF",
        spotlight = TRUE,
        filename="../images/bootmap-15-hexagon.png",
        dpi = 300)
