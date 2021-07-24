library(ggplot2)
library(hexSticker)
require(magick)

imgurl <- image_read("../images/icons/plants-vector-free-icon-set-15.png")
sticker(imgurl, package="bootmap",
        p_size=10,
        p_x =1,
        p_y = 1.65,
        p_color = "#FFFFFF",
        s_x=1, s_y=1,
        s_width=1.7, s_height = 1.4,
        h_fill = "#577042",
        h_color = "#FFFFFF",
        spotlight = TRUE,
        filename="../images/bootmap-15-hexagon.png",
        dpi = 300)
