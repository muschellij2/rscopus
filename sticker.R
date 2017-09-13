library(hexSticker)
library(desc)
desc = desc::description$new()
package = desc$get("Package")
outline = "#3f8cb1"
background = "black"
sticker("icon.png",	
        package = package,
        h_fill = background,
        h_color = outline, 
        s_width = 0.7, 
        s_height = 0.25,
        s_x = 1,
        filename = "sticker.png")
