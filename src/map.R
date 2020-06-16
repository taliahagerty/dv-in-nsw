# # cute, but unhelpful
# install.packages("oz")
# library(oz)
# nsw()

library(rvest)
library(rgdal)
library(leaflet)
library(raster)

path <- "./data/NSW_LGA_POLYGON_shp.shx"

nsw <- shapefile(path)

key <- match(tolower(nsw$NSW_LGA__3), tolower(latest$lga))

# # see which names to fix -- all matching now
# m <- nsw$NSW_LGA__3[is.na(key)]
# m <- m[!grepl("UNINCORPORATED", m)]

nsw$value <- latest$value[key]

# # leaflet version
# factpal <- colorNumeric("Spectral", nsw$value)
# 
# leaflet(nsw) %>% 
#   addPolygons(fillColor = ~factpal(value), 
#               label = paste(latest$lga[key], latest$value[key], sep = " : "),
#               fillOpacity = 1, 
#               weight = 1) %>% 
#   addLegend(position = "bottomright", 
#             pal = factpal, 
#             values = ~value, 
#             opacity = 1)


# static version

#generate colour scale, for negative peace use
nbins = 9
# plotclrs = (-heat.colors(nbins))
# plotclrs =  c( "#1A9850", "#91CF60", "#D9EF8B" ,"#FEE08B", "#FC8D59", "#D73027")

plotclrs = c("#d73027", "#e04f3d", "#e86853", "#ef8069", "#f59681", "#faac9a", "#fdc1b3", "#ffd7cd")
plotclrs = rev(plotclrs)

# check distribution
summary(latest$value)
hist(latest$value, breaks="FD")

# set the breaks manually
# the first bin should have the top five most peaceful states, 
# and the last bin should have the five least peaceful
# try to distribute the other states in groups of seven or eight
brks = c(1, 38.5, 71.75, 105, 172.55, 240.1, 296.0, 1002, max(latest$value)) 

class <- classIntervals(latest$value[key], n = nbins, style="fixed", fixedBreaks=brks)

colcode <- findColours(class, plotclrs) 

plot(nsw, col=colcode, lwd = 0.05, family = "Effra")
title("Domestic violence incidents by LGA, Apr 2019 - Mar 2020")
legend(x = "bottomright", legend=names(attr(colcode, "table")),
       fill=attr(colcode, "palette"), cex=0.8, bty="n",
       title = "Number of incidents")
