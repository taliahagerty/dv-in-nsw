# # cute, but unhelpful
# install.packages("oz")
# library(oz)
# nsw()

library(rvest)
library(rgdal)
library(leaflet)

path <- "./data/NSW_LGA_POLYGON_shp.shx"

library(raster)
nsw <- shapefile(path)

m1 <- match(tolower(nsw$NSW_LGA__3), tolower(latest$`Local Government Area`))

# # see which names to fix
# m <- nsw$NSW_LGA__3[is.na(m1)]
# m <- m[!grepl("UNINCORPORATED", m)]

nsw$value <- latest$value[m1]


factpal <- colorFactor("Spectral", nsw$value)

leaflet(nsw) %>% 
  addPolygons(fillColor = ~factpal(value), 
              label = paste(latest$lga[m1], latest$value[m1], sep = " : "),
              fillOpacity = 1, 
              weight = 1) %>% 
  addLegend(position = "bottomright", 
            pal = factpal, 
            values = ~value, 
            opacity = 1)
