library(rgdal)
library(nlme)
library(maptools)
chile.
chile <- readShapePoly(system.file("Final_Shapefile_Chile.shp"))
chile.nb <- poly2nb(chile_r.rg)
chile.wl <- nb2listw(chile.nb)
centroids <- coordinates(chile_r.rg)
col.w <- nb2listw(chile_r.rg)

chile.nb <- dnearneigh(coords, 0, 475000, row.names(row.names))
fix(chile.nb)
chile.w <- nb2listw(chile.nb,style="W". zero.policy=TRUE))