# aRt_maps

library(mapview)
library(mapedit)
library(sf)
library(tidyverse)

# Make Some Points or Draw a Polygon --------------------------------------

# here we make points manually
yose <- tibble("site"=c("yose_valley_head", "clouds_rest"),
               "lon"= c(-119.71047, -119.46836), 
               "lat" = c(37.65552, 37.80483)) 
# make spatial with sf
yose <- st_as_sf(yose, coords = c("lon","lat"), crs=4326)
# and take a look using mapview
(m1 <- mapview(yose))

# Use MAPEDIT HERE!
# here we can draw a polygon around the area we are interested in with mapedit
# this opens a map and we draw some points or a polygon for what we want and click "Done!"
yose_poly <- drawFeatures(m1) # click Done!
class(yose_poly) # now have an sf feature

# look at plot
m1 + yose_poly


# Elevatr -----------------------------------------------------------------

library(elevatr)

# get raster of YOSEMITE VALLEY
ynp_elev <- elevatr::get_elev_raster(yose, z = 12,
                                clip = "bbox",
                                src="aws")

# tried z=14 and it took forever for rayshader stuff...this works better and still gives good resolution

# take a look at the extent using mapview
# do this first to make sure tile in right place
viewExtent(ynp_elev) + mapview(yose)

# then can view actual raster
mapview(ynp_elev)

# take look using cubeview (best with raster stack/time)
cubeview::cubeview(ynp_elev)

# Using {stars} package for ggplot ----------------------------------------

library(stars)
ynp_elev_df <- st_as_stars(ynp_elev) # make "stars" raster

# make some points for labels
sites <- c("El Capitan", "Half Dome", "Clouds Rest")
lats <- c(37.7339, 37.7459, 37.7677)
lons <- c(-119.6377, -119.5332, -119.4894)
yose_sites <- tibble(site=sites, lat=lats, lon=lons)

# ggplot
library(viridis)

gg1 <- ggplot() + 
    geom_stars(data = ynp_elev_df) +
    
    # add the corner points
    geom_sf(data=yose, color="orange", pch=16, size=5)+
    # site labels + points
    ggrepel::geom_label_repel(data=yose_sites, aes(x=lon, y=lat, label=site), nudge_y = c(-.02, -.02, .02), min.segment.length = .2)+
    #geom_point(data=yose_sites, aes(x=lon, y=lat), color="maroon", pch=16, size=3, alpha=0.5)+
    
    #ADD PICTURESSSSSSSSS BECAUSE WE CAN!
    ggimage::geom_image(data = yose_sites[1,], aes(x=lon, y=lat), image="https://upload.wikimedia.org/wikipedia/commons/thumb/d/d0/El_Capitan_Yosemite_72.jpg/345px-El_Capitan_Yosemite_72.jpg", size=0.1)+
    ggimage::geom_image(data = yose_sites[2,], aes(x=lon, y=lat), image="https://upload.wikimedia.org/wikipedia/commons/thumb/2/25/Half_dome_yosemite_national_park.jpg/640px-Half_dome_yosemite_national_park.jpg", size=.12)+
    ggimage::geom_image(data = yose_sites[3,], aes(x=lon, y=lat), image="https://upload.wikimedia.org/wikipedia/commons/thumb/5/5b/Yosemite_Valley%2C_as_seen_from_Cloud%27s_Rest.jpg/640px-Yosemite_Valley%2C_as_seen_from_Cloud%27s_Rest.jpg", size=.11)+
    
    #coord_equal() +
    coord_sf() +
    theme_void() +
    theme(legend.position = "bottom", legend.direction = "horizontal")+
    scale_fill_viridis("Elevation (m)")

# print it!
gg1 # This is cool

# Try 3d GGplot With Rayshader --------------------------------------------


# try with rayshader:
library(rayshader)

# need to drop photos
gg2 <- ggplot() + 
    geom_stars(data = ynp_elev_df) +
    # add the corner points
    geom_sf(data=yose, color="orange", pch=16, size=5)+
    # site labels + points
    #ggrepel::geom_label_repel(data=yose_sites, aes(x=lon, y=lat, label=site), nudge_y = c(-.02, -.02, .02), min.segment.length = .2)+
    geom_point(data=yose_sites, aes(x=lon, y=lat), color="maroon", pch=16, size=3, alpha=0.9)+
    coord_sf() +
    theme_void() +
    theme(legend.position = "bottom", legend.direction = "horizontal")+
    scale_fill_viridis("Elevation (m)")

gg2 # get a warning but works

# now make it 3d!
plot_gg(gg2, multicore=TRUE,width=6,height=5,scale=250,
        windowsize = c(1000, 800))


# Rayshader ---------------------------------------------------------------
# https://wcmbishop.github.io/rayshader-demo/
library(rayshader)

ynp_elev_rs <- raster_to_matrix(ynp_elev)

# flat rayshade
ynp_elev_rs %>%
  sphere_shade(sunangle = 45, colorintensity = 1.1, 
               texture="imhof4") %>% 
  plot_map()


# Custom Colors -----------------------------------------------------------

# custom colors!
coltexture1 <- create_texture(
  lightcolor = "gray90",
  shadowcolor = "gray30", 
  rightcolor =  "palegreen4",
  leftcolor = "seashell4", 
  centercolor = "darkslateblue")

# replot w colors and more color intensity
ynp_elev_rs %>%
  sphere_shade(sunangle = 45, colorintensity = 2, 
               texture = coltexture1) %>% 
  plot_map()


# 3D RayShade -------------------------------------------------------------

# make a 3d option
ynp_elev_rs %>%
  sphere_shade(texture = "imhof2", 
               colorintensity = 1.1) %>%
  add_water(detect_water(ynp_elev_rs), color = "imhof2") %>%
  plot_3d(ynp_elev_rs, zscale = 8, 
          fov = 0, theta = 300, 
          zoom = 0.85, phi = 45, 
          windowsize = c(1000, 800))
Sys.sleep(0.2)
render_snapshot(clear=TRUE)


# Hi Res Option -----------------------------------------------------------

# takes forever
# ynp_elev_rs %>%
#   sphere_shade(texture = "imhof2", colorintensity = 1.1) %>%
#   add_water(detect_water(ynp_elev_rs), color = "imhof2") %>%
#   plot_3d(ynp_elev_rs, zscale = 8, 
#           fov = 0, theta = 300, 
#           zoom = 0.85, phi = 45, 
#           windowsize = c(1000, 800))
# Sys.sleep(0.2)
#render_highquality() # don't do this unless you have lots of time, it made my computer really unhappy


# Adding Labels -----------------------------------------------------------

ynp_elev_rs %>%
  sphere_shade(texture = coltexture1) %>%
  add_water(detect_water(ynp_elev_rs), color = "imhof2") %>%
  # tracing takes a few moments
  add_shadow(ray_shade(ynp_elev_rs,
                       multicore = TRUE, # much faster
                       zscale = 3), 0.5) %>%
  #add_shadow(ambient_shade(ynp_elev_rs), 0) %>%
  plot_3d(ynp_elev_rs, zscale = 10, fov = 0, 
          theta = 300, zoom = 0.75, phi = 45, 
          windowsize = c(1000, 800)) 
Sys.sleep(0.2)

# labels
render_label(ynp_elev_rs, x = 460, y = 570, z = 20000, zscale = 50, text = "El Cap", textsize = 2, 
             textcolor = "orange", 
             linecolor = "gray15",
             linewidth = 4, clear_previous = TRUE)
render_label(ynp_elev_rs, x = 1030, y = 660, z=25000,
             zscale = 50, text = "Half Dome", 
             textcolor = "orange", 
             linecolor = "gray20",
             dashed = FALSE,
             textsize = 2, linewidth = 4,
             clear_previous = FALSE)
Sys.sleep(0.2)

# to save current view as static version
render_snapshot(clear=FALSE) 

Sys.sleep(0.2)

# make a movie!
render_movie(filename = "yosemite_default.mp4", 
             frames = 60, 
             title_text = "Yosemite Valley")

render_movie(filename = "yosemite_custom.mp4", 
             frames = 180, phi = 33,
             zoom = .2, 
             theta = 300, fov = 30, 
             title_text = "Yosemite Valley")


# Weird Effects -----------------------------------------------------------

ynp_elev_rs %>%
  sphere_shade(texture = "imhof2") %>%
  add_water(detect_water(ynp_elev_rs), color = "imhof2") %>%
  # tracing takes a few moments
  add_shadow(ray_shade(ynp_elev_rs,
                       multicore = TRUE, # much faster
                       zscale = 3), 0.5) %>%
  #add_shadow(ambient_shade(ynp_elev_rs), 0) %>%
  plot_3d(ynp_elev_rs, zscale = 10, fov = 30, 
          theta = 300, zoom = 0.3, phi = 25, 
          windowsize = c(1000, 800)) 
Sys.sleep(0.2)

# labels
render_label(ynp_elev_rs, x = 460, y = 570, z = 13000, zscale = 50, text = "El Cap", textsize = 2, 
             textcolor = "gray15", 
             linecolor = "gray15",
             linewidth = 4, clear_previous = TRUE)
render_label(ynp_elev_rs, x = 1030, y = 660, z=15000,
             zscale = 50, text = "Half Dome", 
             textcolor = "gray10", 
             linecolor = "gray10",
             dashed = FALSE,
             textsize = 2, linewidth = 4,
             clear_previous = FALSE)

# check field of view focus
render_depth(preview_focus = TRUE, 
             focus = 0.53, focallength = 100)

# on half dome
render_depth(preview_focus = FALSE, focus = 0.73,
             focallength = 300, clear = FALSE)

# on el cap
render_depth(preview_focus = FALSE, focus = 0.53,
             focallength = 100, clear = FALSE)

#render_snapshot(clear=TRUE) 

# close out
rgl::rgl.close()
