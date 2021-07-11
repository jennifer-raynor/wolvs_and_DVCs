library(tidyverse)
library(rgdal)
library(sf)
library(maptools)
library(hrbrthemes)

rm(list = ls())

# ------------------------------------------------------------------------------
# parameters
# ------------------------------------------------------------------------------

# ******************************************************************************
# plot theme
# ******************************************************************************

# based on theme_ipsum_rc from 'brbrthemes' package
my_theme <- theme_ipsum_rc(base_family = "sans", base_size = 7,
                           axis_title_size = 7,
                           strip_text_size = 7, strip_text_face = "bold",
                           plot_margin = unit(c(5.5,5.5,5.5,5.5),"points"),
                           panel_spacing = unit(1, "lines"))+
  theme(panel.border = element_rect(colour = "#cccccc", fill = NA, size = 0.15),
        legend.position = "none",
        legend.title = element_blank(),
        legend.key = element_rect(fill = 'transparent', color = NA),
        legend.key.size = unit(0.5,"line"),
        legend.background = element_rect(fill = 'transparent', color = NA))

# ******************************************************************************
# county map for Wisconsin
# ******************************************************************************

# read map
# source: US Census Bureau
state.sp <- readOGR(dsn = "GIS/US_state_carto_shp",
                    layer = "cb_2018_us_state_20m")

state.sp <- state.sp[state.sp$STATEFP == "55", ]

# restructure for mapping in ggplot...

# add a new column termed "id" composed of the rownames of data
state.sp@data$id <- rownames(state.sp@data)

# create a data.frame from our spatial object
state.gg <- fortify(state.sp, region = "id")

# merge the "fortified" data with the data from our spatial object
state.gg <- merge(state.gg, state.sp@data, by = "id")

# ******************************************************************************
# wolf pack boundaries map
# ******************************************************************************

# read and re-project all wolf pack maps
wolf_pack.l <- map(c("1980_81", "1991_92", "2000_01", "2010_11"), function(yr){

  # read in shapefile
  wolf_pack.sp <- readOGR(dsn = "GIS/wolf_packs_shp",
                          layer = yr)

  # reproject to match county map
  wolf_pack.sp <- spTransform(wolf_pack.sp, proj4string(state.sp))

  # add year
  wolf_pack.sp@data$year <- str_sub(yr, 1, 4)

  # keep columns of interest
  wolf_pack.sp <- wolf_pack.sp[c("year")]

  return(wolf_pack.sp)

})

# combine in to single shapefile
wolf_pack.sp <- do.call(rbind, wolf_pack.l)

# restructure for mapping in ggplot...

# add a new column termed "id" composed of the rownames of data
wolf_pack.sp@data$id <- rownames(wolf_pack.sp@data)

# create a data.frame from our spatial object
wolf_pack.gg <- fortify(wolf_pack.sp, region = "id")

# merge the "fortified" data with the data from our spatial object
wolf_pack.gg <- merge(wolf_pack.gg, wolf_pack.sp@data, by = "id")

# ------------------------------------------------------------------------------
# create final figure
# ------------------------------------------------------------------------------

# map wolf packs
ggplot() +
  geom_polygon(data = state.gg,
               aes(x=long, y=lat, group = group), fill = "white", color = "black", size = 0.2) +
  geom_polygon(data = wolf_pack.gg,
               aes(x=long, y=lat, group = group, fill = "id"), color = "black", size = 0.15) +
  scale_fill_manual(values = "#e34a33", labels = c("Wolf pack")) +
  facet_wrap(.~year,ncol = 4) +
  coord_fixed(1.3) +
  guides(fill=guide_legend(title=NULL, size = 20)) +
  labs(x = "longitude", y = "latitude") +
  my_theme+
  theme(legend.position = c(0.17, 0.92))

ggsave("figures/01_out - fig1.pdf", units = "in", height = 2, width = 7, dpi = 900)

ggsave("figures/01_out - fig1.png", units = "in", height = 2, width = 7, dpi = 900)
