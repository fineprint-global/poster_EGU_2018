library(tidyverse)
library(raster) 
library(ggplot2) # Please install development version from Github: devtools::install_github("tidyverse/ggplot2")
library(sf)      # Please install development version from Github: devtools::install_github("r-spatial/sf")
library(stars)   # Please install development version from Github: devtools::install_github("r-spatial/stars")
library(sp)      # sp still needed to interface with raster (wating for stars)
library(ggthemes)
library(maps)
library(mapproj)
library(mapdata)
library(ggmap)
library(grid)

# Load functions 
sources_files <- list.files(path = "./R", pattern = "*.R", full.names = TRUE)
sources_files <- sapply(X = sources_files, FUN = source, local = .GlobalEnv)

# Download and process SPAm data 
spam_data_files <- download_process_spam(output_dir = "./output", quite = TRUE)

# Select material of larger production 
selected_crops <- spam_data_files %>% 
  dplyr::filter(crop_system == "Total", crop %in% c("Wheat", "Soybean", "Oil Palm", "Rice", "Sugar Cane", "Maize")) 

r_selected_crops <- selected_crops %>%
  .$file %>% 
  raster::stack()

names(r_selected_crops) <- selected_crops$crop

# Filter production smaller than 5Kt 
r_selected_crops[r_selected_crops < 10000] <- NA

# Read and process SNL data 
filename <- "EGU_poster_mining_data.xls"
path <- "./input"
data_raw <- readxl::read_excel(file.path(path, filename), na = c("na", "Na", "NA")) 
data_raw <- snl_tidy(data_raw, meas.unit = "tons")

filename <- "EGU_poster_mining_data_gold.xls"
path <- "./input"
data_raw_gold <- readxl::read_excel(file.path(path, filename), na = c("na", "Na", "NA"))
data_raw_gold <- snl_tidy(data_raw_gold, meas.unit = "oz")

# Convert Gold oz to tons and with other commodities 
data <- data_raw_gold %>% 
  dplyr::mutate(production = production * 2.835e-5) %>% 
  dplyr::mutate(production_unit = "tons") %>% 
  dplyr::bind_rows(data_raw, .) %>% 
  dplyr::filter(year == 2005)

# Creat sf (simple feature) object from SNL data 
snl_production <- data %>% 
  dplyr::filter(!is.na(lat) | !is.na(long)) %>% 
  sf::st_as_sf(coords = c("long", "lat"), crs = as.character(raster::crs(r_selected_crops)) ) 

# Create SNL grid 
snl_grid <- aggregate_snl_to_grid(sf_tbl = snl_production, r = r_selected_crops)
sf::write_sf(snl_grid, "./output/snl_grid.gpkg", delete_layer = TRUE)

# Merge SPAM and SNL data 
r_snl_production <- snl_to_raster(grid = snl_grid, r = r_selected_crops, attr = c("Copper", "Gold", "Iron.Ore", "Nickel"))

# Remove crop from mining cells 
r_snl_mask <- raster::stackApply(r_snl_production, indices = c(1), fun = sum, na.rm = TRUE, progress = 'text') > 0
r_snl_mask_crop <- raster::mask(r_selected_crops, r_snl_mask)

# Join SNL and SPAM layers 
r_snl_spam_production <- raster::stack(r_snl_mask_crop, r_snl_production)
raster::writeRaster(r_snl_spam_production, filename = "./output/snl_spam_production.tif", overwrite = TRUE)

# Select material of lagest production for each pixel 
r_snl_spam_largest_production <- raster::which.max(r_snl_spam_production)
r_snl_spam_largest_production[is.na(r_snl_spam_largest_production)] <- 0
raster::writeRaster(r_snl_spam_largest_production, filename = "./output/snl_spam_largest_production.tif", overwrite = TRUE)

# Create map legend colours 
products_legend <- tibble::tribble(
  ~Product,   ~colour1,  ~colour2,  ~colour3,  ~colour4,
  "Wheat",      "#b3cc33", "#e41a1c", "#66c2a5", "#66a61e",
  "Soybean",    "#e4a540", "#377eb8", "#fc8d62", "#e31a1c",
  "Oil.Palm",   "#b5d5b5", "#4daf4a", "#8da0cb", "#1b9e77",
  "Rice",       "#377eb8", "#377eb8", "#377eb8", "#7570b3",
  "Sugar.Cane", "#f781bf", "#f781bf", "#f781bf", "#f781bf",
  "Maize",      "#e31a1c", "#e31a1c", "#e31a1c", "#a6761d",
  "Copper",     "#be94e8", "#984ea3", "#e78ac3", "#d95f02",
  "Gold",       "#a4507d", "#ff7f00", "#a6d854", "#e6ab02", 
  "Iron.Ore",   "#c948a2", "#ffff33", "#ffd92f", "#377eb8",
  "Nickel",     "#be5b1d", "#a65628", "#e5c494", "#e7298a"
) %>% 
  dplyr::mutate(class_id = match(Product, names(r_snl_spam_production)))

# Set legend colour 
legend_colour <- products_legend$colour4
names(legend_colour) <- products_legend$Product

# Get data from raster 
df <- stars::read_stars("./output/snl_spam_largest_production.tif") %>% 
  as.data.frame(stars_snl_spam_largest_production) %>% 
  tibble::as_tibble() %>% 
  dplyr::filter(snl_spam_largest_production.tif > 0) %>% 
  dplyr::mutate(Product = as.integer(snl_spam_largest_production.tif)) %>% 
  dplyr::transmute(x = x, y = y, group = 1, Product = factor(x = Product, 
                                                             levels = products_legend$class_id, 
                                                             labels = products_legend$Product)) 

# Plot scale 
zoom_scale <- 12

make_bbox()

# Set bbox for zoom areas 
zoom_bbox <- tibble::tribble(
  ~region,                    ~x_lim,            ~y_lim,
  "Chile",         c(-76.00, -67.00), c(-45.00, -20.00),
  "South Africa",  c( 16.50,  32.50), c(-34.50, -22.50),
  "Mato Grosso",   c(-61.63, -50.22), c(-18.04,  -7.35),
  "Indonesia",     c( 95.50, 122.00), c( -8.50,   6.00)
) %>% 
  dplyr::mutate(geometry = lapply(seq_along(region), function(i) sf::st_multipoint(matrix(c(x_lim[[i]], y_lim[[i]]), nrow = 2))),
                group = 1,
                geometry = lapply(geometry, sf::st_bbox),
                geometry = lapply(geometry, sf::st_as_sfc),
                geometry = lapply(geometry, sf::st_geometrycollection),
                geometry = sf::st_sfc(geometry)) %>% 
  sf::st_sf() %>% 
  st_collection_extract()

# Get world map
map_world <- ggplot2::map_data(map = "world")
box_coords <- st_coordinates(zoom_bbox) %>% 
  tibble::as.tibble() %>% 
  dplyr::transmute(long = X, lat = Y, group = L2)
# Plot map layers background+products+borders
gp_global_map <- ggplot2::ggplot(map_world, aes(x = long, y = lat, group = group)) + 
  ggplot2::geom_polygon(fill = "#e3e3e3") +
  ggthemes::theme_map() +
  ggplot2::coord_quickmap(xlim = c(-160, 175)) + # For more acurate portion of the earth use ggplot2::coord_map (slow)
  ggplot2::geom_tile(data = df, aes(x = x, y = y, fill = Product)) +
  ggplot2::scale_fill_manual(values = legend_colour) + 
  ggplot2::geom_path(data = map_world, mapping = aes(long, lat), colour = "#6f7072", size = 0.1) +
  ggplot2::geom_path(data = box_coords, size = 0.8) 
  
gp_global_map + ggplot2::theme(legend.position = "bottom", plot.margin = grid::unit(c(0,0,0,0), "mm"))

# Save legend 
tmp <- ggplot_gtable(ggplot_build(gp_global_map)) 
leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
legend <- tmp$grobs[[leg]]
grDevices::svg(filename = "./output/plot_map_legend.svg", width = 15, height = 5, 
               family = "sans", bg = "transparent")
grid::grid.draw(legend) 
dev.off() 

# Save global map 
gp_global_map <- gp_global_map + ggplot2::theme(legend.position = "none")

# Add zoom bbox

ggplot2::ggsave("plot_global_map.tif", plot = gp_global_map, device = "tiff", path = "./output",
                scale = 1, width = 1189, height = 600, units = "mm", dpi = 300)



# Zoom to Chile 
chile_map <- ggplot2::map_data("world", region =  "Chile")
lim <- zoom_bbox %>% 
  dplyr::filter(region == "Chile") 
gp_chile_map <- ggplot2::ggplot(chile_map, aes(x = long, y = lat, group = group)) + 
  ggplot2::geom_polygon(fill = "#e3e3e3") +
  ggthemes::theme_map() +
  ggplot2::coord_quickmap(xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]]) + # For more acurate portion of the earth use ggplot2::coord_map (slow)
  ggplot2::geom_tile(data = df, aes(x = x, y = y, fill = Product)) +
  ggplot2::scale_fill_manual(values = legend_colour) + 
  ggplot2::geom_path(data = map_world, mapping = aes(long, lat), colour = "#6f7072", size = 0.1) +
  ggplot2::theme(legend.position = "bottom", plot.margin = grid::unit(c(0,0,0,0), "mm"))
gp_chile_map
gp_chile_map <- gp_chile_map + ggplot2::theme(legend.position = "none")
ggplot2::ggsave("plot_zoom_chile.tif", plot = gp_chile_map, device = "tiff", path = "./output",
                scale = 1, width = diff(lim$x_lim[[1]]) * zoom_scale, height = diff(lim$y_lim[[1]]) * zoom_scale, units = "mm", dpi = 300)

  
# Zoom to South Africa 
map_lesotho <- ggplot2::map_data("world", region =  "Lesotho") # workaround to remove Lesotho from map 
map_south_africa <- ggplot2::map_data("world", region =  "South Africa")
lim <- zoom_bbox %>% 
  dplyr::filter(region == "South Africa") 
gp_map_south_africa <- ggplot2::ggplot(map_south_africa, aes(x = long, y = lat, group = group)) + 
  ggplot2::geom_polygon(fill = "#e3e3e3") +
  ggthemes::theme_map() +
  ggplot2::coord_quickmap(xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]]) + # For more acurate portion of the earth use ggplot2::coord_map (slow)
  ggplot2::geom_tile(data = df, aes(x = x, y = y, fill = Product)) +
  ggplot2::scale_fill_manual(values = legend_colour) + 
  ggplot2::geom_polygon(data = map_lesotho, mapping = aes(x = long, y = lat, group = group), fill = "white") +
  ggplot2::geom_path(data = map_world, mapping = aes(long, lat), colour = "#6f7072", size = 0.1) +
  ggplot2::theme(legend.position = "bottom", plot.margin = grid::unit(c(0,0,0,0), "mm")) 
gp_map_south_africa

gp_map_south_africa <- gp_map_south_africa + ggplot2::theme(legend.position = "none")
ggplot2::ggsave("plot_zoom_south_africa.tif", plot = gp_map_south_africa, device = "tiff", path = "./output",
                scale = 1, width = diff(lim$x_lim[[1]]) * zoom_scale, height = diff(lim$y_lim[[1]]) * zoom_scale, units = "mm", dpi = 300)

# Zoom to MT Brazil 
download_file(url = "http://biogeo.ucdavis.edu/data/gadm2.8/shp/BRA_adm_shp.zip", destfile = "./output/BRA_adm_shp.zip")
utils::unzip(zipfile = "./output/BRA_adm_shp.zip", exdir = "./output/BRA_adm_shp")
map_adm_br <- sf::st_read("./output/BRA_adm_shp/BRA_adm1.shp")
map_mt <- map_adm_br %>% 
  dplyr::filter(NAME_1 == "Mato Grosso")
lim <- zoom_bbox %>% 
  dplyr::filter(region == "Mato Grosso") 
gp_map_brazil <- ggplot2::ggplot(map_adm_br) + 
  ggplot2::geom_sf(fill = "white", colour = "#6f7072", size = 0.1) +
  ggplot2::geom_sf(data = map_mt, fill = "#e3e3e3", colour = "#6f7072", size = 0.1) + 
  ggplot2::coord_sf(datum = NA, xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]]) +
  ggthemes::theme_map() +
  ggplot2::geom_tile(data = df, aes(x = x, y = y, fill = Product)) +
  ggplot2::scale_fill_manual(values = legend_colour) + 
  ggplot2::theme(legend.position = "bottom", plot.margin = grid::unit(c(0,0,0,0), "mm")) 
gp_map_brazil

gp_map_brazil <- gp_map_brazil + ggplot2::theme(legend.position = "none")
ggplot2::ggsave("plot_zoom_brazil.tif", plot = gp_map_brazil, device = "tiff", path = "./output",
                scale = 1, width = diff(lim$x_lim[[1]]) * zoom_scale, height = diff(lim$y_lim[[1]]) * zoom_scale, units = "mm", dpi = 300)



# Zoom to Indonesia 
map_indonesia <- ggplot2::map_data("world", region =  "Indonesia")
lim <- zoom_bbox %>% 
  dplyr::filter(region == "Indonesia")
gp_map_indonesia <- ggplot2::ggplot(map_indonesia, aes(x = long, y = lat, group = group)) + 
  ggplot2::geom_polygon(fill = "#e3e3e3") +
  ggthemes::theme_map() +
  ggplot2::coord_quickmap(xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]]) + # For more acurate portion of the earth use ggplot2::coord_map (slow)
  ggplot2::geom_tile(data = df, aes(x = x, y = y, fill = Product)) +
  ggplot2::scale_fill_manual(values = legend_colour) + 
  ggplot2::geom_path(data = map_world, mapping = aes(long, lat), colour = "#6f7072", size = 0.1) +
  ggplot2::theme(legend.position = "bottom", plot.margin = grid::unit(c(0,0,0,0), "mm")) 
gp_map_indonesia

gp_map_indonesia <- gp_map_indonesia + ggplot2::theme(legend.position = "none")
ggplot2::ggsave("plot_zoom_indonesia.tif", plot = gp_map_indonesia, device = "tiff", path = "./output",
                scale = 1, width = diff(lim$x_lim[[1]]) * zoom_scale, height = diff(lim$y_lim[[1]]) * zoom_scale, units = "mm", dpi = 300)



