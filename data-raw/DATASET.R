# Load the osmdata package

library(osmdata)

# Load %>%

`%>%` <- magrittr::`%>%`

# Create a bounding box

bangalore_boundary <- data.frame(lat = c(12.964045),
                         long = c(77.585611)) %>%
  sf::st_as_sf(coords = c("long", "lat"),
               crs = 4326) %>%
  sf::st_transform(3857) %>%
  sf::st_buffer(.,
                500,
                endCapStyle = "SQUARE") %>%
  sf::st_transform(4326) %>%
  sf::st_make_valid()

usethis::use_data(bangalore_boundary,
                  overwrite = TRUE)

# Download buildings

bangalore_buildings <- osmdata::opq(bbox = sf::st_bbox(bangalore_boundary)) %>%
  osmdata::add_osm_feature("building") %>%
  osmdata::osmdata_sf() %>%
  purrr::pluck("osm_polygons") %>%
  dplyr::select(osm_id, geometry) %>%
  sf::st_make_valid()

usethis::use_data(bangalore_buildings,
                  overwrite = TRUE)

# Download highways

bangalore_highways <- osmdata::opq(bbox = sf::st_bbox(bangalore_boundary)) %>%
  osmdata::add_osm_feature("highway") %>%
  osmdata::osmdata_sf() %>%
  purrr::pluck("osm_lines") %>%
  dplyr::select(osm_id, highway, geometry) %>%
  sf::st_make_valid()

usethis::use_data(bangalore_highways,
                  overwrite = TRUE)
