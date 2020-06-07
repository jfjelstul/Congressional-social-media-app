###########################################################################
# Josh Fjelstul, Ph.D.
# Congressional Social Media (CSM) Project
###########################################################################

# libraries
# library(rgdal)
# library(ggplot2)
# library(ggmap)
# library(mapproj)
# library(sp)
# library(rgeos)
# library(dplyr)
# library(stringr)
# library(cartogram)
# library(sf)
# library(ggiraph)

# data source
# https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.2017.html

# set working directory
# setwd("~/Dropbox/Data Science/Machine Learning Demo")

##################################################
# state map data
##################################################

generate_state_map_data <- function(file) {
  
  # read in data
  state_map_data <- read_sf(file)
  
  # apply projection
  state_map_data <- st_transform(state_map_data, crs = "+proj=merc")
  
  # rename variable
  state_map_data <- rename(state_map_data, state = NAME)
  
  # select variables
  state_map_data <- select(state_map_data, state, geometry)
  
  # drop DC and puerto rico
  state_map_data <- filter(state_map_data, !(state %in% c("District of Columbia", "Puerto Rico")))
  
  # return
  return(state_map_data)
}

##################################################
# district map data
##################################################

generate_district_map_data <- function(file) {
  
  # read in data
  district_map_data <- read_sf(file)
  
  # apply projection
  district_map_data <- st_transform(district_map_data, crs = "+proj=merc")
  
  # rename variable
  district_map_data <- rename(district_map_data, FIPS_code = STATEFP, district = CD115FP)
  
  # select variables
  district_map_data <- select(district_map_data, FIPS_code, district, geometry)
  
  # convert FIPS codes
  district_map_data$FIPS_code <- as.numeric(district_map_data$FIPS_code)
  
  # FIPS codes
  fips <- read.csv("data/FIPS-codes.csv", stringsAsFactors = FALSE)
  
  # merge in FIPS codes
  district_map_data <- left_join(district_map_data, fips, by = "FIPS_code")
  
  # drop DC and puerto rico
  district_map_data <- filter(district_map_data, !(FIPS_code %in% c(11, 72)))
  
  # district
  district_map_data$district <- str_c(district_map_data$state_short, district_map_data$district)
  
  # merge in distrct data
  district_map_data <- select(district_map_data, state, district, geometry) 
  
  # return
  return(district_map_data)
}

##################################################
# generate color palette
##################################################

generate_color_palette <- function(x, min_color, max_color) {
  
  # extract levels
  levels <- levels(x)
  
  # identify which levels are used in the fill variable
  used_levels <- as.character(unique(x))
  used_levels <- used_levels[!is.na(used_levels)]
  
  # numerical indexes for each level
  levels_indexes <- 1:length(levels)
  
  # indexes of the levels that are used
  used_level_indexes <- levels_indexes[which(levels %in% used_levels)]
  
  # create a palette
  palette <- colorRampPalette(c(min_color, "white", max_color))
  palette <- palette(length(levels))
  
  # choose colors from palette that are used
  colors <- palette[used_level_indexes]

  # return
  return(colors)
}

##################################################
# bounding box
##################################################

generate_bounding_box_state <- function(map_data, selected_state, percent = 0.2) {
  
  # bounding box
  selected_state_data <- filter(map_data, state == selected_state)
  bounding_box_data <- st_as_sfc(st_bbox(selected_state_data))
  
  # expand bounding box
  matrix <- bounding_box_data[[1]][[1]]
  xmin <- min(matrix[,1])
  xmax <- max(matrix[,1])
  ymin <- min(matrix[,2])
  ymax <- max(matrix[,2])
  
  # ranges
  x_range <- xmax - xmin
  y_range <- ymax - ymin
  
  # new limits
  out <- list(
    xmin = xmin - x_range * percent,
    xmax = xmax + x_range * percent,
    ymin = ymin - y_range * percent,
    ymax = ymax + y_range * percent
  )
  
  # return
  return(out)
}

generate_bounding_box_district <- function(map_data, selected_district, percent = 0.2) {
 
  # bounding box
  selected_district_data <- filter(map_data, district == selected_district)
  bounding_box_data <- st_as_sfc(st_bbox(selected_district_data))
  
  # expand bounding box
  matrix <- bounding_box_data[[1]][[1]]
  xmin <- min(matrix[,1])
  xmax <- max(matrix[,1])
  ymin <- min(matrix[,2])
  ymax <- max(matrix[,2])
  
  # ranges
  x_range <- xmax - xmin
  y_range <- ymax - ymin
  
  # new limits
  out <- list(
    xmin = xmin - x_range * percent,
    xmax = xmax + x_range * percent,
    ymin = ymin - y_range * percent,
    ymax = ymax + y_range * percent
  )
  
  # return
  return(out)
}

##################################################
# US map
##################################################

make_US_map <- function(x, selected_state = NULL, fill = NULL, box = FALSE, shaded = FALSE, tooltip = "state", background_color = "white", selected_color = "gray60", unselected_color = "gray80", NA_color = "gray90", min_color = blue, max_color = red) {

  # drop protected variables from provided data
  indexes_to_drop <- which(names(x) %in% c("geometry"))
  if(length(indexes_to_drop) > 0) {
    x <- x[,-indexes_to_drop]
  }

  # error message for selected state
  if(!is.null(selected_state)) {
    if(!(selected_state %in% state_map_data$state)) {
      stop("state string is not valid")
    }
  }
  
  # error message for fill variable
  if(!is.null(fill)) {
    if(!(fill %in% names(x))) {
      stop("fill variable is not in provided data frame")
    }
    if(class(as.data.frame(x)[,fill]) != "factor") {
      stop("fill variable needs to be a factor")
    }
  }
  
  # merge in data
  map_data <- left_join(state_map_data, x, by = "state")
  
  # Alaska map data
  alaska_map_data <- filter(map_data, state == "Alaska")
  alaska_map_data <- suppressWarnings(st_crop(alaska_map_data, xmin = -19945590, xmax = 0, ymin = 6626947, ymax = 11483370))
  
  # Hawaii map data
  hawaii_map_data <- filter(map_data, state == "Hawaii")
  
  # continential map data
  continental_map_data <- filter(map_data, !(state %in% c("Hawaii", "Alaska")))
  
  # make map base
  alaska_map <- ggplot()
  hawaii_map <- ggplot()
  continental_map <- ggplot()
  
  # if a fill variable is provided, use it
  if(!is.null(fill)) {
    alaska_map <- alaska_map + 
      geom_sf_interactive(data = alaska_map_data, aes_string(fill = fill, tooltip = tooltip, data_id = "state"), color = "black", size = 0.3) +
      scale_fill_manual(values = generate_color_palette(as.data.frame(alaska_map_data)[,fill], min_color, max_color), na.value = NA_color, guide = FALSE)
    hawaii_map <- hawaii_map + 
      geom_sf_interactive(data = hawaii_map_data, aes_string(fill = fill, tooltip = tooltip, data_id = "state"), color = "black", size = 0.3) +
      scale_fill_manual(values = generate_color_palette(as.data.frame(hawaii_map_data)[,fill], min_color, max_color), na.value = NA_color, guide = FALSE)
    continental_map <- continental_map + 
      geom_sf_interactive(data = continental_map_data, aes_string(fill = fill, tooltip = tooltip, data_id = "state"), color = "black", size = 0.3) +
      scale_fill_manual(values = generate_color_palette(as.data.frame(continental_map_data)[,fill], min_color, max_color), na.value = NA_color, guide = FALSE)
  }
  
  # otherwise apply the unselected color
  else {
    alaska_map <- alaska_map + 
      geom_sf_interactive(data = alaska_map_data, aes_string(tooltip = tooltip, data_id = "state"), fill = unselected_color, color = "black", size = 0.3)
    hawaii_map <- hawaii_map + 
      geom_sf_interactive(data = hawaii_map_data, aes_string(tooltip = tooltip, data_id = "state"), fill = unselected_color, color = "black", size = 0.3)
    continental_map <- continental_map + 
      geom_sf_interactive(data = continental_map_data, aes_string(tooltip = tooltip, data_id = "state"), fill = unselected_color, color = "black", size = 0.3)
  }
  
  # if shaded
  if(shaded) {
    if(selected_state == "Alaska") {
      alaska_map <- alaska_map + 
        geom_sf_interactive(data = alaska_map_data, aes_string(tooltip = tooltip, data_id = "state"), fill = unselected_color, color = "black", size = 0.3)
    } else if(selected_state == "Hawaii") {
      hawaii_map <- hawaii_map + 
        geom_sf_interactive(data = hawaii_map_data, aes_string(tooltip = tooltip, data_id = "state"), fill = unselected_color, color = "black", size = 0.3) 
    } else {
      continental_map <- continental_map + 
        geom_sf_interactive(data = filter(continental_map_data, state == selected_state), aes_string(tooltip = tooltip, data_id = "state"), fill = selected_color, color = "black", size = 0.3)
    }
  }
  
  # if bounding box option is selected and a district is selected
  if(!is.null(selected_state) & box) {
    
    # generate bounding box
    bounds <- generate_bounding_box_state(map_data, selected_state)
    
    # add bounding box to map
    if(selected_state == "Alaska") {
      alaska_map <- alaska_map + geom_rect(aes(xmin = bounds$xmin, xmax = bounds$xmax, ymin = bounds$ymin, ymax = bounds$ymax), fill = NA, color = "black", size = 0.5)
    } else if(selected_state == "Hawaii") {
      hawaii_map <- hawaii_map + geom_rect(aes(xmin = bounds$xmin, xmax = bounds$xmax, ymin = bounds$ymin, ymax = bounds$ymax), fill = NA, color = "black", size = 0.5)
    } else {
      continental_map <- continental_map + geom_rect(aes(xmin = bounds$xmin, xmax = bounds$xmax, ymin = bounds$ymin, ymax = bounds$ymax), fill = NA, color = "black", size = 0.5)
    }
  }
  
  # finish maps
  alaska_map <- alaska_map + coord_sf() +       my_map_theme(background_color = background_color) # + theme(plot.background = element_rect(fill = NA, color = "gray90"))
  hawaii_map <- hawaii_map + coord_sf() +       my_map_theme(background_color = background_color) # + theme(plot.background = element_rect(fill = NA, color = "gray90"))
  continental_map <- continental_map + coord_sf() +       my_map_theme(background_color = background_color) # + theme(plot.background = element_rect(fill = NA, color = "gray90"))
  
  # position maps
  map <- ggplot() +
    coord_equal(xlim = c(0, 10), ylim = c(0, 7), expand = FALSE) +
    annotation_custom(ggplotGrob(continental_map), xmin = 0, xmax = 10, ymin = 1, ymax = 7) +
    annotation_custom(ggplotGrob(alaska_map), xmin = 0, xmax = 3, ymin = 0, ymax = 2.5) +
    annotation_custom(ggplotGrob(hawaii_map), xmin = 3.25, xmax = 5, ymin = 0, ymax = 2) +
          my_map_theme(background_color = background_color) # + theme(plot.background = element_rect(fill = NA, color = "black"))
  
  # return
  return(map)
}

##################################################
# district map data
##################################################

make_US_district_map <- function(x, fill = NULL, box = FALSE, shaded = FALSE, tooltip = "state", background_color = "white", selected_color = "gray60", unselected_color = "gray80", NA_color = "gray90", min_color = blue, max_color = red) {
  
  # drop protected variables from provided data
  indexes_to_drop <- which(names(x) %in% c("state", "geometry"))
  if(length(indexes_to_drop) > 0) {
    x <- x[,-indexes_to_drop]
  }

  # error message for fill variable
  if(!is.null(fill)) {
    if(!(fill %in% names(x))) {
      stop("fill variable is not in provided data frame")
    }
    if(class(as.data.frame(x)[,fill]) != "factor") {
      stop("fill variable needs to be a factor")
    }
  }

  # merge in data
  map_data <- left_join(district_map_data, x, by = "district")
  
  # Alaska map data
  alaska_map_data <- filter(map_data, state == "Alaska")
  alaska_map_data <- suppressWarnings(st_crop(alaska_map_data, xmin = -19945590, xmax = 0, ymin = 6626947, ymax = 11483370))
  
  # Hawaii map data
  hawaii_map_data <- filter(map_data, state == "Hawaii")
  
  # continential map data
  continental_map_data <- filter(map_data, !(state %in% c("Hawaii", "Alaska")))

  # fill
  alaska_map <- ggplot() + 
    geom_sf_interactive(data = alaska_map_data, aes_string(fill = fill, tooltip = tooltip, data_id = "district"), color = "black", size = 0.3) +
    scale_fill_manual(values = generate_color_palette(as.data.frame(alaska_map_data)[,fill], min_color, max_color), na.value = NA_color, guide = FALSE)
  hawaii_map <- ggplot() + 
    geom_sf_interactive(data = hawaii_map_data, aes_string(fill = fill, tooltip = tooltip, data_id = "district"), color = "black", size = 0.3) +
    scale_fill_manual(values = generate_color_palette(as.data.frame(hawaii_map_data)[,fill], min_color, max_color), na.value = NA_color, guide = FALSE)
  continental_map <- ggplot() + 
    geom_sf_interactive(data = continental_map_data, aes_string(fill = fill, tooltip = tooltip, data_id = "district"), color = "black", size = 0.3) +
    scale_fill_manual(values = generate_color_palette(as.data.frame(continental_map_data)[,fill], min_color, max_color), na.value = NA_color, guide = FALSE)
  
  # finish maps
  alaska_map <- alaska_map + coord_sf() +       my_map_theme(background_color = background_color) # + theme(plot.background = element_rect(fill = NA, color = "gray90"))
  hawaii_map <- hawaii_map + coord_sf() +       my_map_theme(background_color = background_color) # + theme(plot.background = element_rect(fill = NA, color = "gray90"))
  continental_map <- continental_map + coord_sf() +       my_map_theme(background_color = background_color) # + theme(plot.background = element_rect(fill = NA, color = "gray90"))
  
  # position maps
  map <- ggplot() +
    coord_equal(xlim = c(0, 10), ylim = c(0, 7), expand = FALSE) +
    annotation_custom(ggplotGrob(continental_map), xmin = 0, xmax = 10, ymin = 1, ymax = 7) +
    annotation_custom(ggplotGrob(alaska_map), xmin = 0, xmax = 3, ymin = 0, ymax = 2.5) +
    annotation_custom(ggplotGrob(hawaii_map), xmin = 3.25, xmax = 5, ymin = 0, ymax = 2) +
          my_map_theme(background_color = background_color) # + theme(plot.background = element_rect(fill = NA, color = "black"))
  
  # return
  return(map)
}

##################################################
# state map
##################################################

# x <- state_data
# selected_state <- "Texas"
# fill <- "score_all"
# tooltip <- "state"

make_state_map <- function(x, selected_state = NULL, fill = NULL, tooltip = "state", background_color = "white", default_color = "gray80", NA_color = "gray90", min_color = blue, max_color = red) {
  
  # drop protected variables from provided data
  indexes_to_drop <- which(names(x) %in% c("geometry"))
  if(length(indexes_to_drop) > 0) {
    x <- x[,-indexes_to_drop]
  }
  
  # error message for selected district
  if(!is.null(selected_state)) {
    if(!(selected_state %in% state_map_data$state)) {
      stop("state string is not valid")
    } 
  }
  
  # error message for fill variable
  if(!is.null(fill)) {
    if(!(fill %in% names(x))) {
      stop("fill variable is not in provided data frame")
    } 
  }
  
  # merge in data
  map_data <- left_join(state_map_data, x, by = "state")
  
  # choose state
  map_data <- filter(map_data, state == selected_state)
  
  # crop Alaska if Alaska is selected
  if(selected_state == "Alaska") {
    map_data <- suppressWarnings(st_crop(map_data, xmin = -19945590, xmax = 0, ymin = 6626947, ymax = 11483370))
  }
  
  # if a fill variable is provided, use it
  if(!is.null(fill)) {
    
    # state map
    map <- ggplot() + 
      geom_sf_interactive(data = map_data, aes_string(fill = fill, tooltip = tooltip, data_id = "state"), color = "black", size = 0.3) +
      scale_fill_manual(values = generate_color_palette(as.data.frame(map_data)[,fill], min_color, max_color), na.value = NA_color, guide = FALSE) +
      my_map_theme(background_color = background_color)
  } 
  
  # otherwise, apply the default color
  else {
    # state map
    map <- ggplot() + 
      geom_sf_interactive(data = map_data, aes_string(tooltip = tooltip, data_id = "state"), fill = default_color, color = "black", size = 0.3) +
      my_map_theme(background_color = background_color)
  }

  # return
  return(map)
}

##################################################
# state map with districts
##################################################

make_district_map <- function(x, selected_state, selected_district = NULL, fill = NULL, tooltip = "district", box = FALSE, shaded = FALSE, background_color = "white", selected_color = "gray60", unselected_color = "gray80", NA_color = "gray90", min_color = blue, max_color = red) {
  
  # drop protected variables from provided data
  indexes_to_drop <- which(names(x) %in% c("state", "geometry"))
  if(length(indexes_to_drop) > 0) {
    x <- x[,-indexes_to_drop] 
  }
 
  # error message for selected state
  if(!(selected_state %in% district_map_data$state)) {
    stop("state string is not valid")
  }
  
  # error message for selected district
  if(!is.null(selected_district)) {
    if(!(selected_district %in% district_map_data$district)) {
      stop("district string is not valid")
    } 
  }
  
  # error message for fill variable
  if(!is.null(fill)) {
    if(!(fill %in% names(x))) {
      stop("fill variable is not in provided data frame")
    } 
    if(class(as.data.frame(x)[,fill]) != "factor") {
      stop("fill variable needs to be a factor")
    }
  }

  # merge in data
  map_data <- left_join(district_map_data, x, by = "district")
  
  # select map data
  map_data <- filter(map_data, state == selected_state)
  
  # crop Alaska if it is selected
  if(selected_state == "Alaska") {
    map_data <- suppressWarnings(st_crop(map_data, xmin = -19945590, xmax = 0, ymin = 6626947, ymax = 11483370))
  }

  # make the map
  map <- ggplot()
  
  # if a fill variable is provided
  if(!is.null(fill)) {

    # apply the color scale
    map <- map + 
      geom_sf_interactive(data = map_data, aes_string(fill = fill, tooltip = tooltip, data_id = "district"), color = "black", size = 0.3) +
      scale_fill_manual(values = generate_color_palette(as.data.frame(map_data)[,fill], min_color, max_color), na.value = NA_color, guide = FALSE)
  }
  
  # otherwise
  else {
    
    # apply the unselected color
    map <- map + geom_sf_interactive(data = map_data, aes_string(tooltip = tooltip, data_id = "district"), fill = unselected_color, color = "black", size = 0.3)
  }
  
  # if shaded
  if(shaded) {
    map <- map + geom_sf_interactive(data = filter(map_data, district == selected_district), aes_string(tooltip = tooltip, data_id = "district"), fill = selected_color, color = "black", size = 0.3) 
  }
  
  # if bounding box option is selected and a district is selected
  if(!is.null(selected_district) & box) {
    
    # generate bounding box
    bounds <- generate_bounding_box_district(map_data, selected_district)
    
    # add bounding box to map
    map <- map + geom_rect(aes(xmin = bounds$xmin, xmax = bounds$xmax, ymin = bounds$ymin, ymax = bounds$ymax), fill = NA, color = "black", size = 0.5)
  }
  
  # finish map
  map <- map + 
    coord_sf() + 
    my_map_theme(background_color = background_color)
  
  # return
  return(map)
}

##################################################
# single district map
##################################################

make_single_district_map <- function(x, selected_district = NULL, fill = NULL, tooltip = "district", background_color = "white", default_color = "gray80", NA_color = "gray90", min_color = blue, max_color = red) {
  
  # drop protected variables from provided data
  indexes_to_drop <- which(names(x) %in% c("geometry"))
  if(length(indexes_to_drop) > 0) {
    x <- x[,-indexes_to_drop]
  }
  
  # error message for selected district
  if(!is.null(selected_district)) {
    if(!(selected_district %in% district_map_data$district)) {
      stop("district string is not valid")
    } 
  }
  
  # error message for fill variable
  if(!is.null(fill)) {
    if(!(fill %in% names(x))) {
      stop("fill variable is not in provided data frame")
    } 
  }
  
  # merge in data
  map_data <- left_join(district_map_data, x, by = "district")
  
  # choose state
  map_data <- filter(map_data, district == selected_district)
  
  # crop Alaska if Alaska is selected
  if(selected_district == "AK00") {
    map_data <- suppressWarnings(st_crop(map_data, xmin = -19945590, xmax = 0, ymin = 6626947, ymax = 11483370))
  }
  
  # if a fill variable is provided, use it
  if(!is.null(fill)) {
    
    # state map
    map <- ggplot() + 
      geom_sf_interactive(data = map_data, aes_string(fill = fill, tooltip = tooltip, data_id = "district"), color = "black", size = 0.3) +
      scale_fill_manual(values = generate_color_palette(as.data.frame(map_data)[,fill], min_color, max_color), na.value = NA_color, guide = FALSE) +

            my_map_theme(background_color = background_color)
  } 
  
  # otherwise, apply the default color
  else {
    # state map
    map <- ggplot() + 
      geom_sf_interactive(data = map_data, aes_string(tooltip = tooltip, data_id = "district"), fill = default_color, color = "black", size = 0.3) +

            my_map_theme(background_color = background_color)
  }
  
  # return
  return(map)
}

##################################################
# inset maps
##################################################

# make_state_map_with_inset <- function(US_map, state_map) {
# 
#   # position maps
#   map <- ggplot() +
#     coord_equal(xlim = c(0, 10), ylim = c(0, 8), expand = FALSE) +
#     annotation_custom(ggplotGrob(state_map), xmin = 4.5, xmax = 10, ymin = 0, ymax = 7) +
#     annotation_custom(ggplotGrob(US_map), xmin = 0, xmax = 4, ymin = 4, ymax = 8) +
#     geom_segment(aes(x = 4.25, xend = 4.25, y = -Inf, yend = Inf), color = "gray90", size = 0.25) +
#     geom_segment(aes(x = 0, xend = 4.25, y = 4.25, yend = 4.25), color = "gray90", size = 0.25) +
#           my_map_theme(background_color = background_color) # + theme(plot.background = element_rect(fill = NA, color = "gray90"))
#   
#   # return map
#   return(map)  
# }

# make_single_district_map_with_inset <- function(US_map, state_map, district_map) {
#   
#   # position maps
#   map <- ggplot() +
#     coord_equal(xlim = c(0, 10), ylim = c(0, 8), expand = FALSE) +
#     annotation_custom(ggplotGrob(district_map), xmin = 4.5, xmax = 10, ymin = 0, ymax = 7) +
#     annotation_custom(ggplotGrob(US_map), xmin = 0, xmax = 4, ymin = 4, ymax = 8) +
#     annotation_custom(ggplotGrob(state_map), xmin = 0, xmax = 4, ymin = 0, ymax = 4) +
#     geom_segment(aes(x = 4.25, xend = 4.25, y = -Inf, yend = Inf), color = "gray90", size = 0.25) +
#     geom_segment(aes(x = 0, xend = 4.25, y = 4.25, yend = 4.25), color = "gray90", size = 0.25) +
#           my_map_theme(background_color = background_color) # + theme(plot.background = element_rect(fill = NA, color = "gray90"))
#   
#   # return map
#   return(map)  
# }

# make_delegation_map <- function(US_map, house_map, senate_map_1, senate_map_2) {
#   
#   # position maps
#   map <- ggplot() +
#     coord_equal(xlim = c(0, 10), ylim = c(0, 12), expand = FALSE) +
#     annotation_custom(ggplotGrob(house_map), xmin = 0, xmax = 10, ymin = 0, ymax = 8.5) +
#     annotation_custom(ggplotGrob(US_map), xmin = 0, xmax = 4, ymin = 9, ymax = 12) +
#     annotation_custom(ggplotGrob(senate_map_1), xmin = 4 + 0.15, xmax = 7 - 0.15, ymin = 9, ymax = 12) +
#     annotation_custom(ggplotGrob(senate_map_2), xmin = 7 + 0.15, xmax = 10 - 0.15, ymin = 9, ymax = 12) +
#           my_map_theme(background_color = background_color) # + theme(plot.background = element_rect(fill = NA, color = "gray90"))
#   
#   # return map
#   return(map)  
# }

##################################################
# inset map with state
##################################################

# make_inset_map_state <- function(state_map_data, selected_state) {
#   
#   # make maps
#   US_map <- make_US_map(state_map_data, selected_state)
#   state_map <- make_state_map(state_map_data, selected_state = selected_state)
#   
#   # position maps
#   map <- ggplot() +
#     coord_equal(xlim = c(0, 10), ylim = c(0, 7), expand = FALSE) +
#     annotation_custom(ggplotGrob(state_map), xmin = 4, xmax = 10, ymin = 0, ymax = 7) +
#     annotation_custom(ggplotGrob(US_map), xmin = 1, xmax = 4, ymin = 3, ymax = 7) +
#           my_map_theme(background_color = background_color)
#   # theme(plot.background = element_rect(fill = NA, color = "black"))
#   
#   # return
#   return(map)  
# }

##################################################
# make interactive
##################################################

make_interactive <- function(map, width, height) {
  
  # tooltip CSS
  tooltip_css <- "background-color:white;color:#414155;border:none;box-shadow: 0px 0px 20px rgba(0, 0, 0, .1);padding:8px 12px;border-radius:8px;"

  # conver to D3 map
  interactive_map <- girafe(
    print(map), width_svg = width, height_svg = height,
    options = list(
      opts_hover(css = ""),
      opts_selection(type = "none"),
      opts_tooltip(css = tooltip_css, offx = 20, offy = 20, opacity = 1, delay_mouseover = 100, delay_mouseout = 100),
      opts_toolbar(saveaspng = FALSE),
      opts_sizing(rescale = TRUE, width = 1)
    )
  ) 
  
  # return
  return(interactive_map)
}

##################################################
# initialize
##################################################

# generate state map data
state_map_data <- generate_state_map_data("cb_2017_us_state_20m")

# generate district map data
district_map_data <- generate_district_map_data("cb_2017_us_cd115_20m")

##################################################
# US map examples
##################################################

# # no fill
# make_US_map(state_data, selected_state = "Texas") %>% make_interactive()
# 
# # variable fill
# make_US_map(state_data, selected_state = "Texas", fill = "score_all") %>% make_interactive()
# 
# # variable fill, box
# make_US_map(state_data, selected_state = "Texas", fill = "score_all", box = TRUE) %>% make_interactive()
# 
# # variable fill, shaded
# make_US_map(state_data, selected_state = "Texas", fill = "score_all", shaded = TRUE) %>% make_interactive()
# 
# # variable fill, box, shaded
# make_US_map(state_data, selected_state = "Texas", fill = "score_all", box = TRUE, shaded = TRUE) %>% make_interactive()
# 
# # no fill, box
# make_US_map(state_data, selected_state = "Texas", box = TRUE, shaded = FALSE) %>% make_interactive()
# 
# # no fill, shaded
# make_US_map(state_data, selected_state = "Texas", shaded = TRUE) %>% make_interactive()
# 
# # no fill, box, shaded
# make_US_map(state_data, selected_state = "Texas", box = TRUE, shaded = TRUE) %>% make_interactive()
# 
# # error message for selected state
# make_US_map(state_data, selected_state = "x", box = TRUE)
# 
# # error message for fill variable
# make_US_map(state_data, selected_state = "Texas", fill = "x")

##################################################
# state map examples
##################################################

# # no fill
# make_state_map(state_data, selected_state = "Texas")
# 
# # variable fill
# make_state_map(state_data, selected_state = "Texas", fill = "score_all")
# 
# # error message for selected state
# make_state_map(state_data, selected_state = "x")
# 
# # error message for fill variable
# make_state_map(state_data, selected_state = "Texas", fill = "x")

##################################################
# single district map examples
##################################################

# # no fill
# make_single_district_map(district_data, selected_district = "TX31")
# 
# # variable fill
# make_single_district_map(district_data, selected_district = "TX31", fill = "score")
# 
# # error message for selected state
# make_single_district_map(district_data, selected_district = "TX00")
# 
# # error message for fill variable
# make_single_district_map(district_data, selected_district = "TX31", fill = "x")

##################################################
# district map examples
##################################################

# # no fill
# make_district_map(district_data, selected_state = "Texas") %>% make_interactive()
# 
# # variable fill
# make_district_map(district_data, selected_state = "Texas", fill = "score") %>% make_interactive()
# 
# # variable fill, box
# make_district_map(district_data, selected_state = "Texas", selected_district = "TX31", fill = "score", box = TRUE) %>% make_interactive()
# 
# # variable fill, shaded
# make_district_map(district_data, selected_state = "Texas", selected_district = "TX31", fill = "score", shaded = TRUE) %>% make_interactive()
# 
# # variable fill, box, shaded
# make_district_map(district_data, selected_state = "Texas", selected_district = "TX31", fill = "score", box = TRUE, shaded = TRUE) %>% make_interactive()
# 
# # no fill, box
# make_district_map(district_data, selected_state = "Texas", selected_district = "TX31", box = TRUE, shaded = FALSE) %>% make_interactive()
# 
# # no fill, shaded
# make_district_map(district_data, selected_state = "Texas", selected_district = "TX31", shaded = TRUE) %>% make_interactive()
# 
# # no fill, box, shaded
# make_district_map(district_data, selected_state = "Texas", selected_district = "TX31", box = TRUE, shaded = TRUE) %>% make_interactive()
# 
# # error message for selected state
# make_district_map(district_data, selected_state = "x",selected_district = "TX31", box = TRUE)
# 
# # error message for selected district
# make_district_map(district_data, selected_state = "Texas",selected_district = "TX00", box = TRUE)
# 
# # error message for fill variable
# make_district_map(district_data, selected_state = "Texas", fill = "x")

##################################################
# inset map examples
##################################################

# make_state_map_with_inset(
#   make_US_map(state_data, selected_state = "Texas", box = TRUE, shaded = TRUE),
#   make_district_map(district_data, selected_state = "Texas", fill = "score")
# ) %>% make_interactive()
# 
# make_state_map_with_inset(
#   make_US_map(state_data, selected_state = "California", box = TRUE, shaded = TRUE),
#   make_state_map(state_data, selected_state = "California", fill = "score_all")
# ) %>% make_interactive()
# 
# make_single_district_map_with_inset(
#   make_US_map(state_data, selected_state = "New York", box = TRUE, shaded = TRUE),
#   make_district_map(district_data, selected_state = "New York", selected_district = "NY21", box = TRUE, shaded = TRUE),
#   make_single_district_map(district_data, selected_district = "NY21", fill = "score")
# ) %>% make_interactive()

###########################################################################
# end R script
###########################################################################
