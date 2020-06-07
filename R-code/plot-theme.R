###########################################################################
# Josh Fjelstul, Ph.D.
# Congressional Social Media (CSM) Project
###########################################################################

##################################################
# plot theme
##################################################

my_theme <- function (title = 14, text = 10, background_color = "white") {
  
  out <- theme_minimal()
  
  text_color <- "#414155"
  
  out <- out + theme(
    # text = element_text(family = "Open Sans Light"),
    # plot.background = element_blank(),
    panel.grid.major = element_line(size = 0.25, color = "#E4E4ED", lineend = "round"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    # plot.title = element_text(color = text_color, hjust = 0.5, margin = margin(t = 0, r = 0, b = 25, l = 0), size = title, family = "Open Sans Light"),
    # axis.title.y = element_text(color = text_color, margin = margin(t = 0, r = 15, b = 0, l = 0), size = title, family = "Open Sans Light"),
    # axis.title.x = element_text(color = text_color, margin = margin(t = 15, r = 0, b = 0, l = 0), size = title, family = "Open Sans Light"),
    plot.title = element_text(color = text_color, hjust = 0.5, margin = margin(t = 0, r = 0, b = 25, l = 0), size = title),
    axis.title.y = element_text(color = text_color, margin = margin(t = 0, r = 15, b = 0, l = 0), size = title),
    axis.title.x = element_text(color = text_color, margin = margin(t = 15, r = 0, b = 0, l = 0), size = title),
    axis.text.y = element_text(color = text_color, margin = margin(t = 0, r = 15, b = 0, l = 0), size = text),
    axis.text.x = element_text(color = text_color, margin = margin(t = 15, r = 0, b = 0, l = 0), size = text),
    strip.background = element_blank(),
    # axis.line.x = element_line(size = 0.5, color = "black", lineend = "round"),
    # axis.line.y = element_blank(),
    # axis.ticks = element_line(size = 0.5, color = "black", lineend = "round"),
    # axis.line = element_blank(),
    axis.ticks = element_line(size = 0.25, color = "#E4E4ED", lineend = "round"),
    axis.ticks.length = unit(5, "pt"),
    strip.text = element_text(color = text_color, size = title),
    plot.margin = margin(t = 15, r = 25, b = 15, l = 15),
    legend.title = element_text(color = text_color, size = title),
    legend.text = element_text(color = text_color, size = text),
    legend.position = "bottom",
    legend.direction = "vertical",
    plot.background = element_rect(fill = background_color, color = NA)
  )
  
  return(out)
}

##################################################
# map theme
##################################################

my_map_theme <- function (title = 14, text = 10, background_color = "white") {
  
  out <- theme_minimal()
  
  text_color <- "#414155"
  
  out <- out + theme(
    text = element_text(family = "Open Sans Light"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    # plot.title = element_text(color = text_color, hjust = 0.5, margin = margin(t = 0, r = 0, b = 25, l = 0), size = title, family = "Open Sans SemiBold"),
    plot.title = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    strip.background = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    strip.text = element_blank(),
    plot.margin = margin(t = 15, r = 15, b = 15, l = 15),
    legend.title = element_blank(),
    legend.text = element_blank(),
    legend.position = "bottom",
    legend.direction = "vertical",
    plot.background = element_rect(fill = background_color, color = NA)
  )
  
  return(out)
}

##################################################
# function to add titles
##################################################

# titles function
my_titles <- function(x = NULL, y = NULL, main = NULL) {
  list(xlab(x), ylab(y), ggtitle(main))
}

##################################################
# tooltip CSS
##################################################

tooltip_css <- "background-color:white;color:#414155;border:none;box-shadow: 0px 0px 20px rgba(0, 0, 0, .1);padding:8px 12px;border-radius:8px;"

##################################################
# function to get dimensions
##################################################

get_dimensions <- function(plot) {
  width <- 0
  height <- 0
  build <- ggplot_build(plot)$data
  for(i in 1:length(build)) {
    width <- max(build[[i]]$xmax, width)
    height <- max(build[[i]]$ymax, height)
  }
  dim <- c(width, height)
  return(dim)
}

##################################################
# color palette
##################################################

# colors
# colors <- c("#3498DB", "#E74C3C", "#1ABC9C", "#9B59B6")

# make a color palette
# palette <- colorRampPalette(c("#82C786", "#51C5F7", "#877AD4"))
# blue <- "#2D8CFF"
# red <- "#FF4242"
blue <- "#3498db"
red <- "#e74c3c"
palette <- colorRampPalette(c(blue, "white", red))
palette <- palette(10)

##################################################
# function to make a D3 plot
##################################################

# convert_plot <- function(plot, width, height) {
#   tooltip_css <- "min-width:75px;background-color:white;color:#414155;border:none;box-shadow: 0px 0px 20px rgba(0, 0, 0, .1);padding:6px 18px;border-radius:8px;"
#   girafe(print(plot), width_svg = width, height_svg = height,
#          options = list(
#            opts_hover(css = "r:7;"),
#            opts_selection(type = "single", css = "fill:#414155;stroke:#414155;r:7;alpha:1;"),
#            opts_tooltip(css = tooltip_css, offx = 20, offy = 20, opacity = 1, delay_mouseover = 100, delay_mouseout = 100), 
#            opts_toolbar(saveaspng = FALSE), 
#            opts_sizing(rescale = TRUE, width = 1)
#          )
#   )
# }

##################################################
# function to make a D3 map
##################################################

# # function to make a map of Europe
# insert_map <- function(plot_data) {
#   
#   # get map projection
#   sp <- spTransform(getMap(resolution = "low"), CRS("+proj=wintri"))
#   
#   # crop the map
#   sp <- crop(sp, c( -1050000, 2500000, 3800000, 8000000))
#   
#   # convert to a data frame
#   map_data <- fortify(sp)
#   
#   # recode Cyprus
#   map_data$id[map_data$group == "Greece.2"] <- "Cyprus"
#   
#   # rename variables
#   names(plot_data) <- c("id", "var")
#   
#   # merge data
#   map_data <- left_join(map_data, plot_data, by = "id")
# 
#   # make tooltip variable
#   map_data$tooltip <- str_c(map_data$id, ": ", map_data$var)
#   map_data$tooltip[is.na(map_data$var)] <- map_data$id[is.na(map_data$var)]
# 
#   # make categories
#   map_data$cut <- cut(map_data$var, 10)
# 
#   # make a color palette  
#   palette <- colorRampPalette(c("#82C786", "#51C5F7", "#877AD4"))
# 
#   # filter
#   map_data <- filter(map_data, !is.na(map_data$var))
#   
#   # make ggplot object
#   map <- ggplot() +
#     geom_polygon_interactive(data = map_data, mapping = aes(x = long, y = lat, group = group, fill = cut, data_id = id, tooltip = tooltip), color = "white", size = 0.3) +
#     scale_fill_manual(values = palette(10), na.value = "#E0E5F1", guide = FALSE) +
#     coord_equal() +
#     theme_void()
#   
#   # tooltip CSS
#   tooltip_css <- "background-color:white;color:#414155;border:none;box-shadow: 0px 0px 20px rgba(0, 0, 0, .1);padding:6px 18px;border-radius:8px;"
#   
#   # conver to D3 map
#   map <- girafe(print(map), width_svg = 8, height_svg = 8,
#                 options = list(
#                   opts_hover(css = "stroke:white;fill:#BABACC;"),
#                   opts_selection(type = "single", css = "stroke:white;fill:#747487;"),
#                   opts_tooltip(css = tooltip_css, offx = 20, offy = 20, opacity = 1, delay_mouseover = 100, delay_mouseout = 100), 
#                   opts_toolbar(saveaspng = FALSE), 
#                   opts_sizing(rescale = TRUE, width = 1)
#                 )
#   )
#   
#   # return map
#   return(map)
# }

##################################################
# function to make a D3 chamber plot
##################################################

# functon to make a chamber plot
insert_chamber_plot <- function(plot_data, rows, limits = c(3, 10), blue, red) {

  plot_data <- filter(plot_data, !is.na(score))
  seats <- nrow(plot_data)
  
  # function to calculate coordinates
  get_coordinates <- function(seats, rows, limits) {
    
    # spread of dots
    radii <- seq(limits[1], limits[2], length.out = rows)
    
    # sets per row
    counts <- numeric(rows)
    
    # make an empty object
    out <- list()
    
    # loop over rows
    for(i in 1:rows) {
      
      # seats in row
      counts[i] <- round(seats * radii[i] / sum(radii[i:rows]))
      
      # angle
      theta <- seq(0, pi, length.out = counts[i])
      
      # seats remaining
      seats <- seats - counts[i]
      
      # make a data frame
      out[[i]] <- data.frame(
        x = radii[i] * cos(theta),
        y = radii[i] * sin(theta),
        row = i,
        theta = theta)
    }
    
    # stack data frames
    out <- bind_rows(out)
    
    # arrange
    out <- arrange(out, theta, row)
    
    return(out)
  }
  
  # get coordinates
  plot_coordinates <- get_coordinates(seats = seats, rows = rows, limits = c(3, 10))
  
  # merge in data
  # plot_data <- arrange(plot_data, desc(party), desc(score))
  plot_data <- arrange(plot_data, desc(party), desc(last))
  
  # tooltip
  plot_data$tooltip <- str_c(
    "<div class=plot-tooltip-score>", round(plot_data$score, 2), "</div>", 
    "<div class=plot-tooltip-member>", plot_data$last, "</div>", 
    "<div class=plot-tooltip-info>", "(", plot_data$party_short, "-", plot_data$district, ")", "</div>"
  )
  
  # cut scores into groups 
  plot_data$cut <- cut(plot_data$score, 10)
  
  # merge data
  plot_data <- cbind(plot_coordinates, plot_data)
  
  # make a color palette
  palette <- colorRampPalette(c(blue, "white", red))
  
  # make a ggplot
  plot <- ggplot(plot_data) + 
    geom_point(aes(x = x, y = y), color = "black", size = 2.25) +
    geom_point_interactive(aes(x = x, y = y, color = cut, tooltip = tooltip, data_id = tooltip), size = 1.25) + 
    scale_color_manual(values = palette(10), guide = FALSE) +
    coord_fixed() +
    theme_void()
  
  # conver to D3 plot
  interactive_plot <- girafe(
    print(plot), width_svg = 6, height_svg = 4,
    options = list(
      opts_hover(css = "stroke:#232333;fill:#232333;r:3;"),
      # opts_selection(type = "single", css = "stroke:232333;fill:#232333;r:5;"),
      opts_selection(type = "single"),
      opts_tooltip(css = tooltip_css, offx = 20, offy = 20, opacity = 1, delay_mouseover = 100, delay_mouseout = 100), 
      opts_toolbar(saveaspng = FALSE), 
      opts_sizing(rescale = TRUE, width = 1)
    )
  )
  
  # return
  return(interactive_plot)  
}

###########################################################################
# end R script
###########################################################################
