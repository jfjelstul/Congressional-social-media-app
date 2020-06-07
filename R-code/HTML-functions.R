###########################################################################
# Josh Fjelstul, Ph.D.
# Congressional Social Media (CSM) Project
###########################################################################

##################################################
# general
##################################################

component_list <- function(...) {
  list(...)
}

create_site <- function(body) {
  htmlTemplate("index-template.html", body = body)
}

insert_HTML_text <- function(id) {
  html <- HTML_text %>% html_node(xpath = str_c("//*[@id=\"",id, "\"]")) %>% as.character()
  HTML(html)
}

insert_minor_heading <- function(text) {
  insert_row_center(tags$h4(text))
}

insert_major_heading <- function(text) {
  insert_row_center(tags$h3(text))
}

##################################################
# output widgets
##################################################

insert_text <- function(id) {
  div(id = id, class = "shiny-text-output")
}

insert_HTML <- function(id) {
  div(id = id, class = "shiny-html-output")
}

insert_plot <- function(id) {
  div(id = id, class = "shiny-plot-output", style="width:100%;height:100%;")
}

insert_plotly <- function(id) {
  div(id = id, class = "plotly html-widget html-widget-output shiny-report-size", style="width:100%;height:100%;")
}

insert_girafe <- function(id) {
  div(id = id, class = "girafe html-widget html-widget-output")
}

##################################################
# input widgets
##################################################

insert_contained_button <- function(id, label) {
  insert_container() %<%
    tags$button(id = id, type = "button", class = "btn btn-default action-button contained-button", label)
}

insert_outlined_button <- function(id, label) {
  insert_container() %<%
    tags$button(id = id, type = "button", class = "btn btn-default action-button outlined-button", label)
}

insert_text_button <- function(id, label) {
  insert_container() %<%
    tags$button(id = id, type = "button", class = "btn btn-default action-button text-button", label)
}

insert_download_button <- function(id, label) {
  insert_container() %<%
    tags$a(id = id, class = "btn btn-default shiny-download-link outlined-button", href = "", target = "_blank", download = NA, NULL, label)
}

insert_contained_button_link <- function(id, label, url) {
  insert_container(
    tags$a(id = id, class = "btn btn-default shiny-download-link contained-button", href = url, label)
  )
}

insert_header_row <- function(...) {
  div(class = "header-row", ...)
}


insert_header_button <- function(id, label, url) {
  tags$a(id = id, class = "btn btn-default shiny-download-link header-button", href = url, label)
}

insert_text_box <- function(id, placeholder, width) {
  insert_container() %<%
  div(class = "form-group shiny-input-container") %<%
    tags$input(id = id, type = "text", class = "form-control", value = "", placeholder = placeholder, style = str_c("width:", width, ";"))
}

insert_text_area <- function(id, placeholder, width, height) {
  insert_container() %<%
    div(class = "form-group shiny-input-container") %<%
    tags$textarea(id = id, type = "text", class = "form-control", value = "", placeholder = placeholder, style = str_c("resize:none;width:", width, ";height:", height, ";"))
}

insert_menu <- function(id, choices) {
  choice_list <- list()
  for(i in 1:length(choices)) {
    choice_list[[i]] <- tags$option(value = choices[i], choices[i])
  }
  insert_container(
    div(
      class = "form-group shiny-input-container",
      tags$label(class = "control-label", `for` = "id"),
      div(
        tags$select(id = id) %<% list(
          choice_list
        ),
        tags$script(type = "application/json", `data-for` = id, `data-nonempty` = "", "{}")
      )
    )
  )
}

insert_switch <- function(id, label) {
  insert_container %<%
    div(class = "switch") %<% list(
      tags$input(id = id, type = "checkbox", class = "switch-input"),
      tags$label(`for` = id, class = "switch-label") %<% list(
        div(class = "switch-label-text", label)
      )
    )
}

insert_plot_container <- function(plot, height = 600, ratio = 0.7) {
  width <- round(height / ratio)
  div(
    class = "plot-container",
    style = str_c("height:", height, "px;width:", width, "px;"),
    plot
  )
}

insert_banner <- function(...) {
  div(class = "banner", ...) 
}

##################################################
# title bar
##################################################

insert_app_header <- function(...) {
  div(
    class = "header-container",
    div(
      class = "header-content", 
      ...
    )
  )
}

insert_title_bar <- function(tabs) {
  tab_list <- list()
  for(i in 1:length(tabs)) {
    if(i == 1) {
      tab_list[[i]] <- tags$li(class = "active") %<% tags$a(href = str_c("#tab-1-", i), `data-toggle` = "tab", tabs[i])
    } else {
      tab_list[[i]] <- tags$li() %<% tags$a(href = str_c("#tab-1-", i), `data-toggle` = "tab") %<% div(class = "tab-text", tabs[i]) 
    }
  }
  tags$nav(class = "navbar navbar-default navbar-static-top title-bar-container", role = "navigation") %<%
    tags$ul(class = "nav navbar-nav", `data-tabsetid` = "1") %<% tab_list
}

insert_branding <- function(...) {
  div(
    class = "branding-container",
    div(
      class = "branding-content", 
      ...
    )
  )
}

##################################################
# spacing
##################################################

insert_spacer <- function(height = 100) {
  div(class = "spacer", style = str_c("height:", height, "px;"))
}

insert_divider <- function() {
  div(class = "divider-container") %<%
    tags$hr()
}

insert_container <- function(...) {
  div(class = "container", ...)
}

##################################################
# page layout
##################################################

insert_page_container <- function(...) {
  div(class = "tab-content", `data-tabsetid` = "1", ...)
}

insert_full_page <- function(...) {
  div(class = "full-page-container") %<%
    div(class = "full-page-content", ...)
}

insert_page <- function(page, active = FALSE, ...) {
  div(class = ifelse(active, "tab-pane active", "tab-pane"), id = str_c("tab-1-", page), ...)
}

insert_main <- function(...) {
  div(class = "main-container", div(class = "main-content", ...))
}

insert_side_bar <- function(...) {
  div(class = "side-bar-container") %<%
    div(class = "side-bar-content", ...)
}

insert_floating_side_bar <- function(...) {
  div(class = "floating-side-bar-container") %<%
    div(class = "side-bar-content", ...)
}

insert_block <- function(...) {
  div(
    class = "block-container",
    div(
      class = "block-content",
      ...
    )
  )
}

insert_block_shaded <- function(...) {
  div(
    class = "block-container-shaded",
    div(
      class = "block-content",
      ...
    )
  )
}

insert_footer <- function(...) {
  div(class = "footer-container", ...)
}

##################################################
# rows
##################################################

insert_row_center <- function(...) {
  div(class = "row-center", ...)
}

insert_row_evenly <- function(...) {
  div(class = "row-evenly", ...)
}

insert_row_around <- function(...) {
  div(class = "row-around", ...)
}

insert_row_left <- function(...) {
  div(class = "row-left", ...)
}

insert_row_right <- function(...) {
  div(class = "row-right", ...)
}

insert_text_block <- function(...) {
  div(class = "text-block", ...)
}

##################################################
# cards
##################################################

insert_card <- function(image_height, body_height, width, image = NULL, body = NULL) {
  div(class = "card-container", style = str_c("width:", width, ";")) %<% component_list(
    div(class = "card-image", style = str_c("height:", image_height, ";"), image),
    div(class = "card-body", style = str_c("height:", body_height, ";"), body)
  )
}

insert_invisible_card <- function(image_height, body_height, width, image = NULL, body = NULL) {
  div(class = "card-container-invisible", style = str_c("width:", width, ";")) %<% component_list(
    div(class = "card-image-invisible", style = str_c("height:", image_height, ";"), image),
    div(class = "card-body-invisible", style = str_c("height:", body_height, ";"), body)
  )
}

###########################################################################
# end R script
###########################################################################
