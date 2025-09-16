#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    sidebarLayout(
    sidebarPanel(
    fluidPage(
        mod_input_data_ui("input_data"),
        mod_split_settings_ui("split_settings")
      )
      )
  ,
      mainPanel(
      fluidPage(
        # h1("main"),
        mod_coverage_plots_ui("coverage_plots"),
        plotOutput("subsections_plot"),
        p("Creating a downloadable file may take a while!"),
        downloadButton("download_subsections")
        
    )
    )
    )
  ,
      
    includeCSS(path = app_sys("app/utils/datatable.css")),
    includeCSS(path = app_sys("app/utils/selectize.css"))
    
    )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )
  add_resource_path(
    "utils",
    app_sys("app/utils")
  )
  
  ##
  library(dplyr)
  library(data.table)
  library(ggplot2)
  library(HaDeX)

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "splitteR"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    golem::activate_js()
  )
}
