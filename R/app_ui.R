#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    sidebarLayout(
    sidebarPanel(
    fluidPage(
        mod_input_data_ui("input_data"),
        mod_settings_ui("split_settings")
      )
    ),
    mainPanel(
      fluidPage(
        navset_pill( 
          nav_panel("Coverage",
                    mod_coverage_plots_ui("coverage_plots"),
                    mod_download_sub_csv_ui("subfragments")
          ),
          nav_panel("UCs",
                    mod_table_plot_uc_ui("uptake_curves")
          ),
          nav_panel("Back-exchange",
                    mod_back_exchange_ui("bex")
          )
        )
      )
    )
    ),
    
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
  library(HaDeX2)
  library(bslib)
  library(DT)
  
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
