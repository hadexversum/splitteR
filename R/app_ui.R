#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    apply_ui_settings(),
    fluidPage(
      br(),
      sidebarLayout(
      sidebarPanel(
        class = "HaDeX-tab-content-element",
        br(),
        img(src='./www/prep_logo.png', width = "40%", align = "center"),
        br(),
        br(),
        span("Be careful! This app is still under development.  ", style="color:red"),
        fluidPage(
          # actionButton(inputId = "test", label = "browse!"),
            mod_input_data_ui("input_data"),
            mod_settings_ui("split_settings"),
            mod_download_sub_csv_ui("subfragments")
          )
        ),
        mainPanel(
          fluidPage(
            tabsetPanel( 
              tabPanel("Coverage",
                        mod_coverage_ui("coverage_plots")
                        
                        
              ),
              tabPanel("Uptake curves",
                        mod_table_plot_uc_ui("uptake_curves")
              ),
              tabPanel("Rescalling",
                        mod_rescale_ui("rescale")
              ),
              tabPanel("Sequences",
                       mod_sequence_work_ui("sequences")),
              tabPanel("Retention",
                       mod_retention_plot_ui("retention"))
            )
          )
      )
      )
    )
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
      app_title = "prepHaDeX"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    golem::activate_js()
  )
}

#' @noRd
apply_ui_settings <- function(){
  
  options(shiny.maxRequestSize = 10 * 1024^2) 
  
}
