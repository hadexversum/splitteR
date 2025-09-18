#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  dat <- mod_input_data_server("input_data")
  
  mod_settings_server("split_settings", dat = dat)
  
  mod_coverage_plots_server("coverage_plots", dat = dat)
  
  mod_download_sub_csv_server("subfragments", dat = dat)

}
