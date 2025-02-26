#' coverage_plots UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_coverage_plots_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
    plotOutput(outputId = ns("peptide_coverage_plot"))
  )
}
    
#' coverage_plots Server Functions
#'
#' @noRd 
mod_coverage_plots_server <- function(id, dat){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
    output[["peptide_coverage_plot"]] <- renderPlot({
      # browser()
     HaDeX::plot_coverage(dat[[1]]()) 
    })
    
  })
}
    
## To be copied in the UI
# mod_coverage_plots_ui("coverage_plots_1")
    
## To be copied in the server
# mod_coverage_plots_server("coverage_plots_1")
