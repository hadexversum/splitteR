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
 
    plotOutput(outputId = ns("peptide_coverage_plot")),
    plotOutput(outputId = ns("subfragments_coverage_plot"))
  )
}
    
#' coverage_plots Server Functions
#'
#' @noRd 
mod_coverage_plots_server <- function(id, dat){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
    output[["peptide_coverage_plot"]] <- renderPlot({
     HaDeX::plot_coverage(dat[[1]]()) 
    })
    
    subsections <- reactive({ create_subsections(dat[[1]]()) })
    
    output[["subfragments_coverage_plot"]] <- renderPlot({
      plot_subs_cov(dat = dplyr::rename(subsections(), Sequence = sub_sequence, Start = sub_start, End = sub_end))
      
    })
    
    dat_subsections <- reactive({
      # browser()
      create_subsections_dataset(dat = dat[[1]](), subsections = subsections())
    })
    
  })
}
    
## To be copied in the UI
# mod_coverage_plots_ui("coverage_plots_1")
    
## To be copied in the server
# mod_coverage_plots_server("coverage_plots_1")
