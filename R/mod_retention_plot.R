#' retention_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_retention_plot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(outputId = ns("hamuro_heatmap"),
               height = "900px")
  )
}
    
#' retention_plot Server Functions
#'
#' @noRd 
mod_retention_plot_server <- function(id, dat, settings){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
    
    output[["hamuro_heatmap"]] <- renderPlot({
      
      # browser()
      splitteR::plot_hrates_heatmap(dat = dat(),
                                    hamuro_threshold = as.numeric(settings()[["hamuro_threshold"]]))
    })
  })
}
    
## To be copied in the UI
# mod_retention_plot_ui("retention_plot_1")
    
## To be copied in the server
# mod_retention_plot_server("retention_plot_1")
