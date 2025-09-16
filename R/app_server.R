#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  dat <- mod_input_data_server("input_data")
  
  mod_coverage_plots_server("coverage_plots", dat = dat)
  
  subsections <- reactive({ 
    create_subsections(dat[[1]]()) })
  
  output[["subsections_plot"]] <- renderPlot({
    
    plot_subs_cov(dat = dplyr::rename(subsections(), Sequence = sub_sequence, Start = sub_start, End = sub_end))
    
  })
  
  dat_subsections <- reactive({
    # browser()
    create_subsections_dataset(dat = dat[[1]](), subsections = subsections())
  })
  
  output[["download_subsections"]] <- downloadHandler(
    filename = "subsection_data.csv",
    content = function(file){
      write.csv(dat_subsections(), file, row.names = FALSE)
    }
  )
}
