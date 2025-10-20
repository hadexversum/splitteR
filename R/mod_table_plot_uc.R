#' table_plot_uc UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_table_plot_uc_ui <- function(id) {
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns("subsections_list")),
    ggiraph::girafeOutput(ns("subsections_uc"))
    
  )
}
    
#' table_plot_uc Server Functions
#'
#' @noRd 
mod_table_plot_uc_server <- function(id, dat, subsections, dat_subsections){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
    output[["subsections_list"]] <- DT::renderDataTable({
      
      DT::datatable(data = subsections(),
                    selection = "single")
    })
    
    output[["subsections_uc"]] <- ggiraph::renderGirafe({
      
      validate(need(!is.null(input[["subsections_list_rows_selected"]]), ""))
      i = input[["subsections_list_rows_selected"]]
      
      # browser()
      
      chosen_subsection <- subsections()[i, c("sub_sequence", "sub_start", "sub_end")]
      
      plt <- dat_subsections() %>%
        filter(Sequence == chosen_subsection[["sub_sequence"]],
               Start == chosen_subsection[["sub_start"]],
               End == chosen_subsection[["sub_end"]]) %>%
        ggplot() +
        geom_point(aes(x = Exposure, y = Center, color = State)) + 
        scale_x_log10() 
      
      
        girafe(ggobj = plt,
               width_svg = 9, height_svg = 5, opts_sizing(rescale = TRUE))
      
    })
    
  })
}
    
## To be copied in the UI
# mod_table_plot_uc_ui("table_plot_uc_1")
    
## To be copied in the server
# mod_table_plot_uc_server("table_plot_uc_1")
