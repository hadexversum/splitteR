#' sequence_work UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sequence_work_ui <- function(id) {
  ns <- NS(id)
  tagList(
    DTOutput(outputId = ns("seq_data"))
  )
}
    
#' sequence_work Server Functions
#'
#' @noRd 
mod_sequence_work_server <- function(id, dat, settings){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
    seqs <- reactive({
      
      dat() %>%
        select(Sequence, Start, End) %>%
        unique() 
      # %>%
      #   dplyr::rowwise() %>%
      #   mutate(seq_uptake = splitteR::get_sequence_bonds(sequence = Sequence))
    })
    
    output[["seq_data"]] <- renderDT({
      
      seqs()
      
    })
  })
}
    
## To be copied in the UI
# mod_sequence_work_ui("sequence_work_1")
    
## To be copied in the server
# mod_sequence_work_server("sequence_work_1")
