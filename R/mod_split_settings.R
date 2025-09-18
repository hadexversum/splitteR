#' split_settings UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_settings_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
    radioButtons(inputId = "selected_state",
                 label = "Select state to see its coverage:",
                 choices = c("state_1", "state_2"),
                 selected = "state_1"
                 )
    
  )
}
    
#' split_settings Server Functions
#'
#' @noRd 
mod_settings_server <- function(id, dat){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    states <- reactive(unique(dat[[1]]()[["State"]]))
    
    observe({
      
      # browser()
      updateRadioButtons(session, inputId = ns("selected_state"),
                         choices = states(),
                         selected = states()[1])
    })
 
  })
}