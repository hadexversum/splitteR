#' split_settings UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_split_settings_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
    checkboxGroupInput(inputId = ns(""),
                       label = "Select slicing method:",
                       choiceNames = c("Common N terminus", "Common C terminus"),
                       choiceValues = c(1, 2))
    
  )
}
    
#' split_settings Server Functions
#'
#' @noRd 
mod_split_settings_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_split_settings_ui("split_settings_1")
    
## To be copied in the server
# mod_split_settings_server("split_settings_1")
