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
 
    radioButtons(inputId = ns("selected_state"),
                 label = "Select state to see its coverage:",
                 choices = c("state_1", "state_2"),
                 selected = "state_1"
                 ),
    splitLayout(
    selectInput(inputId = ns("time_0"),
                label = "Select no deut timepoint",
                choices = c(0, 0.1)),
    selectInput(inputId = ns("time_100"),
                label = "Select FD timepoint",
                choices = c(1000, 1400))
    ),
    numericInput(inputId = ns("deut_part"),
                 label = "Deuterium concentration:",
                 value = 0.9),
    actionButton(inputId = ns("download_csv"),
                 label = "Download file")
    
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
      
      updateRadioButtons(session, inputId = "selected_state",
                         choices = states(),
                         selected = states()[1])
    })
    
    observe({
      
      updateSelectInput(session, inputId = "time_0", 
                        choices = unique(dat[[1]][["Exposure"]]),
                        selected = min(unique(dat[[1]][["Exposure"]])))
    })
    
    observe({
      
      updateSelectInput(session, inputId = "time_100", 
                        choices = unique(dat[[1]][["Exposure"]]),
                        selected = max(unique(dat[[1]][["Exposure"]])))
    })
 
    
  })
}