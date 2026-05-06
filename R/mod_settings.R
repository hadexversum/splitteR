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
 
    splitLayout(
    radioButtons(inputId = ns("selected_state"),
                 label = "Select state to see its coverage:",
                 choices = c("state_1", "state_2"),
                 selected = "state_1"
                 ),
    div(
    span("Peptide bonds convention is used!", style="color:red"),
    numericInput(inputId = ns("hamuro_threshold"),
                 label = "Hamuro retention threshold:",
                 value = 0.3))
    ),
    splitLayout(
    selectInput(inputId = ns("time_0"),
                label = "Select no deut timepoint",
                choices = c(0, 0.1),
                selected = 0)
    ,
    selectInput(inputId = ns("time_100"),
                label = "Select FD timepoint",
                choices = c(1000, 1400),
                selected = 1400)
    ),
    splitLayout(
      
      numericInput(inputId = ns("deut_part"),
                   label = "Deuterium concentration:",
                   value = 0.9),
      div(
        selectInput(inputId = ns("rescalling_value"),
                    label = "Select rescalling value:",
                    choices = c("ret_scale", "ret_scale_2", "theo_ret"),
                    selected = "ret_scale"),
        p("This is a scalling value for UC in `Rescalling` tab."),
        checkboxInput(inputId = ns("if_rescaled"),
                      label = "Do you want to rescale the subfragment data?")
      )
      
    ),
    br()
  )
}
    
#' split_settings Server Functions
#'
#' @noRd 
mod_settings_server <- function(id, dat){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # observe({
    #   
    #   toggle("rescalling_value", condition = input[["if_rescaled"]])
    #   
    # })
    
    times <- reactive(unique(dat()[["Exposure"]]))
    
    observeEvent(dat(), {
      
      states <- reactive(unique(dat()[["State"]]))
      
      updateRadioButtons(session, inputId = "selected_state",
                         choices = states())
    })
    
    observeEvent(dat(), {
      
      updateSelectInput(session, inputId = "time_0", 
                        choices = times(),
                        selected = min(times()))
    })
    
    observeEvent(dat(), {
      
      updateSelectInput(session, inputId = "time_100", 
                        choices = times(),
                        selected = max(times()))
    })
 
  params <- reactive(
    data.frame(
      state = input[["selected_state"]],
      time_0 = as.numeric(input[["time_0"]]),
      time_100 = as.numeric(input[["time_100"]]),
      deut_part = as.numeric(input[["deut_part"]]),
      if_rescaled = input[["if_rescaled"]],
      rescalling_value = input[["rescalling_value"]],
      hamuro_threshold = as.numeric(input[["hamuro_threshold"]])
    )
  )  
    
  return(params)
    
  })
}