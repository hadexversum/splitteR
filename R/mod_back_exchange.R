#' back_exchange UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_back_exchange_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(inputId = ns("FD"),
                label = "Select FD timepoint",
                choices = c(1000, 1400)),
    DTOutput(outputId = ns("bex_data"))
 
  )
}
    
#' back_exchange Server Functions
#'
#' @noRd 
mod_back_exchange_server <- function(id, dat){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    observe({
      
      times <- unique(dat[[1]]()[["Exposure"]])
      
      updateSelectInput(inputId = "FD",
                        choices = times,
                        selected = times[length(times)])
    })
    
    bex_dat <- reactive({
      
      HaDeX2::calculate_back_exchange(dat = dat[[1]](),
                                      time_100 = as.numeric(input[["FD"]])) 
      
    })
    
    ##
    
    output[["bex_data"]] <- renderDT({
      
      bex_dat() %>%
        mutate(back_exchange = round(back_exchange, 4),
               err_back_exchange = round(err_back_exchange, 4))
      
    })
 
  })
  
  
}
    
## To be copied in the UI
# mod_back_exchange_ui("back_exchange_1")
    
## To be copied in the server
# mod_back_exchange_server("back_exchange_1")
