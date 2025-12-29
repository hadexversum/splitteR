#' input_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_input_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    # hadex_tab_plot(
    #   title = "Input data",
      
    wellPanel(
      fillRow(
        flex = c(NA, 1),
        fileInput(
          inputId = ns("data_file"),
          label = "Choose file:",
          multiple = FALSE,
          accept = c(".csv", ".xlsx", ".xls"),
          placeholder = "No file selected"
        ),
        div(
          id = "HaDeX-file-status-panel",
          h6("File status:"),
          div(
            id = "HaDeX-file-status-message",
            verbatimTextOutput(ns("data_file_info"))
          )
        )
      ),
      checkboxInput(inputId = ns("omit"),
                    label = "Omit first amino?",
                    value = TRUE),
      p("Omitting first amino is necessary when working with subsection to eliminate back-exchange effect.")
    )
    # )
  )
}
    
#' input_data Server Functions
#'
#' @noRd 
mod_input_data_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
    dat_raw <- reactive({
      data_file <- input[["data_file"]]
      
      if (is.null(data_file)) {
        # example_data_alpha
        HaDeX2::read_hdx(system.file(package = "splitteR", "app/data/alpha_uncut.csv"))
      } else {
        validate(need(try({
          file <- HaDeX2::read_hdx(data_file[["datapath"]])
        }), "File does not fullfill requirements. Check file requirements!"))
        file
      }
    })
    
    data_source <- reactive({ attr(dat_raw(), "source") })
    
    dat <- reactive({
      
      HRaDeX::omit_amino(dat = dat_raw(), omit = input[["omit"]])
             
    })
    
    ### other outputs
    
    output[["data_file_info"]] <- renderText({
      
      # paste0(
      #   if (is.null(input[["data_file"]]))
      #     "Example file: eEF1B_alpha.csv."
      #   else "Supplied file is valid.",
      #   "\nDetected data source: ", data_source(),
      #   if (data_source() == "HDeXaminer")
      #     ". User action needed below!"
      #   else "."
      # )
      # 
      
      if (is.null(input[["data_file"]]))
            "Example file: eEF1B_alpha.csv."
      else "Other data!"
    })
    
    dat_rt_raw <- reactive({
      data_file <- input[["data_file"]]
      
      if (is.null(data_file)) {
        # example_data_alpha
        read.csv(system.file(package = "splitteR", "./app/data/alpha_uncut.csv"))
      } else {
        validate(need(try({
          file <- read.csv(data_file[["datapath"]])
        }), "File does not fullfill requirements. Check file requirements!"))
        file
      }
    })
    
    
    dat_rt <- reactive({
      
      HRaDeX::omit_amino(dat = dat_rt_raw(), omit = input[["omit"]])
      
    })
    ### return values
    
    return(
      c(
        dat,
        list(input_info = reactive({
          data_file <- input[["data_file"]]
          
          if (is.null(data_file)) {
            list(is_example = TRUE)
          } else {
            list(
              is_example = FALSE,
              name = data_file[["name"]],
              hash = tools::md5sum(as.character(data_file[["datapath"]]))
            )
          }
          
        }),
        dat_rt
        )))
    
    
    
  })
  
} 
    
    