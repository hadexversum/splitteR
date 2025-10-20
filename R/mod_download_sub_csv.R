#' download_sub_csv UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_download_sub_csv_ui <- function(id) {
  ns <- NS(id)
  
  actionButton(
    inputId = ns("get_downloads"),
    label = "Download data"
  )
}
    
#' download_sub_csv Server Functions
#'
#' @noRd 
mod_download_sub_csv_server <- function(id, dat, dat_subsections){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
    states <- reactive(unique(dat[[1]]()[["State"]]))
    
    observe({
      showModal(
        modalDialog(
          title = "Export subfragment data for HaDeX2/HRaDeX",
          p("The data is not transfered automatically, please do it manually."),
          wellPanel(
            checkboxGroupInput(inputId = ns("download_states"),
                               label = "Select states to import:",
                               choices = states(),
                               selected = states()),
            downloadButton(outputId = ns("download_button"),
                           label = "Create file"),
            actionButton(inputId = ns("download_open_hadex2"),
                         label = "Open HaDeX2",
                         icon = icon("th"),
                         onclick ="window.open('https://hadex2.mslab-ibb.pl/)"),
            actionButton(inputId = ns("download_open_hradex"),
                         label = "Open HRaDeX",
                         icon = icon("th"),
                         onclick ="window.open('https://hradex.mslab-ibb.pl/)"),
            p("Creating a downloadable file may take a while!")
            
          )
        )
      )
    }) %>% bindEvent(input[["get_downloads"]])
    
    downlad_dat <- reactive({
      
      filter(dat_subsections(), State %in% input[["download_states"]])
      
    })
    
    file_name <- reactive({
      unique(dat[[1]]()[["Protein"]])
    })
    
    # download file
    
    output[["download_button"]] <- downloadHandler(

      filename = paste0("subsections_", file_name(), ".csv"),
      content = function(file){
        write.csv(downlad_dat(),
                  file = file,
                  quote = FALSE,
                  row.names = FALSE)}
    )
    
  })
}
    
