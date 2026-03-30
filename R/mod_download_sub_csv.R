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
mod_download_sub_csv_server <- function(id, dat, settings){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
    states <- reactive(unique(dat()[["State"]]))
    
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
                         onclick ="window.open('https://hadex2.mslab-ibb.pl/')"),
            actionButton(inputId = ns("download_open_hradex"),
                         label = "Open HRaDeX",
                         icon = icon("th"),
                         onclick ="window.open('https://hradex.mslab-ibb.pl/')"),
            checkboxInput(inputId = ns("change_sequences"),
                          label = "Use sequence convention",
                          value = TRUE),
            p("Creating a downloadable file may take a while!"),
            p("At this moment, the download only works for unscaled data. This will change soon.")
            
          )
        )
      )
    }) %>% bindEvent(input[["get_downloads"]])
    
   ## data after sequence transformation
    
   current_dat <- reactive({
     
     if(input[["change_sequences"]]){
       replace_sequences(dat())
     } else dat()
     
   })
    
   download_dat <- reactive({
     
     lapply(input[["download_states"]], function(state){
       
       print(paste0("Creating data for ", state))
       
       state_dat <- filter(current_dat(), State == state)
       
       create_subsections_dataset(dat = state_dat, 
                                  subsections = create_subsections(state_dat))
     }) %>% bind_rows()
     
   })
    
    # download file
    
    output[["download_button"]] <- downloadHandler(

      filename = paste0("subsections_", unique(current_dat()[["Protein"]]), ".csv"),
      content = function(file){
        write.csv(download_dat(),
                  file = file,
                  quote = FALSE,
                  row.names = FALSE)}
    )
    
  })
}
    
