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
          title = "Export data for HaDeX2/HRaDeX",
          p("The data is not transfered automatically, please do it manually."),
          wellPanel(
            checkboxGroupInput(inputId = ns("download_states"),
                               label = "Select states to import:",
                               choices = states(),
                               selected = states()),
            checkboxInput(inputId = ns("change_sequences"),
                          label = "Use sequence convention",
                          value = TRUE),
            checkboxInput(inputId = ns("use_subfragments"),
                          label = "Export subfragments",
                          value = TRUE),
            checkboxInput(inputId = ns("use_rescaled"),
                          label = "Export rescaled values",
                          value = TRUE),
            br(),
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
            
            p("Creating a downloadable file may take a while!"),
            p("At this moment, the download only works for unscaled data. This will change soon.")
            
          )
        )
      )
    }) %>% bindEvent(input[["get_downloads"]])
    
   ## data after sequence transformation
    
   current_dat <- reactive({
     
     dat_1 <- if(input[["change_sequences"]]){
       replace_sequences(dat())
     } else dat()
     
     dat_2 <- if(input[["use_rescaled"]]){
       
       lapply(input[["download_states"]], function(state){
         
         # browser()
         
         ret_params <- create_retention_dataset(dat_1, state = state)
         create_rescaled_uptake_dataset(dat_1, ret_params, state)
         
       }) %>% bind_rows()
       
        
     } else {
       dat_1
     }
     
     dat_2
     
   })
   
   ##
   
   
   
  subs_dat <- reactive({
     
     lapply(input[["download_states"]], function(state){
       
       print(paste0("Creating data for ", state))
       
       state_dat <- filter(current_dat(), State == state)
       
       create_subsections_dataset(dat = state_dat, 
                                  subsections = create_subsections(state_dat))
     }) %>% bind_rows()
     
   })
  
  download_dat <- reactive({
    
    # browser()
    
    if(input[["use_subfragments"]]){
      subs_dat()
    } else {
      current_dat() %>%
        mutate(Fragment = "",
               RT = 1)
    }
    
  })
  
  download_filename <- reactive({
    
    if(input[["use_subfragments"]]){
      paste0("subsections_", unique(current_dat()[["Protein"]]), ".csv")
    } else {
      paste0("dat_", unique(current_dat()[["Protein"]]), ".csv")
    }
  })
    
    # download file
    
    output[["download_button"]] <- downloadHandler(

      filename = function(){ download_filename() } ,
      content = function(file){
        write.csv(download_dat(),
                  file = file,
                  quote = FALSE,
                  row.names = FALSE)}
    )
    
  })
}
    
