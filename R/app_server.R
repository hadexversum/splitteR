#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  dat <- mod_input_data_server("input_data")
  
  settings <- mod_settings_server("split_settings", dat = dat)
  
  subsections <- reactive({ create_subsections(dat()) })
  
  
  mod_coverage_server("coverage_plots", dat = dat, subsections = subsections)
  
  # observe({
  #   
  #   # input[["test"]]
  #   
  #   # browser()
  #   
  #   settings()[["state"]]
  #   settings()[["time_0"]]
  #   settings()[["time_100"]]
  #   settings()[["deut_part"]]
  #   settings()[["if_rescaled"]]
  #   
  # })
  
  dat_subsections <- reactive({
    
    states <- unique(dat()[["State"]])
    
    lapply(states, function(state){
      
      print(state)
      
      state_dat <- filter(dat(), State == settings()[["state"]])
      
      create_subsections_dataset(dat = state_dat, 
                                 subsections = create_subsections(state_dat))
    }) %>% bind_rows()
    
  })
  
  mod_download_sub_csv_server("subfragments", dat = dat, 
                              dat_subsections = dat_subsections)
  
  mod_table_plot_uc_server("uptake_curves", dat = dat, 
                           subsections = subsections)
  
  mod_rescale_server("rescale", 
                     dat = dat,
                     state = settings()[["state"]],
                     time_0 = settings()[["time_0"]],
                     time_100 = settings()[["time_100"]],
                     deut_part = settings()[["deut_part"]])

}
