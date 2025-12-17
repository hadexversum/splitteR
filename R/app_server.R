#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  dat <- mod_input_data_server("input_data")
  
  mod_settings_server("split_settings", dat = dat)
  
  subsections <- reactive({ create_subsections(dat[[1]]()) })
  
  mod_coverage_plots_server("coverage_plots", dat = dat, subsections = subsections)
  
  dat_subsections <- reactive({
    
    states <- unique(dat[[1]]()[["State"]])
    
    lapply(states, function(state){
      
      print(state)
      
      state_dat <- filter(dat[[1]](), State == state)
      
      create_subsections_dataset(dat = state_dat, 
                                 subsections = create_subsections(state_dat))
    }) %>% bind_rows()
    
  })
  
  mod_download_sub_csv_server("subfragments", dat = dat, 
                              dat_subsections = dat_subsections)
  
  mod_table_plot_uc_server("uptake_curves", dat = dat, 
                           subsections = subsections)
  
  mod_back_exchange_server("bex", dat = dat)

}
