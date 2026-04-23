#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  dat_raw <- mod_input_data_server("input_data")
  
  dat <- reactive({
    
    splitteR::replace_sequences(dat = dat_raw(),
                                threshold = settings()[["hamuro_threshold"]])
    
  })
  
  settings <- mod_settings_server("split_settings", dat = dat_raw)
  
  subsections <- reactive({ create_subsections(dat(),
                                               use_convention = TRUE) })
  
  mod_coverage_server("coverage_plots", dat = dat, subsections = subsections)
  
  # observe({
  # 
  #   input[["test"]]
  # 
  #   # browser()
  # 
  #   # settings()[["state"]]
  #   # settings()[["time_0"]]
  #   # settings()[["time_100"]]
  #   # settings()[["deut_part"]]
  #   # settings()[["if_rescaled"]]
  #   # settings()[["hamuro_threshold"]]
  # })
  # 

  rescale_params <- reactive({
    
    create_retention_dataset(dat(),
                             state = settings()[["state"]],
                             time_0 = settings()[["time_0"]],
                             time_100 = settings()[["time_100"]],
                             deut_part = settings()[["deut_part"]])
    
  })
  
  dat_rescaled <- reactive({

    create_rescaled_uptake_dataset(dat(),
                                   rescale_params(),
                                   time_0 = settings()[["time_0"]],
                                   time_100 = settings()[["time_100"]],
                                   deut_part = settings()[["deut_part"]])
  })
  
  dat_subsections <- reactive({

      state_dat <- filter(dat(), State == settings()[["state"]])
      
      create_subsections_dataset(dat = state_dat, 
                                 subsections = create_subsections(state_dat,
                                                                  use_convention = TRUE))
    
  })
  
  
  mod_download_sub_csv_server("subfragments", 
                              dat = dat_raw,
                              settings = settings)
  
  mod_table_plot_uc_server("uptake_curves", dat = dat, 
                           ret_params = rescale_params,
                           settings = settings,
                           subsections = subsections)
  
  mod_rescale_server("rescale", 
                     dat = dat,
                     settings = settings)
  
  mod_sequence_work_server("sequences",
                           dat = dat,
                           settings = settings)
  
  mod_retention_plot_server("retention",
                            dat = dat_raw,
                            settings = settings)

}
