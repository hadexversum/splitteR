#' table_plot_uc UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_table_plot_uc_ui <- function(id) {
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns("subsections_list")),
    ggiraph::girafeOutput(ns("subsections_uc"), width = "80%")
    
  )
}
    
#' table_plot_uc Server Functions
#'
#' @noRd 
mod_table_plot_uc_server <- function(id, dat, subsections, settings, ret_params){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
    output[["subsections_list"]] <- DT::renderDataTable({
      
      DT::datatable(data = subsections(),
                    selection = "single")
    })
    
    subsection_dat <- reactive({
      
          get_subsection_data(dat = if(settings()[["if_rescaled"]]) rescaled_dat() else dat(), 
                          subsection = subsections()[input[["subsections_list_rows_selected"]], ])
      
    })
    
    rescaled_dat <- reactive({
      
      dat() %>%
        filter(State == settings()[["state"]]) %>%
        create_rescaled_uptake_dataset(.,
                                       ret_params = ret_params(),
                                       time_0 = settings()[["time_0"]],
                                       time_100 = settings()[["time_100"]],
                                       deut_part = settings()[["deut_part"]],
                                       for_download = FALSE)
      
    })
    
    
    output[["subsections_uc"]] <- ggiraph::renderGirafe({
      
      validate(need(!is.null(input[["subsections_list_rows_selected"]]), ""))
      i = input[["subsections_list_rows_selected"]]
      
      # browser()
      
      
      if(subsections()[input[["subsections_list_rows_selected"]], "common"] == "origin"){
        
        ## origin peptide
        
        pep_dat <- dat() %>%
          filter(Sequence == subsections()[input[["subsections_list_rows_selected"]], "sub_sequence"],
                 Start == subsections()[input[["subsections_list_rows_selected"]], "sub_start"],
                 End == subsections()[input[["subsections_list_rows_selected"]], "sub_end"],
                 State == settings()[["state"]]) 
        
        if(settings()[["if_rescaled"]]){
          
          ret_scale <- ret_params() %>%
            filter(Sequence == subsections()[input[["subsections_list_rows_selected"]], "sub_sequence"],
                   Start == subsections()[input[["subsections_list_rows_selected"]], "sub_start"],
                   End == subsections()[input[["subsections_list_rows_selected"]], "sub_end"]) %>%
            .[["ret_scale"]]
          
          pep_dat <- calculate_rescaled_uptake(pep_dat, 
                                               ret_scale = ret_scale,
                                               time_0 = settings()[["time_0"]], 
                                               time_100 = settings()[["time_100"]], 
                                               deut_part = settings()[["deut_part"]])
        }
        
        plt <- calculate_peptide_kinetics(pep_dat,
                                          states = settings()[["state"]]) %>%
          plot_uptake_curve(.) +
          ggplot2::ylim(c(0, NA))
        
      } else {
        
        ## subsected peptide
        
        plt <- if(settings()[["if_rescaled"]]){
          
          plot_uc_with_origin(dat = rescaled_dat(), 
                              subsection = subsections()[input[["subsections_list_rows_selected"]], ],
                              subsection_dat = subsection_dat())
     
        } else {
          
            plot_uc_with_origin(dat = dat(), 
                                subsection = subsections()[input[["subsections_list_rows_selected"]], ],
                                subsection_dat = subsection_dat())
        }
        
        
      }
      
      
      girafe(ggobj = plt,
            width_svg = 9, height_svg = 5, opts_sizing(rescale = TRUE))
      
    })
    
  })
}
    
## To be copied in the UI
# mod_table_plot_uc_ui("table_plot_uc_1")
    
## To be copied in the server
# mod_table_plot_uc_server("table_plot_uc_1")
