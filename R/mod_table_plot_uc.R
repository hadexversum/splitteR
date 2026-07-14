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
    
      nicer_table(tbl_dat = subsections(), filename = "subsections_data", selection = "single")
    }, server = FALSE)
    
    subsection_dat <- reactive({
      
          get_subsection_data(dat = if(settings()[["if_rescaled"]]) rescaled_dat() else dat(), 
                          subsection = subsections()[input[["subsections_list_rows_selected"]], ])
      
    })
    
    rescaled_dat <- reactive({

      dat() %>%
        filter(State == settings()[["state"]]) %>%
        create_rescaled_uptake_dataset(.,
                                       ret_params = ret_params(),
                                       scaling_value = settings()[["rescaling_value"]], 
                                       time_0 = settings()[["time_0"]],
                                       time_100 = settings()[["time_100"]],
                                       deut_part = settings()[["deut_part"]])
      
    })
    
    
    output[["subsections_uc"]] <- ggiraph::renderGirafe({
      
      validate(need(!is.null(input[["subsections_list_rows_selected"]]), ""))
      i = input[["subsections_list_rows_selected"]]
      
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
            .[[settings()[["rescaling_value"]]]]
          
         
          
          pep_dat <- calculate_rescaled_uptake(pep_dat, 
                                               ret_scale = ret_scale,
                                               time_0 = settings()[["time_0"]], 
                                               time_100 = settings()[["time_100"]], 
                                               deut_part = settings()[["deut_part"]]) 
        }
        
        
        plt <- calculate_deut_uptake(pep_dat,
                                     state = settings()[["state"]]) %>%
          filter(Exposure > settings()[["time_0"]]) %>%
          ggplot2::ggplot(aes(x = Exposure, y = deut_uptake)) +
            ggiraph::geom_point_interactive(mapping = aes(x = Exposure, y = deut_uptake, 
                                                 tooltip = glue("Sequence: {Sequence}
                                                                Exposure: {Exposure} [min]
                                                                DU: {round(deut_uptake, 2)} [Da]
                                                                Err(DU): {round(err_deut_uptake, 2)} [Da]"))) +
            geom_line() +
            geom_ribbon(aes(x = Exposure, 
                          ymin = deut_uptake - err_deut_uptake, 
                          ymax = deut_uptake + err_deut_uptake),  alpha = 0.15) +
          scale_x_log10() + 
          theme(legend.position = "bottom") +
          labs(title = paste0("Deuterium uptake for ", subsections()[input[["subsections_list_rows_selected"]], "sub_sequence"]),
               x = "Exposure [log(min)]",
               y = "Deuterium uptake [Da]")
            
      } else {
        
        ## subsected peptide
        
        plt <- if(settings()[["if_rescaled"]]){
          
          plot_uc_with_origin(dat = rescaled_dat(), 
                              time_0 = settings()[["time_0"]], 
                              time_100 = settings()[["time_100"]], 
                              subsection = subsections()[input[["subsections_list_rows_selected"]], ] ,
                              # subsection_dat = subsection_dat()
                              interactive = TRUE
                              )
     
        } else {
          
            plot_uc_with_origin(dat = dat(), 
                                time_0 = settings()[["time_0"]], 
                                time_100 = settings()[["time_100"]], 
                                subsection = subsections()[input[["subsections_list_rows_selected"]], ] ,
                                # subsection_dat = subsection_dat()
                                interactive = TRUE
                                )
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
