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
mod_table_plot_uc_server <- function(id, dat, subsections){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
    output[["subsections_list"]] <- DT::renderDataTable({
      
      DT::datatable(data = subsections(),
                    selection = "single")
    })
    
    subsection_dat <- reactive({
      
      get_subsection_data(dat[[1]](), subsection = subsections()[input[["subsections_list_rows_selected"]], ])
      
    })
    
    output[["subsections_uc"]] <- ggiraph::renderGirafe({
      
      validate(need(!is.null(input[["subsections_list_rows_selected"]]), ""))
      i = input[["subsections_list_rows_selected"]]
      
      # browser()
      
      if(subsections()[input[["subsections_list_rows_selected"]], "common"] == "origin"){
        
        plt <- dat[[1]]() %>%
          filter(Sequence == subsections()[input[["subsections_list_rows_selected"]], "sub_sequence"],
                 Start == subsections()[input[["subsections_list_rows_selected"]], "sub_start"],
                 End == subsections()[input[["subsections_list_rows_selected"]], "sub_end"]) %>%
        calculate_peptide_kinetics(.,
                                   states = dat[[1]]()[["State"]][1]) %>%
        plot_uptake_curve(.)
        
      } else {
        
        plt <- plot_uc_with_origin(dat = dat[[1]](), 
                                   subsection = subsections()[input[["subsections_list_rows_selected"]], ],
                                   subsection_dat = subsection_dat())
        
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
