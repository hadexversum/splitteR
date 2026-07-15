#' back_exchange UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_rescale_ui <- function(id) {
  ns <- NS(id)
  tagList(
    DT::DTOutput(outputId = ns("res_data")),
    p("What do we see?"),
    p("h_ret = retention for peptide based on Hamuro table - value in Daltons"),
    p("deut_uptake = deuterium uptake for peptide: m(t = FD) - m(t_0) - value in Daltons"),
    # p("max_exp_ret = deut_uptake(t = FD)/(deut_part*MaxUptake)"), 
    p("theo_ret = MaxUptake/h_ret"),
    # p("ret_ratio = max_exp_ret/theo_ret"),
    # p("avg_rt = mean retention time for t = FD"),
    p("Normalisation to Nmax: ret_scale = MaxUptake*deut_part/deut_uptake(t=FD)"), #  = 1/max_exp_ret
    p("Normalisation to standard conditions: ret_scale_2 = h_ret/deut_uptake(t = FD)"),
    ggiraph::girafeOutput(outputId = ns("uc_scaled"), width = "80%"),
    uiOutput(outputId = ns("uc_info")),
    ggiraph::girafeOutput(outputId = ns("rescale_values"), width = "80%"),
    # p("Scaled uptake curve is the standard uptake curve times ret_scale parameter."),
    ggiraph::girafeOutput(outputId = ns("res_scatter"), width = "80%"),
    ggiraph::girafeOutput(outputId = ns("res_scatter_2"), width = "80%"),
    ggiraph::girafeOutput(outputId = ns("plot_FD"), width = "80%"),
    ggiraph::girafeOutput(outputId = ns("plot_h_ret"), width = "80%"),
    ggiraph::girafeOutput(outputId = ns("plot_FD_h_ret"), width = "80%"),
    # plotOutput(outputId = ns("rt_vs_ratio")),
    ggiraph::girafeOutput(outputId = ns("standard_bex"), width = "80%")
    
  )
}

#' back_exchange Server Functions
#'
#' @noRd 
mod_rescale_server <- function(id, dat, settings, dat_rt){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
  
    
    res_dat <- reactive({

      create_retention_dataset(dat = dat(),
                               state = settings()[["state"]], 
                               time_0 = settings()[["time_0"]], 
                               time_100 = settings()[["time_100"]],
                               deut_part = as.numeric(settings()[["deut_part"]]))
      
    })
    
    ##
    
    
    
    
    output[["res_data"]] <- DT::renderDT({
      
      # res_dat_rt() %>%
      res_dat() %>%
        mutate(back_exchange = round(back_exchange, 4),
               err_back_exchange = round(err_back_exchange, 4),
               deut_uptake = round(deut_uptake, 4),
               h_ret = round(h_ret, 4),
               # avg_rt = round(avg_rt, 4),
               theo_ret = round(theo_ret, 4),
               ret_ratio = round(ret_ratio, 4),
               max_exp_ret = round(max_exp_ret, 4),
               ret_scale = round(ret_scale, 4),
               ret_scale_2 = round(ret_scale_2, 4),
               FD = deut_uptake) %>%
      select(Protein, Sequence, State, Start, End, MaxUptake, FD, h_ret, ret_scale, ret_scale_2, theo_ret, back_exchange) %>%
        nicer_table(tbl_dat = ., filename = "rescaling_data", selection = "single")

        # select(ID, Protein, Sequence, State, Start, End, Modification, seq_length, MaxUptake, deut_uptake, h_ret, theo_ret, max_exp_ret, ret_scale, ret_ratio, back_exchange, err_back_exchange) #, avg_rt)
      
    },
    selection = "single",
    server = FALSE)
 
    ##
    
    output[["rescale_values"]] <- ggiraph::renderGirafe({
      
      ## ADD TOOLTIPS
      
      plt <- ggplot(res_dat(), aes(x = ID)) + 
        geom_point(aes(y = ret_scale, color = "ret_scale")) +
        ggiraph::geom_point_interactive(aes(y = ret_scale, 
                                            color = "ret_scale",
                                           tooltip = glue("Sequence: {Sequence}
                                                          Position: {Start}-{End}
                                                          ret_scale: {round(ret_scale, 4)}"))) + 
        geom_line(aes(y = ret_scale, color = "ret_scale")) + 
        geom_point(aes(y = ret_scale_2, color = "ret_scale_2")) +
        ggiraph::geom_point_interactive(aes(y = ret_scale_2, 
                                            color = "ret_scale_2",
                                            tooltip = glue("Sequence: {Sequence}
                                                          Position: {Start}-{End}
                                                          ret_scale_2: {round(ret_scale_2, 4)}"))) + 
        geom_line(aes(y = ret_scale_2, color = "ret_scale_2")) +
        geom_point(aes(y = theo_ret, color = "theo_ret")) +
        ggiraph::geom_point_interactive(aes(y = theo_ret, 
                                            color = "theo_ret",
                                            tooltip = glue("Sequence: {Sequence}
                                                          Position: {Start}-{End}
                                                          theo_ret: {round(theo_ret, 4)}"))) + 
        geom_line(aes(y = theo_ret, color = "theo_ret")) +
        labs(x = "Peptide ID", 
             y = "", 
             title = "Comparison of possible rescaling values",
             color = "") +
        # theme_bw(base_size = 18) +
        theme(legend.position = "bottom") 
      
      ggiraph::girafe(ggobj = plt, width_svg = 7, height_svg = 4, opts_sizing(rescale = TRUE))
      
    })
    
    rt_dat <- reactive({

      dat_rt() %>%
        mutate(Exposure = round(as.numeric(Exposure, 4))) %>%
        filter(Exposure == input[["time_100"]],
               State == input[["state"]]) %>%
        group_by(Protein, Start, End, Sequence, MaxUptake, State) %>%
        summarise(avg_rt = mean(RT)) %>%
        ungroup()


     })

    ##

    res_dat_rt <- reactive({

      merge(res_dat(), rt_dat(), by = c("Protein", "Start", "End", "Sequence", "MaxUptake", "State"), all.x = TRUE)

      })

    ##
    
    output[["res_scatter"]] <- ggiraph::renderGirafe({

      plt <- ggplot(res_dat()) +
        geom_segment_interactive(aes(x = Start, xend = End, 
                                     y = ret_scale, yend = ret_scale,
                                     tooltip = glue("Sequence: {Sequence}
                                                    Position: {Start}-{End}
                                                    ret_scale: {round(ret_scale, 2)}"),
                                     color = seq_length), size = 2) + 
        geom_hline(yintercept = 1, linewidth = 0.5, color = "red", linetype = "dashed", alpha = 0.3) + 
        scale_colour_gradientn(colours = terrain.colors(10)) +
        theme(legend.position = "bottom") +
        labs(x = "Protein sequence",
             y = "Normalisation to Nmax",
             color = "Peptide length")
      
      ggiraph::girafe(ggobj = plt, width_svg = 9, height_svg = 5, opts_sizing(rescale = TRUE))
      
    })
    
    output[["res_scatter_2"]] <- ggiraph::renderGirafe({
      
      plt <- ggplot(res_dat()) +
        geom_segment_interactive(aes(x = Start, xend = End, 
                                     y = ret_scale_2, yend = ret_scale_2, 
                                     tooltip = glue("Sequence: {Sequence}
                                                    Position: {Start}-{End}
                                                    ret_scale_2: {round(ret_scale_2, 2)}"),
                                     color = seq_length), size = 2) + 
        geom_hline(yintercept = 1, linewidth = 0.5, color = "red", linetype = "dashed", alpha = 0.3) + 
        scale_colour_gradientn(colours = terrain.colors(10)) +
        theme(legend.position = "bottom") +
        labs(x = "Protein sequence",
             y = "Normalisation to standard conditions",
             color = "Peptide length")
      
      ggiraph::girafe(ggobj = plt, width_svg = 9, height_svg = 5, opts_sizing(rescale = TRUE))
      
    })
  
    ##
    
  output[["plot_FD"]] <- ggiraph::renderGirafe({
    
    plt <- ggplot(res_dat()) +
      geom_segment_interactive(aes(x = Start, xend = End, 
                       y = deut_uptake, yend = deut_uptake,
                       tooltip = glue("Sequence: {Sequence}
                                      Position: {Start}-{End}
                                      DU: {round(deut_uptake, 2)} [Da]")
                       ), size = 2) + 
      # scale_colour_gradientn(colours = rainbow(10)) +
      theme(legend.position = "bottom") +
      labs(x = "Protein sequence",
           y = "FD [Da]",
           color = "Peptide length")
    
    ggiraph::girafe(ggobj = plt, width_svg = 9, height_svg = 5, opts_sizing(rescale = TRUE))
  })
  
  ##
  
  output[["plot_h_ret"]] <- ggiraph::renderGirafe({

    plt <- ggplot(res_dat()) +
      geom_segment_interactive(aes(x = Start, xend = End, 
                                   y = h_ret, yend = h_ret,
                                   tooltip = glue(
                                     "Sequence: {Sequence}
                                      Position: {Start}-{End}
                                      h_ret: {round(h_ret, 2)}"
                                   )), size = 2) + 
      theme(legend.position = "bottom") +
      labs(x = "Protein sequence",
           y = "Hamuro retention [Da]",
           color = "Peptide length")
    
    ggiraph::girafe(ggobj = plt, width_svg = 9, height_svg = 5, opts_sizing(rescale = TRUE))
    
  })
  
  ##
  
  output[["plot_FD_h_ret"]] <- ggiraph::renderGirafe({
    
    plt <- ggplot(res_dat()) +
      geom_segment_interactive(aes(x = Start, xend = End, 
                                   y = h_ret, yend = h_ret, 
                                   tooltip = glue(
                                     "Sequence: {Sequence}
                                      Position: {Start}-{End}
                                      h_ret: {round(h_ret, 2)}"
                                   ),
                                   color = "h_ret"),
                               size = 2) + 
      geom_segment_interactive(aes(x = Start, xend = End, 
                                   y = deut_uptake, yend = deut_uptake,
                                   tooltip = glue(
                                     "Sequence: {Sequence}
                                      Position: {Start}-{End}
                                      DU: {round(deut_uptake, 2)} [Da]"
                                   ),
                                   color = "FD"),
                               size = 2) + 
      theme(legend.position = "bottom") +
      labs(x = "Protein sequence",
           y = "[Da]",
           color = "Peptide length")
    
    ggiraph::girafe(ggobj = plt, width_svg = 9, height_svg = 5, opts_sizing(rescale = TRUE))
    
  })
  
  ##
    
    # output[["rt_vs_ratio"]] <- renderPlot({
    #   
    #   res_dat_rt() %>%
    #     ggplot() +
    #     geom_point(aes(x = avg_rt, y = ret_scale)) +
    #     geom_hline(yintercept = 1, linewidth = 0.5, color = "red", linetype = "dashed", alpha = 0.3) + 
    #     theme_bw(base_size = 18) + 
    #     labs(x = "Mean RT for FD", 
    #          y = "ret_scale")
    # })
    
    ##
  
  kin_dat_uc <- reactive({
    
    ## outside of uc_scaled item to ensure that the validate is shown
    
    validate(need(!is.null(input[["res_data_rows_selected"]]), "Select peptide from the table to see the plot."))
    
    ## TODO: change to splitteR function
    HaDeX2::calculate_kinetics(dat = dat(),
                               state = settings()[["state"]], 
                               sequence = res_dat()[input[["res_data_rows_selected"]], "Sequence"],
                               start = res_dat()[input[["res_data_rows_selected"]], "Start"],
                               end = res_dat()[input[["res_data_rows_selected"]], "End"],
                               time_0 = settings()[["time_0"]], 
                               time_100 = settings()[["time_100"]])
    
  })
  
  output[["uc_scaled"]] <- ggiraph::renderGirafe({
    
    ret_scale <- res_dat()[input[["res_data_rows_selected"]], settings()[["rescaling_value"]] ]
    
    plt <- splitteR::plot_uc_scaled(kin_dat = kin_dat_uc(), 
                   state = settings()[["state"]],
                   ret = ret_scale, 
                   interactive = TRUE) +
      # ggplot2::theme_bw(base_size = 18) +
      ggplot2::theme(legend.position = "bottom") 
    
    ggiraph::girafe(ggobj = plt, width_svg = 9, height_svg = 5, opts_sizing(rescale = TRUE))
    
  })
  
  
  output[["uc_info"]] <- renderUI({
    
    paste0("The UC above is scalled using ", settings()[["rescaling_value"]], " value.")
    
  })
  ##
  
  
  
  output[["standard_bex"]] <- ggiraph::renderGirafe({
    
    ## ADD TOOLTIPS
    
    bex_dat <- calculate_back_exchange(dat = dat(), 
                                       states = settings()[["state"]],
                                       time_100 = settings()[["time_100"]])
    
    plt <- splitteR::plot_comparison_backexchange(bex_dat)
    
    ggiraph::girafe(ggobj = plt, width_svg = 9, height_svg = 5, opts_sizing(rescale = TRUE))
    
    # HaDeX2::plot_coverage_heatmap(bex_dat, 
    #                               value = "back_exchange")
    
  })
  
  })
}
    
