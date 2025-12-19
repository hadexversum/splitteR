#' back_exchange UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_back_exchange_ui <- function(id) {
  ns <- NS(id)
  tagList(
    wellPanel(
    splitLayout(
      p("Choose parameters:"),
        selectInput(inputId = ns("time_0"),
                    label = "Select no deut timepoint",
                    choices = c(0, 0.1)),
        selectInput(inputId = ns("time_100"),
                    label = "Select FD timepoint",
                    choices = c(1000, 1400)),
        numericInput(inputId = ns("deut_part"),
                     label = "Deuterium concentration:",
                     value = 0.9),
        selectInput(inputId = ns("state"),
                    label = "Select_state",
                    choices = "Alpha")
        ))
    ,
    DTOutput(outputId = ns("res_data")),
    p("What do we see?"),
    p("h_ret = retention for peptide based on Hamuro table - value in Daltons"),
    p("deut_uptake = deuterium uptake for peptide: m(t = FD) - m(t_0) - value in Daltons"),
    p("max_exp_ret = deut_uptake(t = FD)/(deut_part*MaxUptake)"), 
    p("theo_ret = h_ret/MaxUptake"),
    p("ret_ratio = max_exp_ret/theo_ret"),
    plotOutput(outputId = ns("res_scatter"))
 
  )
}
    
#' back_exchange Server Functions
#'
#' @noRd 
mod_back_exchange_server <- function(id, dat){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    observe({
      
      times <- unique(dat[[1]]()[["Exposure"]])
      states <- unique(dat[[1]]()[["State"]])
      
      updateSelectInput(inputId = "time_0",
                        choices = times,
                        selected = min(times))
      
      updateSelectInput(inputId = "time_100",
                        choices = times,
                        selected = max(times))
      
      updateSelectInput(inputId = "state",
                        choices = states, 
                        selected = states[1])
   
    })
    
    res_dat <- reactive({
      
      kin_dat_fd <- HaDeX2::calculate_state_uptake(dat[[1]](),
                                                   state = input[["state"]], 
                                                   time_0 = as.numeric(input[["time_0"]]), 
                                                   time_t = as.numeric(input[["time_100"]]),
                                                   time_100 = as.numeric(input[["time_100"]])) %>%
        select(Protein, Sequence, Start, End, MaxUptake, deut_uptake, Modification)
      
      h_res <- HaDeX2::calculate_back_exchange(dat = dat[[1]](),
                                               states = input[["state"]],
                                               time_100 = as.numeric(input[["time_100"]])) %>%
        mutate(seq_length = nchar(Sequence))
      
      h_res["h_ret"] <- lapply(1:nrow(h_res), function(i){
        splitteR::calculate_hrate(sequence = h_res[i, "Sequence"])
      }) %>% unlist(.)
      
      # res <- merge(kin_dat_fd, h_res, by = c("Protein", "Sequence", "Start", "End", "Modification")) %>%
      #   arrange(Start, End) %>%
      #   mutate(hrates_scaled = hrates*as.numeric(input[["deut_part"]]),
      #          hrates_ratio = hrates_scaled/deut_uptake,
      #          ID = 1:nrow(.)) 
      
      
      res <- merge(kin_dat_fd, h_res, by = c("Protein", "Sequence", "Start", "End", "Modification")) %>%
        arrange(Start, End) %>%
        mutate(ID = 1:nrow(.),
               max_exp_ret = deut_uptake/(MaxUptake*as.numeric(input[["deut_part"]])),
               theo_ret = h_ret/MaxUptake,
               ret_ratio = max_exp_ret/theo_ret)
      
      res
      
    })
    
    ##
    
    
    
    output[["res_data"]] <- renderDT({
      
      res_dat() %>%
        mutate(back_exchange = round(back_exchange, 4),
               err_back_exchange = round(err_back_exchange, 4),
               deut_uptake = round(deut_uptake, 4),
               h_ret = round(h_ret, 4),
               theo_ret = round(theo_ret, 4),
               ret_ratio = round(ret_ratio, 4),
               max_exp_ret = round(max_exp_ret, 4)) %>%
        select(ID, Protein, Sequence, State, Start, End, Modification, seq_length, MaxUptake, h_ret, theo_ret, max_exp_ret, ret_ratio, back_exchange, err_back_exchange)
      
    })
 
    
    output[["res_scatter"]] <- renderPlot({
      
      ggplot(res_dat()) +
        geom_segment(aes(x = Start, xend = End, y = ret_ratio, color = seq_length), size = 2) + 
        geom_hline(yintercept = 1, linewidth = 0.5, color = "red", linetype = "dashed", alpha = 0.3) + 
        scale_colour_gradientn(colours = terrain.colors(10)) +
        theme_bw(base_size = 18) +
        theme(legend.position = "bottom") +
        labs(x = "Peptide ID",
             y = "exp/theo retention ratio",
             color = "Peptide length")
      
    })
    
  })
  
  
}
    
## To be copied in the UI
# mod_back_exchange_ui("back_exchange_1")
    
## To be copied in the server
# mod_back_exchange_server("back_exchange_1")
