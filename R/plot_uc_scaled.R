#' Plots UC scaled with retention
#' 
#' @param kin_dat uptake dat for a peptide
#' @param ret scaling parameter
#' @param interactive indicator of interactivity
#' 
#' This function plots two deuterium uptake curve:
#' experimental and scaled by the retention parameter.
#' 
#' @export

plot_uc_scaled <- function(kin_dat, 
                           state = unique(kin_dat[["State"]])[1],
                           ret = 1,
                           interactive = FALSE){
  
  kin_dat_state <- filter(kin_dat, State == state)
  
  sequence <- unique(kin_dat_state[["Sequence"]])
  
  plot_dat <- mutate(kin_dat_state,
                     deut_uptake_scaled = deut_uptake*ret, 
                     err_deut_uptake_scaled = err_deut_uptake)
  
  geom_point_original <- if(interactive){
    geom_point_interactive(data = plot_dat, 
                           mapping = aes(x = Exposure, y = deut_uptake, 
                                         color = "original",
                                         tooltip = glue("Sequence: {sequence}
                                                        Exposure: {Exposure} [min]
                                                        DU: {round(deut_uptake, 2)} [Da]
                                                        Err(DU): {round(err_deut_uptake, 2)} [Da]"
                                                        ))) 
  } else {
    geom_point(data = plot_dat, 
               aes(x = Exposure, y = deut_uptake, color = "original")) 
  }
  
  geom_point_scaled <- if(interactive){
    
    geom_point_interactive(data = plot_dat, 
                           mapping = aes(x = Exposure, y = deut_uptake_scaled, 
                                         color = "scaled",
                                         tooltip = glue("Sequence: {sequence}
                                                        Exposure: {Exposure} [min]
                                                        DU: {round(deut_uptake, 2)} [Da]
                                                        Err(DU): {round(err_deut_uptake_scaled, 2)} [Da]")))
    
  } else {
    geom_point(aes(x = Exposure, y = deut_uptake_scaled, color = "scaled"))
  }
  
  plt <- ggplot(plot_dat) +
    geom_point_original +
    geom_line(aes(x = Exposure, y = deut_uptake, color = "original")) +
    geom_ribbon(aes(x = Exposure, ymin = deut_uptake - err_deut_uptake, ymax = deut_uptake + err_deut_uptake, fill = "original"), alpha = 0.15) +
    geom_point_scaled +
    geom_line(aes(x = Exposure, y = deut_uptake_scaled, color = "scaled")) +
    geom_ribbon(aes(x = Exposure, ymin = deut_uptake_scaled - err_deut_uptake_scaled, ymax = deut_uptake_scaled + err_deut_uptake_scaled, fill = "scaled"), alpha = 0.15) +
    scale_x_log10() + 
    ylim(c(0, NA)) + 
    theme_bw() +
    labs(x = "Exposure [min]",
         y = "Deuterium uptake [Da]",
         title = paste0("Scaled UC for ", sequence, " in ", state, " state"),
         color = "") +
    guides(fill = "none") + 
    theme(legend.position = "bottom")
  
  return(plt)
} 