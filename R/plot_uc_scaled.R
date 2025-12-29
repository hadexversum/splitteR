#' Plots UC scaled with retention
#' 
#' @param kin_dat uptake dat for a peptide
#' @param ret scaling parameter
#' 
#' This function plots two deuterium uptake curve:
#' experimental and scaled by the retention parameter
#' @export

plot_uc_scaled <- function(kin_dat, 
                           state = unique(kin_dat[["State"]])[1],
                           ret = 1){
  
  kin_dat_state <- filter(kin_dat, State == state)
  
  sequence <- unique(kin_dat_state[["Sequence"]])
  
  plot_dat <- mutate(kin_dat_state,
                     deut_uptake_scaled = deut_uptake*ret)
  
  plt <- ggplot(plot_dat) +
    geom_point(aes(x = Exposure, y = deut_uptake, color = "original")) +
    geom_point(aes(x = Exposure, y = deut_uptake_scaled, color = "scaled")) +
    geom_line(aes(x = Exposure, y = deut_uptake, color = "original")) +
    geom_ribbon(aes(x = Exposure, ymin = deut_uptake - err_deut_uptake, ymax = deut_uptake + err_deut_uptake, fill = "original"), alpha = 0.15) +
    geom_line(aes(x = Exposure, y = deut_uptake_scaled, color = "scaled")) +
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