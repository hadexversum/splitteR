#' Plot uc of subfragment with parent peptides
#' 
#' @param dat original dat
#' @param subsection record of chosen subsection 
#' 
#' @examples
#' subsections <- create_subsections(alpha_dat)
#' subsection <- subsections[3,]
#' subsection_dat <- get_subsection_data(alpha_dat, subsection = subsection)
#'
#' plot_uc_with_origin(dat = alpha_dat, subsection_dat = subsection_dat)
#'
#' @export

plot_uc_with_origin <- function(dat,
                                subsection_dat,
                                subsection,
                                state = dat[["State"]][1]){
  
  assert(subsection[["common"]]!="origin")
  
  longer_kin_dat <- calculate_peptide_kinetics(dat,
                                               sequence = subsection[, "longer_sequence"],
                                               start = subsection[, "longer_start"],
                                               end = subsection[, "longer_end"],
                                               states = state)
  
  shorter_kin_dat <- calculate_peptide_kinetics(dat,
                                                sequence = subsection[, "shorter_sequence"],
                                                start = subsection[, "shorter_start"],
                                                end = subsection[, "shorter_end"],
                                                states = state)
  
  sub_kin_dat <- calculate_peptide_kinetics(subsection_dat)
  
  low_y <- min(min(sub_kin_dat[["deut_frac"]]), 0)
    
    ggplot() +
      geom_point(dat = longer_kin_dat, aes(x = Exposure, y = deut_uptake, color = Sequence)) +
      geom_line(dat = longer_kin_dat, aes(x = Exposure, y = deut_uptake, color = Sequence)) +
      geom_point(dat = shorter_kin_dat, aes(x = Exposure, y = deut_uptake, color = Sequence)) +
      geom_line(dat = shorter_kin_dat, aes(x = Exposure, y = deut_uptake, color = Sequence)) +
      geom_point(dat = sub_kin_dat, aes(x = Exposure, y = deut_uptake, color = Sequence)) +
      geom_line(dat = sub_kin_dat, aes(x = Exposure, y = deut_uptake, color = Sequence), linetype = 2) +
      scale_x_log10() + 
      ylim(c(low_y-1, NA)) +
      theme(legend.position = "bottom") +
      labs(title = paste0("Deuterium uptake for subsection ", sub_kin_dat[["Sequence"]][1], " and the original peptides"),
           x = "Exposure [log(min)]",
           y = "Deuterium uptake [Da]")
    
  
 

}