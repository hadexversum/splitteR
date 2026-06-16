#' Plot uc of subfragment with parent peptides
#' 
#' @param dat original dat
#' @param subsection record of chosen subsection 
#' 
#' @examples
#' subsections <- create_subsections(alpha_dat)
#' subsection <- subsections[3,]
#' 
#' # subsection_dat <- get_subsection_data(alpha_dat, subsection = subsection)
#' # plot_uc_with_origin(dat = alpha_dat, subsection_dat = subsection_dat, subsection = subsection)
#' 
#' plot_uc_with_origin(dat = alpha_dat, subsection = subsection)
#' 
#' @export

plot_uc_with_origin <- function(dat,
                                subsection,
                                time_0 = min(dat[["Exposure"]]),
                                time_100 = max(dat[["Exposure"]]),
                                state = dat[["State"]][1],
                                deut_part = 0.9){
  
  assert(subsection[["common"]]!="origin")
  
  longer_kin_dat <- calculate_deut_uptake(dat, 
                                          state = state, 
                                          sequence = subsection[["longer_sequence"]],
                                          start = subsection[["longer_start"]],
                                          end = subsection[["longer_end"]],
                                          time_0 = time_0) %>%
    filter(Exposure > time_0)
  
  
  shorter_kin_dat <- calculate_deut_uptake(dat, 
                                           state = state,
                                           sequence = subsection[["shorter_sequence"]],
                                           start = subsection[["shorter_start"]],
                                           end = subsection[["shorter_end"]],
                                           time_0 = time_0) %>%
    filter(Exposure > time_0)
 
  
  
  sub_kin_dat <- calculate_sub_deut_uptake(subsection = subsection, 
                                           kin_dat_longer = longer_kin_dat,
                                           kin_dat_shorter = shorter_kin_dat) %>%
    filter(Exposure > time_0)
    
  
  sub_max_uptake <- unique(sub_kin_dat[["MaxUptake"]])
  common <- subsection[["common"]]
  
  longer_sequence <- unique(longer_kin_dat[["Sequence"]])
  shorter_sequence <- unique(shorter_kin_dat[["Sequence"]])
  sub_sequence <- unique(sub_kin_dat[["Sequence"]])
  
  low_y <- min(min(sub_kin_dat[["deut_uptake"]]), 0)
  
  ggplot() +
    geom_point(dat = longer_kin_dat, aes(x = Exposure, y = deut_uptake, color = longer_sequence)) +
    geom_line(dat = longer_kin_dat, aes(x = Exposure, y = deut_uptake, color = longer_sequence)) +
    geom_ribbon(dat = longer_kin_dat, aes(x = Exposure, 
                                          ymin = deut_uptake - err_deut_uptake,
                                          ymax = deut_uptake + err_deut_uptake,
                                          fill = longer_sequence),  alpha = 0.15) + 
    geom_point(dat = shorter_kin_dat, aes(x = Exposure, y = deut_uptake, color = shorter_sequence)) +
    geom_line(dat = shorter_kin_dat, aes(x = Exposure, y = deut_uptake, color = shorter_sequence)) +
    geom_ribbon(dat = shorter_kin_dat, aes(x = Exposure, 
                                           ymin = deut_uptake - err_deut_uptake,
                                           ymax = deut_uptake + err_deut_uptake,
                                           fill = shorter_sequence),  alpha = 0.15) +   
    geom_point(dat = sub_kin_dat, aes(x = Exposure, y = deut_uptake, color = sub_sequence)) +
    geom_line(dat = sub_kin_dat, aes(x = Exposure, y = deut_uptake, color = sub_sequence), linetype = 2) +
    geom_ribbon(dat = sub_kin_dat, aes(x = Exposure,
                                          ymin = deut_uptake - err_deut_uptake,
                                          ymax = deut_uptake + err_deut_uptake,
                                          fill = sub_sequence),  alpha = 0.15) +
    scale_x_log10() + 
    ylim(c(low_y-1, NA)) +
    geom_hline(yintercept = 0, linetype = "dotted", size = 0.25) +
    geom_hline(yintercept = sub_max_uptake*deut_part, linetype = "dotted", size = 0.25) +
    theme(legend.position = "bottom") +
    labs(title = paste0("Deuterium uptake for subsection ", sub_kin_dat[["Sequence"]][1], " and the original peptides"),
         x = "Exposure [log(min)]",
         y = "Deuterium uptake [Da]",
         color = "") +
    scale_color_manual(
      values = c(
        setNames("#F8766D", longer_sequence),
        setNames("#00BA38", shorter_sequence),
        setNames("#619CFF", sub_sequence)),
      breaks = c(longer_sequence, shorter_sequence, sub_sequence)) +
    scale_fill_manual(
      values = c(
        setNames("#F8766D", longer_sequence),
        setNames("#00BA38", shorter_sequence),
        setNames("#619CFF", sub_sequence))) +
    guides(fill = "none")
  
  
  
  
  
}

# plot_uc_with_origin_old <- function(dat,
#                                 subsection_dat,
#                                 subsection,
#                                 time_0 = min(subsection_dat[["Exposure"]]),
#                                 time_100 = max(subsection_dat[["Exposure"]]),
#                                 state = dat[["State"]][1],
#                                 deut_part = 0.9){
#   
#   assert(subsection[["common"]]!="origin")
#   
#   longer_kin_dat <- calculate_peptide_kinetics(dat,
#                                                time_0 = time_0, 
#                                                time_100 = time_100, 
#                                                sequence = subsection[, "longer_sequence"],
#                                                start = subsection[, "longer_start"],
#                                                end = subsection[, "longer_end"],
#                                                states = state)
#   
# 
#   
#  
#   shorter_kin_dat <- calculate_peptide_kinetics(dat,
#                                                 time_0 = time_0, 
#                                                 time_100 = time_100, 
#                                                 sequence = subsection[, "shorter_sequence"],
#                                                 start = subsection[, "shorter_start"],
#                                                 end = subsection[, "shorter_end"],
#                                                 states = state)
#   
#   sub_kin_dat <- calculate_peptide_kinetics(subsection_dat,
#                                             time_0 = time_0, 
#                                             time_100 = time_100)
#   
#   sub_max_uptake <- stringr::str_count(unique(sub_kin_dat[["Sequence"]]), "[A-Z]")
#   
#   common <- subsection[["common"]]
#   
#   longer_sequence <- unique(longer_kin_dat[["Sequence"]])
#   shorter_sequence <- unique(shorter_kin_dat[["Sequence"]])
#   sub_sequence <- unique(sub_kin_dat[["Sequence"]])
#                                        
#   low_y <- min(min(sub_kin_dat[["deut_frac"]]), 0)
#     
#     ggplot() +
#       geom_point(dat = longer_kin_dat, aes(x = Exposure, y = deut_uptake, color = longer_sequence)) +
#       geom_line(dat = longer_kin_dat, aes(x = Exposure, y = deut_uptake, color = longer_sequence)) +
#       geom_ribbon(dat = longer_kin_dat, aes(x = Exposure, 
#                                             ymin = deut_uptake - err_deut_uptake,
#                                             ymax = deut_uptake + err_deut_uptake,
#                                             fill = longer_sequence),  alpha = 0.15) + 
#       geom_point(dat = shorter_kin_dat, aes(x = Exposure, y = deut_uptake, color = shorter_sequence)) +
#       geom_line(dat = shorter_kin_dat, aes(x = Exposure, y = deut_uptake, color = shorter_sequence)) +
#       geom_ribbon(dat = shorter_kin_dat, aes(x = Exposure, 
#                                             ymin = deut_uptake - err_deut_uptake,
#                                             ymax = deut_uptake + err_deut_uptake,
#                                             fill = shorter_sequence),  alpha = 0.15) +   
#       geom_point(dat = sub_kin_dat, aes(x = Exposure, y = deut_uptake, color = sub_sequence)) +
#       geom_line(dat = sub_kin_dat, aes(x = Exposure, y = deut_uptake, color = sub_sequence), linetype = 2) +
#       # geom_ribbon(dat = sub_kin_dat, aes(x = Exposure, 
#       #                                       ymin = deut_uptake - err_deut_uptake,
#       #                                       ymax = deut_uptake + err_deut_uptake,
#       #                                       fill = sub_sequence),  alpha = 0.15) + 
#       scale_x_log10() + 
#       ylim(c(low_y-1, NA)) +
#       geom_hline(yintercept = 0, linetype = "dotted", size = 0.25) +
#       geom_hline(yintercept = sub_max_uptake*deut_part, linetype = "dotted", size = 0.25) +
#       theme(legend.position = "bottom") +
#       labs(title = paste0("Deuterium uptake for subsection ", sub_kin_dat[["Sequence"]][1], " and the original peptides"),
#            x = "Exposure [log(min)]",
#            y = "Deuterium uptake [Da]",
#            color = "") +
#       scale_color_manual(
#         values = c(
#           setNames("#F8766D", longer_sequence),
#           setNames("#00BA38", shorter_sequence),
#           setNames("#619CFF", sub_sequence)),
#         breaks = c(longer_sequence, shorter_sequence, sub_sequence)) +
#       scale_fill_manual(
#         values = c(
#           setNames("#F8766D", longer_sequence),
#           setNames("#00BA38", shorter_sequence),
#           setNames("#619CFF", sub_sequence))) +
#       guides(fill = "none")
#    
# 
#       
#  
# 
# }
