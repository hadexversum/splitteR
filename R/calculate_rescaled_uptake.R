
#' @export 
calculate_rescaled_uptake <- function(pep_dat, 
                                      ret_scale,
                                      time_0 = min(pep_dat[["Exposure"]]),
                                      time_100 = max(pep_dat[["Exposure"]])){
  
  fin <- HaDeX2::calculate_exp_masses(pep_dat)
  
  HaDeX2::calculate_kinetics(pep_dat, 
                             time_0 = time_0, 
                             time_100 = time_100) %>%
    mutate(sc_deut_uptake = deut_uptake * ret_scale)
  
  
}
# 
# x_dat
# 
# 
# state = "ALPHA_Gamma"
# sequence = "DDKVGTDM"
# start = 194 
# end = 201 
# 
# pep_dat <- alpha_dat %>%
#   filter(Sequence == sequence, 
#          State == state, 
#          Start == start, 
#          End == end)
# ret_scale <- filter(x_dat, Sequence == sequence, 
#                     State == state, 
#                     Start == start, 
#                     End == end) %>% select(ret_scale) %>% .[[1]]

# thats good
# x <- create_retention_dataset_2(alpha_dat)
# 
# ret_scale <- filter(x, Sequence == sequence, 
#                                          State == state,  
#                                          Start == start, 
#                                          End == end) %>% select(ret_scale) %>% .[[1]]
# 
# state = "ALPHA_Gamma"
# sequence = "DDKVGTDM"
# start = 194
# end = 201
# 
# HaDeX2::calculate_exp_masses_per_replicate(pep_dat) %>%
#   mutate(sc_mass = avg_exp_mass * ret_scale ) %>%
#   group_by(Exposure) %>%
#   mutate(mass = mean(sc_mass)) %>%
#   mutate(uptake = mass - MHP) %>%
#   ggplot() +
#   geom_point(aes(x = Exposure, y = uptake)) + 
#   scale_x_log10() +
#   geom_hline(aes(yintercept = MaxUptake), color = "red") +
#   geom_hline(aes(yintercept = MaxUptake*deut_part), linetype = "dashed", color = "red") +
#   ylim(c(0, NA))
# 
# library(ggplot2)
