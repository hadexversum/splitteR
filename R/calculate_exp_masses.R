#' Modified version of function calculate_exp_masses
#' from HaDeX2 package, to proved information on MaxUptake
#' 
#' @export

calculate_exp_masses <- function(dat){
  
  proton_mass <- 1.00727647
  
  exp_dat <- calculate_exp_masses_per_replicate(dat)
  
  exp_dat <- data.table(exp_dat)
  
  exp_dat <- exp_dat[, .(avg_mass = mean(avg_exp_mass),
                         err_avg_mass = sd(avg_exp_mass)/sqrt(.N)),
                     by = c("Protein", "State", "Sequence", "Start", "End", "MaxUptake", "MHP", "Exposure", "Modification")]
  
  setorderv(exp_dat, cols = c("Start", "End"))
  
  return(exp_dat)
  
}
