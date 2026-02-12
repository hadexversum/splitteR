#' Calculate retention for single peptide
#' 
#' Not sure if necessary
#' 
#' @param time_100
#'
#'
#' @export

calculate_retention <- function(dat, 
                                sequence, 
                                time_100){
  
  
}

#' First version of function, operating 
#' on deuterium uptake
#'
#'
#' @examples
#' 
#' 
#' @export

create_retention_dataset <- function(dat, 
                                     state = unique(dat[["State"]])[1], 
                                     time_0 = min(dat[["Exposure"]]), 
                                     time_100 = max(dat[["Exposure"]]),
                                     deut_part = 0.9){
  
  
  kin_dat_fd <- HaDeX2::calculate_state_uptake(dat,
                                               state = state, 
                                               time_0 = time_0, 
                                               time_t = time_100,
                                               time_100 = time_100) %>%
    select(Protein, Sequence, Start, End, MaxUptake, deut_uptake, Modification)
  
  h_res <- HaDeX2::calculate_back_exchange(dat = dat,
                                           states = state,
                                           time_100 = time_100) %>%
    mutate(seq_length = nchar(Sequence))
  
  h_res["h_ret"] <- lapply(1:nrow(h_res), function(i){
    splitteR::calculate_hrate(sequence = h_res[i, "Sequence"])
  }) %>% unlist(.)
  
  res <- merge(kin_dat_fd, h_res, by = c("Protein", "Sequence", "Start", "End", "Modification")) %>%
    arrange(Start, End) %>%
    mutate(ID = 1:nrow(.),
           max_exp_ret = deut_uptake/(MaxUptake*deut_part),
           theo_ret = h_ret/MaxUptake,
           ret_ratio = max_exp_ret/theo_ret, 
           ret_scale = 1/max_exp_ret)
  
  res
}


#' New version of create_retention_dataset, but
#' operating on masses, not deuterium uptake
#' 
#' @export
create_retention_dataset_2 <- function(dat, 
                                       state = unique(dat[["State"]])[1], 
                                       time_100 = max(dat[["Exposure"]]),
                                       deut_part = 0.9){
  
  mass_dat_fd <- as.data.frame(splitteR::calculate_exp_masses(dat)) %>%
    filter(State == state, Exposure == time_100)
  
  mass_dat_fd["h_ret"] <- lapply(1:nrow(mass_dat_fd), function(i){
    splitteR::calculate_hrate(sequence = mass_dat_fd[i, ][["Sequence"]])
  }) %>% unlist(.)
  
  res <- mass_dat_fd %>%
    arrange(Start, End) %>%
    mutate(ID = 1:nrow(.),
           theo_max_mass = MHP + MaxUptake*deut_part,
           ret_scale = theo_max_mass / avg_mass)
  
  res
}



