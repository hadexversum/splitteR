#' Calculate deut_uptake for all time points
#' but only for one state
#' 
#' @description
#' Variation of HaDeX2 function
#' 
#' 
#' @export

calculate_deut_uptake <- function(dat,
                                  protein = dat[["Protein"]][1],
                                  sequence = dat[["Sequence"]][1],
                                  state = dat[["State"]][1], 
                                  start = dat[["Start"]][1], 
                                  end = dat[["End"]][1], 
                                  time_0 = min(dat[["Exposure"]]),
                                  deut_part = 0.9){
  proton_mass <- 1.00727647
  
  pep_dat <- filter(dat, 
                    Protein == protein, 
                    Sequence == sequence, 
                    State == state, 
                    Start == start,
                    End == end) %>%
    mutate(exp_mass = Center * z - z * proton_mass) %>%
      select(-z, -Center) %>%
    group_by(Sequence, Start, End, MHP, MaxUptake, State, Exposure, Protein, File, Modification) %>%
    mutate(avg_exp_mass = weighted.mean(exp_mass, Inten, na.rm = TRUE)) %>%
    ungroup() %>%
    select(-Inten) %>%
    group_by(Sequence, Start, End, MHP, MaxUptake, State, Exposure, Protein, Modification) %>%
    summarize(mass = mean(avg_exp_mass, na.rm = TRUE),
              err_mass = coalesce(sd(avg_exp_mass, na.rm = TRUE)/sqrt(n()), 0)) %>%
    ungroup()
    
  pep_0 <- filter(pep_dat, Exposure == time_0) 
            
  mass_0 <- pep_0[["mass"]]
  err_mass_0 <- pep_0[["err_mass"]]

  kin_dat <- pep_dat %>%
    mutate(deut_uptake = mass - mass_0, 
           err_deut_uptake = sqrt(err_mass^2 + err_mass_0^2)) %>%
    select(-mass, -err_mass) %>%
    filter(Exposure > time_0)
  
  return(kin_dat)
  
}


#' Calculates the subfragment data with 
#' propagated error
#' 
#' @examples
#' subsections <- create_subsections(alpha_dat)
#' subsection <- subsectons[1, ]
#' kin_dat_longer <- calculate_deut_uptake(dat, 
#'                                         sequence = subsection[["longer_sequence"]],
#'                                         start = subsection[["longer_start"]],
#'                                         end = subsection[["longer_end"]],
#'                                         time_0 = time_0)
#' 
#' kin_dat_shorter <- calculate_deut_uptake(dat, 
#'                                          sequence = subsection[["shorter_sequence"]],
#'                                          start = subsection[["shorter_start"]],
#'                                          end = subsection[["shorter_end"]],
#'                                          time_0 = time_0) 
#' 
#' @export
calculate_sub_deut_uptake <- function(subsection,
                                      kin_dat_longer, 
                                      kin_dat_shorter){

  merge(kin_dat_longer, kin_dat_shorter, by = c("State", "Exposure", "Protein", "Modification")) %>%
    mutate(deut_uptake = deut_uptake.x - deut_uptake.y,
           err_deut_uptake = sqrt(err_deut_uptake.x^2 + err_deut_uptake.y^2)) %>%
    mutate(Sequence = subsection[["sub_sequence"]],
           Start = subsection[["sub_start"]],
           End = subsection[["sub_end"]], 
           MHP = 0,
           MaxUptake = stringr::str_count(Sequence, "[A-Z]")) %>%
    select(Protein, State, Sequence, Start, End, Modification, MaxUptake, Exposure, deut_uptake, err_deut_uptake) %>%
    arrange(Exposure)
    
 
  
}
