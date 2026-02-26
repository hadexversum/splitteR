
#' @export 
calculate_rescaled_uptake <- function(pep_dat, 
                                      ret_scale,
                                      time_0 = min(pep_dat[["Exposure"]]),
                                      time_100 = max(pep_dat[["Exposure"]]),
                                      deut_part = 0.9){
  
  pep_dat <- filter(dat, 
                    Sequence == peptide_list[[i, "Sequence"]],
                    Start == as.numeric(peptide_list[[i, "Start"]]),
                    End == as.numeric(peptide_list[[i, "End"]])) 
  MaxUptake <- unique(pep_dat[["MaxUptake"]])
  MHP <- unique(pep_dat[["MHP"]])
  Protein <- unique(pep_dat[["Protein"]])
  State <- unique(pep_dat[["State"]])
  
  tmp_dat <- HaDeX2::calculate_peptide_kinetics(pep_dat,
                                                time_0 = time_0, 
                                                time_100 = time_100) %>%
    mutate(sc_uptake = deut_uptake * ret_scale_uptake ) %>%
    select(Protein, Sequence, Start, End, State, Exposure, Modification, sc_uptake) 
  
  tmp_dat <- rbind(tmp_dat, c(Protein, peptide_list[[i, "Sequence"]], peptide_list[[i, "Start"]],
                              peptide_list[[i, "End"]], State, time_0, NA, 0))
  
  tmp_dat <- rbind(tmp_dat, c(Protein, peptide_list[[i, "Sequence"]], peptide_list[[i, "Start"]],
                              peptide_list[[i, "End"]], State, time_100, NA, deut_part*MaxUptake))
  
  tmp_dat <- tmp_dat %>% mutate(Fragment = "",
                                Center = sc_uptake, 
                                MaxUptake = MaxUptake, 
                                MHP = MHP,
                                File = paste0("file_", Exposure),
                                Inten = 1, 
                                z = 1,
                                RT = 0) 
  
  tmp_dat
  
  
}



#' @examples
#' ret_params <- create_retention_dataset(alpha_dat)
#' create_rescaled_uptake_dataset(alpha_dat, ret_params)
#' 
#' @export

create_rescaled_uptake_dataset <- function(dat, 
                                           ret_params,
                                           time_0 = min(dat[["Exposure"]]),
                                           time_100 = max(dat[["Exposure"]]),
                                           deut_part = 0.9){
  
  peptide_list <- unique(select(dat, Sequence, Start, End))
  
  res <- lapply(1:nrow(peptide_list), function(i){
    
    ret_scale <- filter(ret_params,
                        Sequence == peptide_list[[i, "Sequence"]],
                        Start == peptide_list[[i, "Start"]],
                        End == peptide_list[[i, "End"]]) %>% .[["ret_scale"]]
    
    pep_dat <- filter(dat, 
                      Sequence == peptide_list[[i, "Sequence"]],
                      Start == as.numeric(peptide_list[[i, "Start"]]),
                      End == as.numeric(peptide_list[[i, "End"]])) 
    
    MaxUptake <- unique(pep_dat[["MaxUptake"]])
    MHP <- unique(pep_dat[["MHP"]])
    Protein <- unique(pep_dat[["Protein"]])
    State <- unique(pep_dat[["State"]])
    
    tmp_dat <- HaDeX2::calculate_peptide_kinetics(pep_dat,
                                                  time_0 = time_0, 
                                                  time_100 = time_100) %>%
      mutate(sc_uptake = deut_uptake * ret_scale ) %>%
      select(Protein, Sequence, Start, End, State, Exposure, Modification, sc_uptake) 
    
    tmp_dat <- rbind(tmp_dat, c(Protein, peptide_list[[i, "Sequence"]], peptide_list[[i, "Start"]],
                                peptide_list[[i, "End"]], State, time_0, NA, 0))
    
    tmp_dat <- rbind(tmp_dat, c(Protein, peptide_list[[i, "Sequence"]], peptide_list[[i, "Start"]],
                                peptide_list[[i, "End"]], State, time_100, NA, deut_part*MaxUptake))
    
    tmp_dat <- tmp_dat %>% mutate(Fragment = "",
                                  Center = sc_uptake, 
                                  MaxUptake = MaxUptake, 
                                  MHP = MHP,
                                  File = paste0("file_", Exposure),
                                  Inten = 1, 
                                  z = 1,
                                  RT = 0) 
    
    tmp_dat
    
  }) %>% bind_rows()
  
  return(res)
  
}