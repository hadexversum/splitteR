#' @examples
#' pep_dat <- dplyr::filter(alpha_dat, Sequence == "LKSPAG", Start == 5, End == 10, State == "Alpha_KSCN")
#' ret_scale <- create_retention_dataset(alpha_dat, state = "Alpha_KSCN", time_0 = min(alpha_dat[["Exposure"]]), 
#'                                       time_100 = max(alpha_dat[["Exposure"]]), deut_part = 0.9) %>%
#'                                       filter(Sequence == "LKSPAG", Start == 5, End == 10) %>% .[["ret_scale"]]
#' @export 
calculate_rescaled_uptake <- function(pep_dat, 
                                      ret_scale,
                                      time_0 = min(pep_dat[["Exposure"]]),
                                      time_100 = max(pep_dat[["Exposure"]]),
                                      deut_part = 0.9){
  
  MaxUptake <- unique(pep_dat[["MaxUptake"]])
  MHP <- unique(pep_dat[["MHP"]])
  Protein <- unique(pep_dat[["Protein"]])
  State <- unique(pep_dat[["State"]])
  
  Sequence <- unique(pep_dat[["Sequence"]])
  Start <- as.numeric(unique(pep_dat[["Start"]]))
  End <- as.numeric(unique(pep_dat[["End"]]))
  
  kin_dat <- calculate_deut_uptake(dat = pep_dat, 
                                   time_0 = time_0,
                                   deut_part = deut_part,
                                   state = State) %>%
    mutate(deut_uptake = deut_uptake * ret_scale)
    
  
  tmp_dat <- kin_dat %>% mutate(Fragment = "",
                                Center = deut_uptake, 
                                MaxUptake = MaxUptake, 
                                MHP = MHP,
                                Modification = NA,
                                File = paste0("file_", Exposure),
                                Inten = 1, 
                                z = 1,
                                RT = 0) %>%
    mutate(Start = as.numeric(Start),
           End = as.numeric(End),
           Exposure = as.numeric(Exposure),
           Center = as.numeric(Center)) %>%
    arrange(Exposure) %>%
    select(-deut_uptake, -err_deut_uptake)
  
  tmp_dat
  
  
}

#' @importFrom dplyr arrange
#' 
#' @param dat data to be rescaled
#' @param ret_params frame with parameters
#' @param scaling_value value used to rescale values
#' @param state biological state
#' @param time_0 time of undeuterated measurement
#' @param time_100 time of FD measurement
#' @param deut_part procentage of deuterium in buffer
#' @param for_downlad indicator if the data shoul be in 
#' form coherent with uploadable file
#' 
#' @examples
#' ret_params <- create_retention_dataset(alpha_dat, state = "Alpha_KSCN")
#' x <- create_rescaled_uptake_dataset(alpha_dat, ret_params, state = "Alpha_KSCN")
#' xx <- create_rescaled_uptake_dataset(alpha_dat, ret_params, for_download = TRUE)
#' 
#' 
#' @export

create_rescaled_uptake_dataset <- function(dat, 
                                           ret_params,
                                           scaling_value = "ret_scale_2",
                                           state = dat[["State"]][1],
                                           time_0 = min(dat[["Exposure"]]),
                                           time_100 = max(dat[["Exposure"]]),
                                           deut_part = 0.9,
                                           for_download = FALSE){
  
  peptide_list <- unique(select(dat, Sequence, Start, End))
  
  dat <- filter(dat, State == state)
  
  print(paste0("Creating rescaled dataset for... ", state))
  
  res <- lapply(1:nrow(peptide_list), function(i){
    
    ret_scale <- filter(ret_params,
                        Sequence == peptide_list[[i, "Sequence"]],
                        Start == peptide_list[[i, "Start"]],
                        End == peptide_list[[i, "End"]]) %>% .[[scaling_value]]
    
    pep_dat <- filter(dat, 
                      Sequence == peptide_list[[i, "Sequence"]],
                      Start == as.numeric(peptide_list[[i, "Start"]]),
                      End == as.numeric(peptide_list[[i, "End"]])) 
    
    MaxUptake <- unique(pep_dat[["MaxUptake"]])
    MHP <- unique(pep_dat[["MHP"]])
    Protein <- unique(pep_dat[["Protein"]])
    State <- unique(pep_dat[["State"]])
    
    kin_dat <- calculate_deut_uptake(dat = pep_dat, 
                                     time_0 = time_0,
                                     deut_part = deut_part,
                                     state = State) %>%
      mutate(deut_uptake = deut_uptake * ret_scale)
    
    tmp_dat <- if(for_download){
      
      kin_dat %>% 
        mutate(Fragment = "",
               Center = as.numeric(deut_uptake), 
               MaxUptake = MaxUptake, 
               MHP = MHP,
               File = paste0("file_", Exposure),
               Inten = 1, 
               z = 1,
               RT = 0) %>%
        mutate(Start = as.numeric(Start),
               End = as.numeric(End),
               Exposure = as.numeric(Exposure),
               Modification = Modification,
               Center = as.numeric(Center)) %>%
        arrange(Start, End, Exposure) %>%
        select(-deut_uptake, -err_deut_uptake)
      
    } else {
      
      kin_dat %>% 
        mutate(Center = as.numeric(deut_uptake), 
               MaxUptake = as.numeric(MaxUptake), 
               MHP = as.numeric(MHP),
               File = paste0("file_", Exposure),
               Inten = 1, 
               z = 1) %>%
        mutate(Start = as.numeric(Start),
               End = as.numeric(End),
               Modification = Modification,
               Exposure = as.numeric(Exposure),
               Center = as.numeric(Center)) %>%
        arrange(Start, End, Exposure) %>%
        select(-deut_uptake, -err_deut_uptake)
      
    }
     
    
    tmp_dat
    
  }) %>% bind_rows()
  
  return(res)
  
}