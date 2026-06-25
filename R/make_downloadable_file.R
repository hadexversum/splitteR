#' Transforms table to downloadable format
#' 
#' @importFrom tidyr pivot_longer
#' @importFrom readr parse_number
#' 
#' @param tmp_dat uptake data
#' 
#' @return 
#' Function returns adjusted table
#' 
#' @description
#' This function transforms uptake data table 
#' into structure that is acceptable as input file 
#' by both HaDeX2 and HRaDeX - it mocks the DynamX
#' file. The function returns table that can be saved 
#' csv file and further used.
#' 
#' @examples
#' pep_dat <- calculate_deut_uptake(alpha_dat,
#'                                  state = "Alpha_KSCN",
#'                                  sequence = "GFGDLKSPAGL",
#'                                  start = 1,
#'                                  end = 11,
#'                                  time_0 = 0)
#' make_downloadable_file(pep_dat)
#' 
#'  
#' @export


make_downloadable_file <- function(tmp_dat){
  
  out_colnames <- c("Sequence", "Start", "End", "Protein", "Modification", "MHP", "State", "Exposure", "File", "z", "Inten", "Center", "MaxUptake")
  
  if(all(c("Center", "File", "z") %in% colnames(tmp_dat))) message("Already good file format.")
 
  ## TODO: clean
 
  
  x <- tmp_dat %>%
      mutate(deut_uptake_2 = deut_uptake + err_deut_uptake,
             deut_uptake_3 = deut_uptake - err_deut_uptake) %>%
      rename(deut_uptake_1 = deut_uptake) %>% 
    pivot_longer(
      cols = starts_with("deut_uptake"),
      names_to = "rep",
      values_to = "deut_uptake") %>%
    mutate(rep = parse_number(rep)) %>%
    mutate(File = paste0("rep_", Exposure, "_", rep)) %>%
    select(-err_deut_uptake, -rep) %>%
    rename(Center = deut_uptake) %>%
    mutate(z = 1,
           Fragment = "",
           Inten = 1,
           MHP = 0, 
           RT = 1)
  
  if(all(out_colnames %in% colnames(x))) message("Done!")

  return(x)

}
