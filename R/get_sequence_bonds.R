#' Rewrites the sequence based on the exchanging bonds
#' 
#' @param sequence peptide sequence
#' @param thereshold cut-off value of hamuro retention
#' 
#' @importFrom dplyr coalesce
#' 
#' @description
#' In this convention, the residue in upper case indicates a presence
#' of peptide bond proceeding it that is able to undergo the exchange.
#' The first residue is by default in lower case as it is not proceeded 
#' by exchangable hydrogen from peptide bond. Second residue, based on the
#' calculated value of hamuro retention between pair of first and second 
#' residue, is either in lower or upper case, depending on the threshold.
#' The rest of residues are in upper case, with exception of proline, that
#' due to its structure, is blocking the hydrogen from proceeding bond from exchange.
#' 
#' @return peptide sequence with lower and upper case
#' 
#' @examples
#' get_sequence_bonds("ALGKYGPADVE")
#' 
#' @export

get_sequence_bonds <- function(sequence,
                               threshold = 0.3){
  
  residues <- strsplit(sequence, "")[[1]]
  h_ret_first_2 <- coalesce(rate_n[residues[1], residues[2]], 0)
  
  res <- lapply(1:length(residues), function(i){
    
    if(i == 1) {
      tolower(residues[i])
    }
    else if(i == 2) {
      if(h_ret_first_2 < threshold)
        tolower(residues[i])
      else residues[i]
    } else if(residues[i] == "P"){
      tolower(residues[i])
    } else {
      residues[i]
    }
  }) %>% 
    unlist() %>%
    paste(., collapse = "")
  
  return(res)
}

