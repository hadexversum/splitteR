#' Create peptide sequence list
#' 
#' @param dat daa with peptide sequences
#' 
#' @importFrom dplyr select rowwise
#' 
#' @description
#' This is a wrapper function for
#' get_sequence_bonds - there are the details.
#' 
#' @return a peptide list with column containg
#' sequence in convention.
#' 
#' @examples
#' head(create_sequence_list(alpha_dat))
#' 
#' @export

create_sequence_list <- function(dat){
  
  peptide_list <- dat %>%
    select(Sequence, Start, End) %>%
    unique(.) %>%
    rowwise() %>%
    mutate(seq_bond = get_sequence_bonds(sequence = Sequence),
           n_bonds = stringr::str_count(seq_bond, "[A-Z]"))
  
  return(peptide_list)
}

#' Replace sequence with sequences within convention
#' 
#' @param dat data with sequences to be replaced
#' 
#' @description The original peptide sequences are 
#' replaced with the peptide convention - described in 
#' the get_sequence_bonds function. Moreover the 
#' maximal uptake value is calculated based on the 
#' number of exchangeable bonds according to the convention.
#' 
#' @examples
#' replace_sequences(alpha_dat)
#' 
#' @export

replace_sequences <- function(dat){
  
  new_peptide_list <- create_sequence_list(dat)
  
  fin <- merge(dat, new_peptide_list, by = c("Sequence", "Start", "End")) %>%
    select(-Sequence, -MaxUptake) %>%
    rename(Sequence = seq_bond,
           MaxUptake = n_bonds) %>%
    select(Sequence, everything()) %>%
    arrange(Start, End)
  
  return(fin)
}