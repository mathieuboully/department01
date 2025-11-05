#' Returns raw and enhanced simulation results
#'
#' @param svs raw simulation results
#' @param llp_potentials data with LLP info (name, potential, type, pn) by engine
#' @param distinction_llp make the distinction between llp and freckles (r65, r88...)
#' @param keep_only_first_sv keep only the first simulated shop visit or not
#' @return enriched simulation
add_info_simu <- function(svs, llp_potentials,
                          distinction_llp = TRUE, keep_only_first_sv = FALSE)
{
  # Add time date, delivery information and shop visit rank
  message("Add vars to simulation")
  
}
