#' OPCD code validation
#'
#' @param code a String of a single OPSC code to be checked
#'
#' @return Boolean and if FLASE then CLI alert
#' @examples
#' codes <- c("A01.3", "S12","A12, A13, A14", "12.3", "A3.2", "Z12.23")
#' for (c in codes){
#' try(check_OPCS(c),)
#' }
#'

check_OPCS <- function(code){

  if(is.na(code)){
    return(NA)
  }else if(nchar(code) > 5){
    cli::cli_abort("Non-valid OPCD code provided.\n You provided the code {.val {code}} this is too long, if you have see mutiple codes then ensure they are passed as a vector")
    return(FALSE)
  }else if(!stringr::str_sub(code, end=1) %in% LETTERS){
    cli::cli_abort("Non-valid OPCD code provided.\n You provided the code {.val {code}} but this doesnt start with a UPPER CASE letter")
    return(FALSE)
  }else if(!stringr::str_sub(code, start=2, end=3) %in% c(paste0(0, 1:9), 10:99)){
    cli::cli_abort("Non-valid OPCD code provided.\n You provided the code {.val {code}} but the second and third characters must be numbers. If it is a single digit number then include a leading 0 (eg A7 ->  A07)")
    return(FALSE)
  }else if(!(nchar(code) == 3 |nchar(code) ==  5)){
    cli::cli_abort("Non-valid OPCD code provided.\n You provided the code {.val {code}} but it must be either 3 (short code) or 5 characters (full code) long including the full stop.")
    return(FALSE)
  }else{
    return(TRUE)
  }
}


