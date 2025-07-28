#' Call the MyHospitals API
#'
#' @param api_url url
#'
#' @return something useful, usually a `data.frame`
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' call_myhosp_api("measure-downloads/myh-adm")
call_myhosp_api <- function(api_url) {
  # Create the base request
  req <- httr2::request(paste0(get_base_url(), api_url))

  # Choose the appropriate method and perform the request
  resp <- httr2::req_perform(req)

  # Check for errors based on the status code
  httr2::resp_check_status(resp)

  if (is_body_xlsx(resp)) {
    fpath <- write_resp(resp)
    return(read_aihw_xlsx(fpath))
  }

  # Return the parsed content of the response
  httr2::resp_body_json(resp)
}

get_base_url <- function() {
  "https://myhospitalsapi.aihw.gov.au/api/v1/"
}
