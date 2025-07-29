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
  max_retries <- 3

  for (attempt in 1:max_retries) {
    # Apply dynamic delay
    delay <- get_delay()
    if (delay > 0) Sys.sleep(delay)

    # Create the request
    req <- httr2::request(paste0(get_base_url(), api_url))

    tryCatch({
      resp <- httr2::req_perform(req)

      # Handle rate limiting
      if (handle_response(resp)) {
        # Success - proceed with original logic
        httr2::resp_check_status(resp)

        if (is_body_xlsx(resp)) {
          fpath <- write_resp(resp)
          return(read_aihw_xlsx(fpath))
        }

        return(httr2::resp_body_json(resp))
      }
      # If handle_response returns FALSE, retry

    }, error = function(e) {
      if (attempt == max_retries) stop(e)
      Sys.sleep(2)  # brief pause before retry 
    })
  }

  stop("API call failed after ", max_retries, " attempts")
}

get_base_url <- function() {
  "https://myhospitalsapi.aihw.gov.au/api/v1/"
}
