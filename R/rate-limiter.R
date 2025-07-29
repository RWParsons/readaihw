#' Rate Limiting for AIHW API
#' @description Internal rate limiting functionality to prevent API quota exhaustion

# Rate limiter state storage
.rate_state <- new.env(parent = emptyenv())

#' Calculate delay between API requests
#'
#' @return Numeric delay in seconds based on recent activity and errors
#' @noRd
get_delay <- function() {
  if (is.null(.rate_state$last_request)) {
    .rate_state$last_request <- Sys.time()
    .rate_state$error_count <- 0
    return(0)
  }

  time_since_last <- as.numeric(Sys.time() - .rate_state$last_request, units = "secs")
  base_delay <- 2  # Base delay between requests

  # Add exponential backoff for errors
  if (.rate_state$error_count > 0) {
    base_delay <- base_delay * (2 ^ min(.rate_state$error_count, 4))
  }

  # Ensure minimum time between requests
  needed_delay <- max(0, base_delay - time_since_last)
  return(needed_delay)
}

#' Handle HTTP response and determine retry strategy
#'
#' @param resp httr2 response object
#' @return Logical indicating success (TRUE) or retry needed (FALSE)
#' @noRd
handle_response <- function(resp) {
  .rate_state$last_request <- Sys.time()

  if (httr2::resp_status(resp) == 429) {
    .rate_state$error_count <- (.rate_state$error_count %||% 0) + 1
    retry_after <- as.numeric(httr2::resp_header(resp, "retry-after") %||% 60)
    warning(glue::glue("Rate limited. Waiting {retry_after} seconds."))
    Sys.sleep(retry_after)
    return(FALSE)  # retry needed
  } else if (httr2::resp_status(resp) >= 500) {
    .rate_state$error_count <- (.rate_state$error_count %||% 0) + 1
    Sys.sleep(5)  # brief pause for server errors
    return(FALSE)  # retry needed
  } else {
    .rate_state$error_count <- 0  # reset on success
    return(TRUE)  # success
  }
}
