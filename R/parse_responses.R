#' Convert chat completion response to a tibble
#'
#' This function takes a single chat completion response from the OpenAI API
#' and converts it into a tibble.
#'
#' @import purrr
#' @import tibble
#' @import dplyr
#'
#' @param response A list containing a chat completion response from the OpenAI API.
#' @param other_data other metadata to include in the resulting tibble, for example temperature etc.
#' @return A tibble containing the relevant information from the chat completion response.
#' @export
#' @examples
#' \dontrun{
#'   # Assuming `result` is a single response from the OpenAI API
#'   tibble_result <- chat_completion_to_tibble(result)
#' }
chat_completion_to_tibble <- function(response, other_data = NULL) {
  row <- c(response[c('id', 'object', 'created', 'model')], purrr::flatten(response$usage))
  messages <- response$choices |>
    purrr::map(purrr::list_flatten) |>
    purrr::map(as_tibble) |>
    purrr::list_rbind()
  addl_data <- as_tibble(other_data)
  dplyr::bind_cols(row, messages, addl_data)
}

#' Convert multiple chat completions to a tibble
#'
#' This function takes a list of chat completion responses from the OpenAI API
#' and converts them into a single tibble.
#'
#' @param completions A list containing multiple chat completion responses from the OpenAI API.
#' @param other_data other metadata to include in the resulting tibble, for example temperature etc.
#' @return A tibble containing the chat completion responses.
#' @export
chat_completions_to_tibble <- function(completions, other_data = NULL) {
  completions |>
    purrr::map(chat_completion_to_tibble, other_data = other_data) |>
    purrr::list_rbind()
}
