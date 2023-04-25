#' Send multiple prompts to the OpenAI API
#'
#' This function sends multiple prompts to the OpenAI API for completion, handling them in parallel.
#' It is built on top of the `future_map` function from the furrr package. Make sure to run future::plan(multisession)
#' to take advantage of parallelism.
#'
#' @import rlang
#' @import furrr
#' @importFrom rlang list2
#' @param prompts A vector of prompts to be sent to the OpenAI API.
#' @param n An integer specifying the number of completions to return for each prompt.
#' @param ... Additional arguments to be passed to the `quick_chat_completion` function.
#' @param credentials (Optional) An API key. Optional and will default to the default value set in the wrapper.
#' @param .progress A logical value indicating whether to display a progress bar.
#' @return A list of responses from the OpenAI API for each prompt.
#' @export
#' @examples
#' \dontrun{
#'   prompts <- c("Once upon a time", "In a galaxy far, far away")
#'   n <- 5
#'   results <- send_prompts(prompts, n)
#' }
send_prompts <- function(prompts, n, ..., credentials = NULL, .progress = FALSE) {
  stopifnot("Cannot request more than 100 responses at a time" = n <= 100)
  args <- list2(..., n = n)
  if (!is.null(credentials)) {
    args <- c(args, list(credentials))
  }
  output <- furrr::future_map(prompts, \(x) do.call(quick_chat_completion, list2(x, !!!args)), .progress =  .progress)
  chat_completions_to_tibble(output, other_data = list(...))
}

