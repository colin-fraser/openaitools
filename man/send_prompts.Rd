% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chatgpt_experiments.R
\name{send_prompts}
\alias{send_prompts}
\title{Send multiple prompts to the OpenAI API}
\usage{
send_prompts(prompts, n, ..., credentials = NULL, .progress = FALSE)
}
\arguments{
\item{prompts}{A vector of prompts to be sent to the OpenAI API.}

\item{n}{An integer specifying the number of completions to return for each prompt.}

\item{...}{Additional arguments to be passed to the \code{quick_chat_completion} function.}

\item{credentials}{(Optional) An API key. Optional and will default to the default value set in the wrapper.}

\item{.progress}{A logical value indicating whether to display a progress bar.}
}
\value{
A list of responses from the OpenAI API for each prompt.
}
\description{
This function sends multiple prompts to the OpenAI API for completion, handling them in parallel.
It is built on top of the \code{future_map} function from the furrr package. Make sure to run future::plan(multisession)
to take advantage of parallelism.
}
\examples{
\dontrun{
  prompts <- c("Once upon a time", "In a galaxy far, far away")
  n <- 5
  results <- send_prompts(prompts, n)
}
}
