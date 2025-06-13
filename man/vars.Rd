% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vars.R
\docType{data}
\name{vars}
\alias{vars}
\title{vars data}
\format{
A list containing the following components:
\describe{
\itemize{
  \item \code{raw_LLM}: A dataframe containing the unprocessed LLM output along with some other LLM information, including:
    \describe{
    \itemize{
      \item \code{function_part}: Part of function from which LLM output originates.
      \item \code{iteration}: Iteration number.
      \item \code{LLM_model}: LLM model used.
      \item \code{prompt}: Prompt used.
      \item \code{system_prompt}: System prompt used.
      \item \code{content}: Unprocessed LLM output.
      \item \code{finish_reason}: Reason the LLM stopped generating output.
      \item \code{prompt_tokens}: Number of tokens used for the LLM prompt.
      \item \code{answer_tokens}: Number of tokens used for the LLM answer.
      \item \code{total_tokens}: Total number of tokens used.
      \item \code{error}: Error message, if any occurred.
    }} \cr
  \item \code{all_vars}: A vector containing the integrated variable list with 52 variables.
  \item \code{final_list}: A vector containing the final variable list with 10 variables.
}
}
}
\source{
Generated using the OpenAI platform: \url{https://platform.openai.com}
}
\usage{
vars
}
\description{
This data set contains 2 lists of variables related to the context of addiction.
The data was generated using the \code{\link{var_list}} function and the "gpt-4o" Large Language Model (LLM).
}
\examples{
data("vars")
}
\keyword{datasets}
