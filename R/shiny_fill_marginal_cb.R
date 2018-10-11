library(tidyverse)
library(shiny)
library(gtools)

#' fill_marginal_cb
#'
#' @param input_id
#'
#' @return
#' @export
#'
#' @examples
fill_marginal_cb <- function(input_id="cbMarginal") {
  selectInput(input_id,
              "Marginal display mode:",
              c("None (default)" = "none",
                "Histogram" = "histogram",
                "Boxplot" = "boxplot"),
              selected = "none")
}
