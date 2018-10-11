library(tidyverse)
library(shiny)
library(gtools)

#' fill_normalization_cb
#'
#' @param input_id
#'
#' @return
#' @export
#'
#' @examples
fill_normalization_cb <- function(input_id="cbNormalizationMethod"){
  selectInput(input_id,
              "Data normalization method:",
              c("Normalization" = "normalization",
                "Scale (default)" = "scale",
                "None (not recommended)" = "none"),
              selected = "scale")
}
