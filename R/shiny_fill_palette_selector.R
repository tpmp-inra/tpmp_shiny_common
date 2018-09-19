library(tidyverse)
library(shiny)
library(gtools)

fill_palette_selector <- function(input_id="cbPaletteSelector") {
  selectInput(input_id,
              "Color Palette:",
              c("Accent" = "Accent",
                "Spectral" = "Spectral",
                "RdYlGn" = "RdYlGn",
                "RdYlBu" = "RdYlBu",
                "Set1" = "Set1",
                "Set2" = "Set2",
                "Set3" = "Set3",
                "Pastel1" = "Pastel1",
                "Pastel2" = "Pastel2",
                "Paired" = "Paired",
                "Dark2" = "Dark2",
                "Blues" = "Blues"
              ))
}
