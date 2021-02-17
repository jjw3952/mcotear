.onLoad <- function(libname, pckgname){
  ggplot2::theme_set(ggplot2::theme_bw())

  assign("scale_colour_discrete", function(..., values = cbbPalette) scale_colour_manual(..., values = values), globalenv())
  assign("scale_fill_discrete", function(..., values = cbbPalette) scale_fill_manual(..., values = values), globalenv())
}

.onUnload <- function(libpath){}