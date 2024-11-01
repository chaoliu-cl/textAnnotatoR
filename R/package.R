#' @keywords internal
"_PACKAGE"

# Declare global variables used in the package
if(getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "rv",
    "input",
    "session",
    "."  # For magrittr pipe operations
  ))
}

# Package initialization
.onLoad <- function(libname, pkgname) {
  # Any initialization code if needed
}

#' @importFrom stats complete.cases sd setNames
#' @importFrom grDevices colorRampPalette
#' @importFrom graphics axis image plot.new
#' @importFrom utils tail
#' @importFrom magrittr %>%
#' @importFrom shiny HTML addResourcePath column conditionalPanel div downloadButton downloadHandler
#' @importFrom shiny fluidRow formatStyle h4 helpText hr modalButton observe p renderText span strong
#' @importFrom shiny styleInterval tabsetPanel tagList uiOutput updateSelectInput verbatimTextOutput
#' @importFrom shiny wellPanel withProgress br
NULL
