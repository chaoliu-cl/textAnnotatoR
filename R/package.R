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
#' @importFrom graphics axis image plot.new abline rect segments
#' @importFrom utils tail head
#' @importFrom magrittr %>%
#' @importFrom shiny HTML addResourcePath column conditionalPanel div downloadButton downloadHandler
#' @importFrom shiny fluidRow h4 helpText hr modalButton observe p renderText span strong
#' @importFrom shiny tabsetPanel tagList uiOutput updateSelectInput verbatimTextOutput
#' @importFrom DT formatStyle styleInterval
#' @importFrom shiny wellPanel withProgress br textOutput radioButtons
NULL
