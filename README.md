
<!-- README.md is generated from README.Rmd. Please edit that file -->

# textAnnotatoR: Interactive Text Annotation Tool for R

[![R-CMD-check](https://github.com/chaoliu-cl/textAnnotatoR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/chaoliu-cl/textAnnotatoR/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/textAnnotatoR)](https://CRAN.R-project.org/package=textAnnotatoR)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

## Overview

`textAnnotatoR` provides an interactive graphical user interface for
qualitative text analysis in R. The package allows researchers,
students, and practitioners to annotate text, manage codes, create
memos, and visualize coding patterns through an intuitive Shiny
interface.

## Key Features

- **Interactive Text Annotation**: Select and code text segments
  directly within the GUI
- **Code Management**: Create, organize, and merge codes with a
  hierarchical structure
- **Theme Organization**: Group related codes into themes with a
  tree-based hierarchy
- **Memo Creation**: Attach notes and observations to annotations
- **Advanced Visualization**: Analyze code frequencies, co-occurrences,
  and patterns
- **Comparison Tools**: Compare coding patterns between different coders
  or documents
- **Project Management**: Save, load, and manage annotation projects
- **Export Options**: Save annotations and coded text in various formats
  (CSV, JSON, HTML)
- **R Integration**: Seamlessly combine with other R packages for
  advanced analysis

## Installation

``` r
# Install from CRAN
install.packages("textAnnotatoR")

# Or install the development version from GitHub
# install.packages("devtools")
devtools::install_github("chaoliu-cl/textAnnotatoR")
```

## Getting Started

Launch the annotation interface with a simple function call:

``` r
library(textAnnotatoR)
annotate_gui()
```

This opens the Shiny application in your default web browser. The
interface includes a toolbar for project management, a tabbed main area
for different functions, and a text display area.

## Basic Usage

1.  **Create a New Project**: Click “New Project” in the top toolbar
2.  **Import Text**: Go to the “File” tab, upload your text document
    (.txt, .docx, .pdf)
3.  **Annotate Text**: Select text segments and apply codes
4.  **Organize Codes**: Create a hierarchical structure of themes and
    codes
5.  **Analyze Patterns**: Use the analysis tools to explore your coding
6.  **Export Results**: Save your annotations and analysis for further
    use

## Documentation

Comprehensive documentation is available through vignettes:

``` r
# List available vignettes
vignette(package = "textAnnotatoR")

# Read specific vignettes
vignette("practical_example", package = "textAnnotatoR")
vignette("technical_integration", package = "textAnnotatoR")
```

## Advanced Features

### Code Hierarchies

Create and manage hierarchical code structures with themes and
subthemes:

- Use “Add Theme” to create organizational categories
- Group related codes under appropriate themes
- Visualize the hierarchy in a tree structure

### Co-occurrence Analysis

Explore relationships between different codes:

- Identify patterns of code co-occurrence
- Visualize connections through network graphs
- Examine statistical measures of code relationships

### Comparison Tools

Compare coding patterns between different coders or documents:

- Upload two annotation sets
- Analyze similarities and differences
- Visualize comparative patterns

### Data Export

Export your annotations in various formats:

- CSV for quantitative analysis
- JSON for web applications
- HTML for formatted viewing with code highlighting

## Integration with R Ecosystem

`textAnnotatoR` is designed to work seamlessly with other R packages:

- **tidytext**: For text mining and natural language processing
- **quanteda**: For advanced text analysis
- **igraph/ggraph**: For network visualizations of code relationships
- **rmarkdown/shiny**: For reporting and interactive dashboards

## System Requirements

- R version 4.0.0 or higher
- Shiny and its dependencies
- A modern web browser

## Contributing

Contributions to `textAnnotatoR` are welcome! Please feel free to submit
issues or pull requests on GitHub.

## License

This package is licensed under the GPL-3 License.

## Contact

- Issues: Please report issues on the [GitHub issues
  page](https://github.com/chaoliu-cl/textAnnotatoR/issues)
- Email: <chaoliu@cedarville.edu>
- X: [@X](https://x.com/ChaoLiu77600168)
