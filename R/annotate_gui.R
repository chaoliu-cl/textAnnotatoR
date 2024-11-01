#' Text Annotation Tool for R
#'
#' @description A Shiny application for interactive text annotation and analysis
#'
#' @name textAnnotatoR
#' @docType package
#' @author Chao Liu
#'
#' @section Dependencies:
#' This package requires the following packages:
#' \itemize{
#'   \item shiny
#'   \item data.tree
#'   \item jsonlite
#'   \item shinydashboard
#'   \item DT
#'   \item readtext
#'   \item magrittr
#' }
#'
#' @keywords internal
"_PACKAGE"

#' Interactive Text Annotation Interface
#'
#' @title Text Annotation GUI
#' @description Launch an interactive Shiny application for text annotation and analysis.
#' The GUI provides tools for importing text, applying codes, creating memos,
#' and analyzing annotations through various visualizations.
#'
#' @details The annotation interface includes the following features:
#' \itemize{
#'   \item Text import and display
#'   \item Code application and management
#'   \item Memo creation and linking
#'   \item Project management (save/load)
#'   \item Annotation analysis tools
#'   \item Export capabilities
#' }
#'
#' @note This package provides functionality for users to interactively save files
#' through the Shiny interface. All file operations are explicitly initiated by
#' users through file dialogs, and no files are written automatically to the user's
#' system without their direct action and consent.
#'
#' @return Launches a Shiny application in the default web browser
#' @export
#'
#' @examples
#' \dontrun{
#' annotate_gui()
#' }
#'
#' @importFrom shiny runApp shinyApp fluidPage actionButton observeEvent renderUI
#'   showNotification showModal modalDialog removeModal updateTextAreaInput
#'   updateTextInput tabPanel fileInput renderTable renderPlot plotOutput
#'   tableOutput textInput textAreaInput selectInput checkboxGroupInput
#' @importFrom shinyjs useShinyjs toggle runjs
#' @importFrom data.tree Node
#' @importFrom jsonlite fromJSON toJSON write_json
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar dashboardBody tabBox
#' @importFrom DT renderDT formatStyle styleInterval datatable
#' @importFrom readtext readtext
#' @importFrom utils write.csv
#'
annotate_gui <- function() {
  # Try to set up resource path safely
  tryCatch({
    # For development, try local path first
    if (dir.exists("inst/www")) {
      addResourcePath("custom", "inst/www")
    } else {
      # Fall back to installed package path
      pkg_www <- system.file("www", package = "textAnnotatoR")
      if (pkg_www != "") {
        addResourcePath("custom", pkg_www)
      }
    }
  }, error = function(e) {
    warning("Could not set up resource path for custom files. Some UI elements might not display correctly.")
  })

  ui <- dashboardPage(
    dashboardHeader(title = span("textAnnotatoR", class = "brand-text")),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
      useShinyjs(),
      fluidRow(
        column(12,
               div(style = "margin-bottom: 15px",
                   actionButton("save_project", "Save Project", icon = icon("save")),
                   actionButton("load_project", "Load Project", icon = icon("folder-open")),
                   actionButton("new_project", "New Project", icon = icon("file"))
               )
        )
      ),
      tags$head(
        # Import Google Font for brand name
        tags$link(rel = "stylesheet",
                  href = "https://fonts.googleapis.com/css2?family=Gamja+Flower&display=swap"),
        tags$script(src = "https://code.jquery.com/ui/1.12.1/jquery-ui.min.js"),
        tags$link(rel = "stylesheet", type = "text/css", href = "https://code.jquery.com/ui/1.12.1/themes/base/jquery-ui.css"),
        tags$style(HTML("
          /* Custom brand text styling */
          .skin-blue .main-header .logo .brand-text {
            font-family: 'Gamja Flower', sans-serif !important;
            font-weight: 800 !important;
            letter-spacing: 1px !important;
            text-transform: none !important;
            color: #FFFFFF !important;
            font-size: 24px !important;
          }
          .skin-blue .main-header .logo:hover {
            background-color: #367fa9 !important;
          }

          body { background-color: #f0f0f0; }
          .char { cursor: pointer; }
          .highlighted { background-color: yellow; }
          .code-display {
            padding: 2px 5px;
            margin-right: 5px;
            border-radius: 3px;
            font-weight: bold;
            color: black;
          }
          #annotations_table { margin-top: 20px; overflow-y: auto; max-height: 200px; }
          .content-wrapper { margin-left: 0 !important; }
          .tab-content { padding-top: 20px; }
          #margin_icons {
            position: fixed;
            right: 0;
            top: 50%;
            transform: translateY(-50%);
            z-index: 1000;
            background-color: rgba(248, 249, 250, 0.8);
            padding: 10px 5px;
            border-top-left-radius: 5px;
            border-bottom-left-radius: 5px;
          }
          #left_margin_icons {
            position: fixed;
            left: 0;
            top: 50%;
            transform: translateY(-50%);
            z-index: 1000;
            background-color: rgba(248, 249, 250, 0.8);
            padding: 10px 5px;
            border-top-right-radius: 5px;
            border-bottom-right-radius: 5px;
          }
          #margin_icons .btn, #left_margin_icons .btn {
            display: block;
            width: 40px;
            height: 40px;
            border-radius: 50%;
            margin-bottom: 10px;
            padding: 0;
            background-color: #f8f9fa;
            border: 1px solid #dee2e6;
            transition: background-color 0.3s;
          }
          #margin_icons .btn:hover, #left_margin_icons .btn:hover {
            background-color: #e9ecef;
          }
          #margin_icons .btn i, #left_margin_icons .btn i {
            font-size: 1.4rem;
          }
          #main_content {
            margin-left: 50px;
            margin-right: 50px;
          }
          #text_display {
            background-color: white;
            border: 1px solid #dee2e6;
            border-radius: 5px;
            padding: 20px;
            margin-top: 20px;
            box-shadow: 0 2px 5px rgba(0,0,0,0.1);
            height: 400px;
            overflow-y: auto;
          }
          .nav-tabs-custom {
            box-shadow: 0 2px 5px rgba(0,0,0,0.1);
          }
          .nav-tabs-custom, #text_display {
            width: 100%;
            max-width: 100%;
            box-sizing: border-box;
          }
          #floating_text_window {
            display: none;
            position: fixed;
            width: 400px;
            height: 300px;
            top: 100px;
            left: 100px;
            background-color: white;
            border: 1px solid #ddd;
            border-radius: 5px;
            box-shadow: 0 2px 10px rgba(0,0,0,0.1);
            z-index: 1000;
          }
          #floating_text_window_header {
            padding: 10px;
            cursor: move;
            background-color: #f1f1f1;
            border-bottom: 1px solid #ddd;
          }
          #floating_text_content {
            padding: 10px;
            height: calc(100% - 40px);
            overflow-y: auto;
          }
          .theme-item, .code-item {
            display: inline-block;
            padding: 2px 4px;
            border-radius: 3px;
            transition: background-color 0.2s;
          }

          .theme-item:hover, .code-item:hover {
            background-color: rgba(0, 0, 0, 0.05);
          }

          .theme-item.selected, .code-item.selected {
            background-color: #e6f3ff;
          }

          #hierarchy_pre {
            font-family: monospace;
            line-height: 1.5;
            white-space: pre;
            margin: 10px 0;
            padding: 10px;
            background-color: #f8f9fa;
            border-radius: 4px;
            border: 1px solid #dee2e6;
          }
        ")),
        tags$head(tags$script(HTML(addJS))),
        tags$script(HTML("
        $(document).ready(function() {
          $(document).on('click', '.theme-item, .code-item', function(e) {
            e.preventDefault();
            e.stopPropagation();

            // Get the item name
            var itemName = $(this).text();

            // Send the selected item to Shiny
            Shiny.setInputValue('selected_theme', itemName);
          });

          // Add hover effects for better UX
          $(document).on('mouseenter', '.theme-item, .code-item', function() {
            $(this).css({
              'cursor': 'pointer',
              'text-decoration': 'underline'
            });
          }).on('mouseleave', '.theme-item, .code-item', function() {
            $(this).css({
              'cursor': 'default',
              'text-decoration': 'none'
            });
          });
        });
          $(document).ready(function() {
            $('#floating_text_window').draggable({
              handle: '#floating_text_window_header',
              containment: 'window'
            });
            $('#floating_text_window').resizable();
          });
          function toggleTextWindow() {
            $('#floating_text_window').toggle();
          }
        "))
      ),
      # Add a button to toggle the floating text window
      actionButton("toggle_text_window", "Toggle Text Window"),

      # Add the floating text window
      div(id = "floating_text_window",
          div(id = "floating_text_window_header", "Text Display"),
          div(id = "floating_text_content", uiOutput("floating_text_display"))
      ),
      div(id = "margin_icons",
          actionButton("select", "", icon = icon("mouse-pointer")),
          actionButton("clear", "", icon = icon("eraser"))
      ),
      div(id = "left_margin_icons",
          actionButton("undo", "", icon = icon("undo")),
          actionButton("redo", "", icon = icon("redo"))
      ),
      div(id = "main_content",
          tabBox(
            width = NULL,
            id = "tabset",
            tabPanel("File", icon = icon("file"),
                     fileInput("file_input", "Choose File",
                               accept = c(".txt", ".docx", ".pdf")),
                     actionButton("import_text", "Import Text")
            ),
            tabPanel("Code and Memo", icon = icon("code"),
                     textInput("code", "Code:"),
                     textAreaInput("memo", "Memo:", height = "100px"),
                     actionButton("save", "Save", icon = icon("save")),
                     actionButton("apply_code", "Apply Code", icon = icon("check")),
                     actionButton("merge_codes", "Merge Codes", icon = icon("object-group")),
                     actionButton("save_code", "Save Annotated Text", icon = icon("download"))
                     #actionButton("rename_code", "Rename Code", icon = icon("edit")),
                     #actionButton("delete_code", "Delete Code", icon = icon("trash")),
                     #actionButton("display_code", "Display Code", icon = icon("eye"))
            ),
            tabPanel("Themes", icon = icon("folder-tree"),
                     fluidRow(
                       column(4,
                              wellPanel(
                                h4("Theme Management"),
                                actionButton("add_theme_btn", "Add Theme", icon = icon("plus")),
                                actionButton("add_code_to_theme_btn", "Add Code to Theme", icon = icon("code")),
                                actionButton("move_item_btn", "Move Item", icon = icon("arrows-alt")),
                                hr(),
                                actionButton("export_hierarchy_btn", "Export Hierarchy", icon = icon("download")),
                                actionButton("import_hierarchy_btn", "Import Hierarchy", icon = icon("upload"))
                              ),
                              wellPanel(
                                h4("Hierarchy Statistics"),
                                verbatimTextOutput("hierarchy_stats")
                              )
                       ),
                       column(8,
                              wellPanel(
                                h4("Code Hierarchy"),
                                uiOutput("hierarchy_view")
                              ),
                              wellPanel(
                                h4("Theme Details"),
                                uiOutput("theme_details")
                              )
                       )
                     )),
            #tabPanel("Tools", icon = icon("tools"),
            #         actionButton("link_memo", "Link Memo", icon = icon("link")),
            #         actionButton("generate_codebook", "Generate Code Book", icon = icon("book"))
            #),
            tabPanel("Analysis", icon = icon("chart-bar"),
                     actionButton("code_frequency", "Code Frequency", icon = icon("chart-bar")),
                     actionButton("code_co_occurrence", "Code Co-occurrence", icon = icon("project-diagram")),
                     actionButton("word_cloud", "Word Cloud", icon = icon("cloud")),
                     actionButton("text_summary", "Text Summary", icon = icon("file-alt"))
            ),
            #tabPanel("Import/Export", icon = icon("exchange-alt"),
            #         actionButton("import_annotations", "Import Annotations", icon = icon("file-import")),
            #         actionButton("export_annotations", "Export Annotations", icon = icon("file-export"))
            #),
            tabPanel("Records", icon = icon("table"),
                     div(id = "annotations_table", DTOutput("annotations")),
                     actionButton("save_records", "Save Records", icon = icon("save"))
            ),
            tabPanel("Comparison", icon = icon("balance-scale"),
                     fluidRow(
                       column(4,
                              wellPanel(
                                h4("Import Comparison Data"),
                                # First file upload
                                fileInput("comparison_file1",
                                          "Upload First File (CSV/JSON)",
                                          multiple = FALSE,
                                          accept = c(".csv", ".json")),
                                # Show first file info when uploaded
                                uiOutput("file1_info"),

                                # Second file upload
                                fileInput("comparison_file2",
                                          "Upload Second File (CSV/JSON)",
                                          multiple = FALSE,
                                          accept = c(".csv", ".json")),
                                # Show second file info when uploaded
                                uiOutput("file2_info"),

                                # Add reset button
                                actionButton("reset_comparison", "Reset Files",
                                             icon = icon("trash-alt"),
                                             class = "btn-warning"),

                                br(), br(),

                                # Run comparison button (disabled until both files are uploaded)
                                actionButton("run_comparison", "Run Comparison",
                                             icon = icon("play"))
                              ),
                              wellPanel(
                                h4("Comparison Settings"),
                                checkboxGroupInput("comparison_metrics",
                                                   "Select Analysis Types:",
                                                   choices = c(
                                                     "Coverage Patterns" = "coverage",
                                                     "Code Application" = "application",
                                                     "Code Overlaps" = "overlaps",
                                                     "Code Sequences" = "sequences"
                                                   ),
                                                   selected = c("coverage", "application"))
                              )
                       ),
                       column(8,
                              tabsetPanel(
                                id = "comparison_results_tabs",
                                tabPanel("Summary",
                                         verbatimTextOutput("comparison_summary")),
                                tabPanel("Visualization",
                                         selectInput("plot_type", "Select View:",
                                                     choices = c(
                                                       "Code Distribution" = "distribution",
                                                       "Code Overlaps" = "overlap",
                                                       "Code Sequences" = "sequence"
                                                     )),
                                         plotOutput("comparison_plot", height = "500px")),
                                tabPanel("Detailed Analysis",
                                         h4("Coverage Analysis"),
                                         verbatimTextOutput("coverage_details"),
                                         h4("Code Application Patterns"),
                                         verbatimTextOutput("application_details"),
                                         h4("Pattern Analysis"),
                                         verbatimTextOutput("pattern_details"))
                              )
                       )
                     ))
          ),
          uiOutput("text_display")
      )
    )
  )

  server <- function(input, output, session) {
    rv <- reactiveValues(
      text = "",
      annotations = data.frame(
        start = integer(),
        end = integer(),
        text = character(),
        code = character(),
        memo = character(),
        stringsAsFactors = FALSE
      ),
      codes = character(),
      history = list(list(text = "", annotations = data.frame())),
      history_index = 1,
      code_tree = Node$new("Root"),
      code_colors = character(),
      memos = list(),
      code_descriptions = list(),
      # Add new state tracking
      project_modified = FALSE,
      current_project = NULL,
      action_history = list(),
      action_index = 0,
      merged_codes = list(),
      # Add new theme-related initializations
      selected_theme = NULL,
      # Add new comparison-related initializations
      comparison_data = NULL,
      comparison_results = NULL
    )

    # Initialize code tree properties in an observe block
    observe({
      rv$code_tree$type <- "theme"
      rv$code_tree$description <- "Root of the code hierarchy"
    })

    # Add Theme button handler
    observeEvent(input$add_theme_btn, {
      theme_choices <- get_theme_paths(rv$code_tree)

      showModal(modalDialog(
        title = "Add New Theme",
        textInput("new_theme_name", "Theme Name:"),
        textAreaInput("theme_description", "Description:"),
        selectInput("parent_theme", "Parent Theme:",
                    choices = theme_choices,
                    selected = "Root"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_add_theme", "Add Theme")
        )
      ))
    })

    # Confirm Add Theme handler
    observeEvent(input$confirm_add_theme, {
      req(input$new_theme_name)

      tryCatch({
        if (input$parent_theme == "Root") {
          # Add theme directly to root
          new_theme <- rv$code_tree$AddChild(input$new_theme_name)
          new_theme$type <- "theme"
          new_theme$description <- input$theme_description
          new_theme$created <- Sys.time()
        } else {
          # Add theme to specified parent
          theme_path <- unlist(strsplit(input$parent_theme, " / "))
          current_node <- rv$code_tree

          # Navigate to parent theme
          for (theme in theme_path) {
            current_node <- current_node$children[[theme]]
            if (is.null(current_node)) {
              stop(paste("Parent theme not found:", theme))
            }
          }

          # Add new theme
          new_theme <- current_node$AddChild(input$new_theme_name)
          new_theme$type <- "theme"
          new_theme$description <- input$theme_description
          new_theme$created <- Sys.time()
        }

        showNotification("Theme added successfully", type = "message")
        removeModal()
      }, error = function(e) {
        showNotification(paste("Error adding theme:", e$message), type = "error")
      })
    })

    # Add Code to Theme button handler
    observeEvent(input$add_code_to_theme_btn, {
      theme_choices <- get_theme_paths(rv$code_tree)

      showModal(modalDialog(
        title = "Add Code to Theme",
        selectInput("code_to_add", "Select Code:",
                    choices = setdiff(rv$codes, get_all_themed_codes(rv$code_tree))),
        selectInput("theme_for_code", "Select Theme:",
                    choices = theme_choices),
        textAreaInput("code_description", "Code Description:"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_add_code", "Add Code")
        )
      ))
    })

    # Confirm Add Code handler
    observeEvent(input$confirm_add_code, {
      req(input$code_to_add, input$theme_for_code)

      tryCatch({
        if(input$theme_for_code == "Root") {
          # Add directly to root
          rv$code_tree <- add_code_to_theme(rv$code_tree,
                                            input$code_to_add,
                                            character(0),  # empty path for root
                                            input$code_description)
        } else {
          # Add to specified theme
          theme_path <- unlist(strsplit(input$theme_for_code, " / "))
          rv$code_tree <- add_code_to_theme(rv$code_tree,
                                            input$code_to_add,
                                            theme_path,
                                            input$code_description)
        }
        showNotification("Code added to theme successfully", type = "message")
        removeModal()
      }, error = function(e) {
        showNotification(paste("Error adding code:", e$message), type = "error")
      })
    })

    # Move Item button handler
    observeEvent(input$move_item_btn, {
      # Get all items that can be moved (both themes and codes)
      movable_items <- get_all_paths(rv$code_tree)

      # Get possible parent themes
      parent_themes <- get_theme_paths(rv$code_tree)

      showModal(modalDialog(
        title = "Move Item",
        selectInput("item_to_move", "Select Item to Move:",
                    choices = movable_items),
        selectInput("new_parent", "Select New Parent:",
                    choices = parent_themes),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_move", "Move Item")
        )
      ))
    })

    # Confirm Move handler
    observeEvent(input$confirm_move, {
      req(input$item_to_move, input$new_parent)

      tryCatch({
        # Split paths into components
        item_path <- unlist(strsplit(input$item_to_move, " / "))
        new_parent_path <- if(input$new_parent == "Root") {
          character(0)
        } else {
          unlist(strsplit(input$new_parent, " / "))
        }

        # Get the item to move
        current_node <- rv$code_tree
        parent_node <- NULL
        target_node <- NULL

        # Find the item to move
        for (path_component in item_path) {
          parent_node <- current_node
          current_node <- current_node$children[[path_component]]
          if (is.null(current_node)) {
            stop(paste("Cannot find item:", input$item_to_move))
          }
        }
        target_node <- current_node

        # Find the new parent
        new_parent_node <- rv$code_tree
        if (length(new_parent_path) > 0) {
          for (path_component in new_parent_path) {
            new_parent_node <- new_parent_node$children[[path_component]]
            if (is.null(new_parent_node)) {
              stop(paste("Cannot find new parent:", input$new_parent))
            }
          }
        }

        # Check for circular reference
        if (is_ancestor(target_node, new_parent_node)) {
          stop("Cannot move a node to its own descendant")
        }

        # Check if new parent is a theme
        if (!is.null(new_parent_node$type) && new_parent_node$type != "theme") {
          stop("Can only move items to theme nodes")
        }

        # Store item data
        item_data <- list(
          name = target_node$name,
          type = target_node$type,
          description = target_node$description,
          created = target_node$created,
          children = target_node$children
        )

        # Remove from old location
        parent_node$RemoveChild(target_node$name)

        # Add to new location
        new_node <- new_parent_node$AddChild(item_data$name)
        new_node$type <- item_data$type
        new_node$description <- item_data$description
        new_node$created <- item_data$created

        # Restore children if any
        if (length(item_data$children) > 0) {
          for (child in item_data$children) {
            restore_node(new_node, child)
          }
        }

        showNotification("Item moved successfully", type = "message")
        removeModal()
      }, error = function(e) {
        showNotification(paste("Error moving item:", e$message), type = "error")
      })
    })

    # Export hierarchy handler
    observeEvent(input$export_hierarchy_btn, {
      showModal(modalDialog(
        title = "Export Hierarchy",
        downloadButton("download_hierarchy", "Download JSON"),
        footer = modalButton("Close")
      ))
    })

    # Download handler for hierarchy export
    output$download_hierarchy <- downloadHandler(
      filename = function() {
        paste0("code_hierarchy_", format(Sys.time(), "%Y%m%d"), ".json")
      },
      content = function(file) {
        writeLines(export_hierarchy(rv$code_tree), file)
      }
    )

    # Import hierarchy handler
    observeEvent(input$import_hierarchy_btn, {
      showModal(modalDialog(
        title = "Import Hierarchy",
        fileInput("hierarchy_file", "Choose JSON file",
                  accept = c("application/json", ".json")),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_import", "Import")
        )
      ))
    })

    # Confirm Import handler
    observeEvent(input$confirm_import, {
      req(input$hierarchy_file)

      tryCatch({
        json_content <- readLines(input$hierarchy_file$datapath)
        imported_tree <- import_hierarchy(paste(json_content, collapse = "\n"))

        # Validate the imported tree
        if (is.null(imported_tree) || !inherits(imported_tree, "Node")) {
          stop("Invalid hierarchy structure in imported file")
        }

        # Ensure root node has required properties
        if (is.null(imported_tree$type)) {
          imported_tree$type <- "theme"
        }
        if (is.null(imported_tree$description)) {
          imported_tree$description <- "Root of the code hierarchy"
        }

        # Process all nodes to ensure they have required properties
        process_node <- function(node) {
          if (is.null(node$type)) {
            node$type <- "theme"
          }
          if (is.null(node$description)) {
            node$description <- ""
          }
          if (is.null(node$created)) {
            node$created <- Sys.time()
          }

          if (!is.null(node$children)) {
            lapply(node$children, process_node)
          }
          return(node)
        }

        rv$code_tree <- process_node(imported_tree)
        showNotification("Hierarchy imported successfully", type = "message")
        removeModal()

      }, error = function(e) {
        showNotification(paste("Error importing hierarchy:", e$message), type = "error")
      })
    })

    # Hierarchy view output
    output$hierarchy_view <- renderUI({
      # Force reactivity
      input$confirm_add_theme
      input$confirm_add_code
      input$confirm_move
      input$confirm_import

      HTML(paste0(
        '<pre id="hierarchy_pre">',
        visualize_hierarchy(rv$code_tree),
        '</pre>'
      ))
    })

    # Hierarchy statistics output
    output$hierarchy_stats <- renderText({
      # Force reactivity
      input$confirm_add_theme
      input$confirm_add_code
      input$confirm_move
      input$confirm_import

      stats <- calculate_hierarchy_stats(rv$code_tree)

      # Format the codes per theme section
      codes_per_theme_text <- if (length(stats$codes_per_theme) > 0) {
        paste("\nCodes per Theme:",
              paste(names(stats$codes_per_theme), ": ",
                    unlist(stats$codes_per_theme),
                    collapse = "\n"),
              sep = "\n")
      } else {
        "\nNo themes with codes yet"
      }

      # Combine all statistics
      paste0(
        "Total Themes: ", stats$total_themes, "\n",
        "Total Codes: ", stats$total_codes, "\n",
        "Maximum Depth: ", stats$max_depth, "\n",
        "Average Codes per Theme: ",
        sprintf("%.2f", stats$average_codes_per_theme),
        codes_per_theme_text
      )
    })

    # Theme details output
    output$theme_details <- renderText({
      # React to theme selection
      theme_name <- input$selected_theme

      if (is.null(theme_name)) {
        return("No theme selected. Click on a theme in the hierarchy to view details.")
      }

      # Find the selected theme in the hierarchy
      theme_node <- find_node_by_name(rv$code_tree, theme_name)

      if (is.null(theme_node)) {
        return(paste("Theme", theme_name, "not found in hierarchy."))
      }

      # Count direct codes and sub-themes with proper error handling
      n_codes <- sum(vapply(theme_node$children, function(x) {
        tryCatch({
          if (is.null(x$type)) return(FALSE)
          x$type == "code"
        }, error = function(e) FALSE)
      }, logical(1)))

      n_subthemes <- sum(vapply(theme_node$children, function(x) {
        tryCatch({
          if (is.null(x$type)) return(FALSE)
          x$type == "theme"
        }, error = function(e) FALSE)
      }, logical(1)))

      # Format the created timestamp safely
      created_time <- tryCatch({
        if (!is.null(theme_node$created)) {
          format(theme_node$created, "%Y-%m-%d %H:%M:%S")
        } else {
          format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        }
      }, error = function(e) format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

      # Safe description handling
      description <- tryCatch({
        if (!is.null(theme_node$description)) {
          theme_node$description
        } else {
          "No description"
        }
      }, error = function(e) "No description")

      # Build the output text
      output_text <- paste0(
        "Theme: ", theme_node$name, "\n",
        "Description: ", description, "\n",
        "Created: ", created_time, "\n",
        "Number of direct codes: ", n_codes, "\n",
        "Number of sub-themes: ", n_subthemes, "\n"
      )

      # Add code list if there are codes
      if (n_codes > 0) {
        codes <- tryCatch({
          vapply(theme_node$children[vapply(theme_node$children, function(x) {
            if (is.null(x$type)) return(FALSE)
            x$type == "code"
          }, logical(1))], function(x) x$name, character(1))
        }, error = function(e) character(0))

        if (length(codes) > 0) {
          output_text <- paste0(output_text,
                                "\nCodes in this theme:\n",
                                paste0("- ", codes, collapse = "\n"))
        }
      }

      # Add sub-themes list if there are sub-themes
      if (n_subthemes > 0) {
        subthemes <- tryCatch({
          vapply(theme_node$children[vapply(theme_node$children, function(x) {
            if (is.null(x$type)) return(FALSE)
            x$type == "theme"
          }, logical(1))], function(x) x$name, character(1))
        }, error = function(e) character(0))

        if (length(subthemes) > 0) {
          output_text <- paste0(output_text,
                                "\n\nSub-themes:\n",
                                paste0("- ", subthemes, collapse = "\n"))
        }
      }

      return(output_text)
    })

    # Add click handler for theme selection
    observeEvent(input$select_theme, {
      rv$selected_theme <- input$select_theme
    })

    # Helper function to get all theme paths
    get_theme_paths <- function(node) {
      paths <- c("Root")  # Start with Root as the first option

      collect_paths <- function(node, current_path = character()) {
        # Check if this node is a theme
        if (!is.null(node$type) && node$type == "theme" && !is.null(node$name)) {
          if (length(current_path) > 0) {
            full_path <- paste(c(current_path, node$name), collapse = " / ")
            paths <<- c(paths, full_path)
          } else if (node$name != "Root") {
            # Add single themes at root level
            paths <<- c(paths, node$name)
          }
        }

        # Recursively process children
        if (!is.null(node$children)) {
          new_path <- if (length(current_path) > 0) {
            c(current_path, node$name)
          } else if (node$name != "Root") {
            node$name
          } else {
            character(0)
          }

          for (child in node$children) {
            collect_paths(child, new_path)
          }
        }
      }

      collect_paths(node)
      return(unique(paths))
    }

    # Helper function to get all themed codes
    get_all_themed_codes <- function(node) {
      codes <- character()

      traverse_node <- function(node) {
        if (node$type == "code") {
          codes <<- c(codes, node$name)
        }
        if (length(node$children) > 0) {
          lapply(node$children, traverse_node)
        }
      }

      traverse_node(node)
      return(codes)
    }

    # Helper function to get all paths (themes and codes)
    get_all_paths <- function(node) {
      paths <- character()

      collect_paths <- function(node, current_path = character()) {
        if (!is.null(node$name) && node$name != "Root") {
          # Create the full path for this node
          full_path <- if (length(current_path) > 0) {
            paste(c(current_path, node$name), collapse = " / ")
          } else {
            node$name
          }
          # Add the path only if it's a code or theme
          if (!is.null(node$type) && (node$type == "theme" || node$type == "code")) {
            paths <<- c(paths, full_path)
          }
        }

        # Process children if they exist
        if (!is.null(node$children) && length(node$children) > 0) {
          for (child in node$children) {
            new_path <- if (length(current_path) > 0 && node$name != "Root") {
              c(current_path, node$name)
            } else if (node$name != "Root") {
              node$name
            } else {
              character(0)
            }
            collect_paths(child, new_path)
          }
        }
      }

      collect_paths(node)
      return(unique(paths))
    }

    # Initialize roots for shinyFiles
    roots <- c(Home = path.expand("~"))
    if (.Platform$OS.type == "windows") {
      roots <- c(roots, getVolumes()())
    }

    # Initialize directory/file choosing
    shinyDirChoose(input, "directory_select", roots = roots, session = session)
    shinyDirChoose(input, "text_directory_select", roots = roots, session = session)
    shinyDirChoose(input, "records_directory_select", roots = roots, session = session)
    shinyFileChoose(input, "file_select", roots = roots, session = session)

    # Save Project handler
    observeEvent(input$save_project, {
      save_project_interactive(rv, input, session, roots)
    })

    # Display selected save directory
    output$selected_dir <- renderText({
      if (!is.null(input$directory_select)) {
        parseDirPath(roots, input$directory_select)
      }
    })

    # Display selected text save directory
    output$selected_text_dir <- renderText({
      if (!is.null(input$text_directory_select)) {
        parseDirPath(roots, input$text_directory_select)
      }
    })

    # Display selected records directory
    output$selected_records_dir <- renderText({
      if (!is.null(input$records_directory_select)) {
        parseDirPath(roots, input$records_directory_select)
      }
    })


    # Merge codes functionality
    observeEvent(input$merge_codes, {
      showModal(modalDialog(
        title = "Merge Codes",
        checkboxGroupInput("codes_to_merge", "Select codes to merge:",
                           choices = rv$codes,
                           selected = NULL
        ),
        textInput("new_code_name", "New code name:"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_merge_codes", "Merge")
        )
      ))
    })

    # Add this observer for handling code merging
    observeEvent(input$confirm_merge_codes, {
      req(input$codes_to_merge, input$new_code_name)

      # Create merge action
      merge_action <- create_action(
        type = "merge_codes",
        data = list(
          old_codes = input$codes_to_merge,
          new_code = input$new_code_name
        )
      )

      # Apply and record the action
      apply_action(rv, merge_action)
      add_action(rv, merge_action)

      # Update codes list
      rv$codes <- unique(c(setdiff(rv$codes, input$codes_to_merge), input$new_code_name))

      removeModal()
      showNotification(paste("Codes merged into", input$new_code_name), type = "message")

      # Update UI
      output$text_display <- renderUI({
        HTML(update_text_display())
      })
    })

    #update_text_display function
    update_text_display <- function() {
      if (nrow(rv$annotations) == 0) {
        return(paste0("<span class='char' id='char_", 1:nchar(rv$text), "'>", strsplit(rv$text, "")[[1]], "</span>", collapse = ""))
      }

      sorted_annotations <- rv$annotations[order(rv$annotations$start), ]
      displayed_text <- ""
      last_end <- 0

      for (i in 1:nrow(sorted_annotations)) {
        if (sorted_annotations$start[i] > last_end + 1) {
          displayed_text <- paste0(displayed_text,
                                   paste0("<span class='char' id='char_", (last_end + 1):(sorted_annotations$start[i] - 1), "'>",
                                          strsplit(substr(rv$text, last_end + 1, sorted_annotations$start[i] - 1), "")[[1]],
                                          "</span>", collapse = ""))
        }
        code_color <- rv$code_colors[sorted_annotations$code[i]]
        if (is.null(code_color)) {
          code_color <- "#CCCCCC"  # Default color if not found
        }
        displayed_text <- paste0(displayed_text,
                                 "<span class='code-display' style='background-color: ", code_color, ";' data-code='", sorted_annotations$code[i], "' data-start='", sorted_annotations$start[i], "' data-end='", sorted_annotations$end[i], "'>",
                                 "[", sorted_annotations$code[i], "]",
                                 paste0("<span class='char' id='char_", sorted_annotations$start[i]:sorted_annotations$end[i], "'>",
                                        strsplit(substr(rv$text, sorted_annotations$start[i], sorted_annotations$end[i]), "")[[1]],
                                        "</span>", collapse = ""),
                                 "</span>")
        last_end <- sorted_annotations$end[i]
      }

      if (last_end < nchar(rv$text)) {
        displayed_text <- paste0(displayed_text,
                                 paste0("<span class='char' id='char_", (last_end + 1):nchar(rv$text), "'>",
                                        strsplit(substr(rv$text, last_end + 1, nchar(rv$text)), "")[[1]],
                                        "</span>", collapse = ""))
      }

      return(displayed_text)
    }

    observeEvent(input$replace_code, {
      showModal(modalDialog(
        title = "Replace Code",
        selectInput("new_code", "Select new code:", choices = rv$codes),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_replace_code", "Replace")
        )
      ))
    })

    observeEvent(input$confirm_replace_code, {
      removeModal()
      code_to_replace <- input$replace_code$code
      new_code <- input$new_code
      start <- input$replace_code$start
      end <- input$replace_code$end

      idx <- which(rv$annotations$start == start & rv$annotations$end == end & rv$annotations$code == code_to_replace)
      if (length(idx) > 0) {
        rv$annotations$code[idx] <- new_code
        save_state()
        output$text_display <- renderUI({
          HTML(update_text_display())
        })
      }
    })

    observeEvent(input$rename_code, {
      showModal(modalDialog(
        title = "Rename Code",
        textInput("new_code_name", "Enter new code name:"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_rename_code", "Rename")
        )
      ))
    })

    observeEvent(input$confirm_rename_code, {
      removeModal()
      old_code <- input$rename_code$code
      new_code <- input$new_code_name

      rv$codes <- unique(c(setdiff(rv$codes, old_code), new_code))
      rv$annotations$code[rv$annotations$code == old_code] <- new_code
      rv$code_colors[new_code] <- rv$code_colors[old_code]
      rv$code_colors <- rv$code_colors[names(rv$code_colors) != old_code]
      save_state()
      output$text_display <- renderUI({
        HTML(update_text_display())
      })
    })

    observeEvent(input$delete_code, {
      showModal(modalDialog(
        title = "Delete Code",
        p("Are you sure you want to delete this code?"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_delete_code", "Delete")
        )
      ))
    })

    observeEvent(input$confirm_delete_code, {
      removeModal()
      code_to_delete <- input$delete_code$code
      start <- input$delete_code$start
      end <- input$delete_code$end

      idx <- which(rv$annotations$start == start & rv$annotations$end == end & rv$annotations$code == code_to_delete)
      if (length(idx) > 0) {
        rv$annotations <- rv$annotations[-idx, ]
        if (!(code_to_delete %in% rv$annotations$code)) {
          rv$codes <- setdiff(rv$codes, code_to_delete)
          rv$code_colors <- rv$code_colors[names(rv$code_colors) != code_to_delete]
        }
        save_state()
        output$text_display <- renderUI({
          HTML(update_text_display())
        })
      }
    })

    # Save Annotated Text handler
    observeEvent(input$save_code, {
      save_annotated_text_interactive(rv, input, session, roots)
    })

    # Save annotated text confirmation handler
    observeEvent(input$confirm_save_annotations, {
      req(input$save_filename)
      req(input$text_directory_select)

      # Get selected directory path
      dir_path <- parseDirPath(roots, input$text_directory_select)

      filename <- input$save_filename
      if (!grepl(paste0("\\.", input$save_format, "$"), filename)) {
        filename <- paste0(filename, ".", input$save_format)
      }
      filepath <- file.path(dir_path, filename)

      tryCatch({
        dir.create(dirname(filepath), recursive = TRUE, showWarnings = FALSE)
        if (input$save_format == "html") {
          save_as_html(filepath)
        } else if (input$save_format == "txt") {
          save_as_text(filepath)
        }
        showNotification(paste("Annotated text saved to", filepath), type = "message")
      }, error = function(e) {
        showNotification(paste("Error saving annotated text:", e$message), type = "error")
      })

      removeModal()
    })

    # Function to save as HTML
    save_as_html <- function(filename) {
      # Get the current state of the text display
      html_content <- update_text_display()

      # Create a complete HTML document
      full_html <- paste0(
        "<!DOCTYPE html>\n<html>\n<head>\n",
        "<style>\n",
        ".code-display { padding: 2px 5px; margin-right: 5px; border-radius: 3px; font-weight: bold; color: black; }\n",
        "</style>\n",
        "</head>\n<body>\n",
        "<h1>Annotated Text</h1>\n",
        "<div id='annotated_text'>\n",
        html_content,
        "\n</div>\n",
        "</body>\n</html>"
      )

      # Write the HTML content to a file
      writeLines(full_html, filename)
    }

    # Function to save as text file
    save_as_text <- function(filename) {
      # Get the annotated text
      annotated_text <- create_plain_text_annotations()

      # Write the content to a file
      writeLines(annotated_text, filename)
    }

    # Helper function to create plain text with annotations
    create_plain_text_annotations <- function() {
      if (nrow(rv$annotations) == 0) {
        return(rv$text)
      }

      sorted_annotations <- rv$annotations[order(rv$annotations$start), ]
      plain_text <- ""
      last_end <- 0

      for (i in 1:nrow(sorted_annotations)) {
        if (sorted_annotations$start[i] > last_end + 1) {
          plain_text <- paste0(plain_text, substr(rv$text, last_end + 1, sorted_annotations$start[i] - 1))
        }
        plain_text <- paste0(plain_text,
                             "[", sorted_annotations$code[i], ": ",
                             substr(rv$text, sorted_annotations$start[i], sorted_annotations$end[i]),
                             "]")
        last_end <- sorted_annotations$end[i]
      }

      if (last_end < nchar(rv$text)) {
        plain_text <- paste0(plain_text, substr(rv$text, last_end + 1, nchar(rv$text)))
      }

      return(plain_text)
    }

    # Modify the text display output
    output$text_display <- renderUI({
      HTML(update_text_display())
    })

    # Create the floating text content once when the text changes
    observe({
      chars <- strsplit(rv$text, "")[[1]]
      rv$floating_text_content <- paste0("<span class='char' id='float_char_", seq_along(chars), "'>", chars, "</span>", collapse = "")
    })

    # Update the floating text display
    output$floating_text_display <- renderUI({
      HTML(update_text_display())
    })

    # Toggle floating text window
    observeEvent(input$toggle_text_window, {
      toggle("floating_text_window")
      runjs("initializeSelection();")
    })



    # Save project confirmation handler
    observeEvent(input$confirm_save_project, {
      req(input$project_name)
      req(input$directory_select)

      # Get selected directory path
      dir_path <- parseDirPath(roots, input$directory_select)

      # Construct full filepath
      filename <- if (!grepl("\\.rds$", input$project_name)) {
        paste0(input$project_name, ".rds")
      } else {
        input$project_name
      }
      filepath <- file.path(dir_path, filename)

      # Create project state
      project_state <- list(
        text = rv$text,
        annotations = rv$annotations,
        codes = rv$codes,
        code_tree = rv$code_tree,
        code_colors = rv$code_colors,
        memos = rv$memos,
        code_descriptions = rv$code_descriptions,
        history = rv$history,
        history_index = rv$history_index
      )

      # Save project
      tryCatch({
        dir.create(dirname(filepath), recursive = TRUE, showWarnings = FALSE)
        saveRDS(project_state, file = filepath)
        rv$current_project <- input$project_name
        rv$project_modified <- FALSE
        showNotification(paste("Project saved successfully to", filepath), type = "message")
      }, error = function(e) {
        showNotification(paste("Error saving project:", e$message), type = "error")
      })

      removeModal()
    })


    # Load Project handler
    observeEvent(input$load_project, {
      load_project_interactive(rv, input, session, roots)
    })

    # Display selected load file
    output$selected_file <- renderText({
      if (!is.null(input$file_select)) {
        selected <- parseFilePaths(roots, input$file_select)
        if (nrow(selected) > 0) {
          as.character(selected$datapath)
        }
      }
    })

    # Load project confirmation handler
    observeEvent(input$confirm_load_project, {
      req(input$file_select)

      # Get selected file path
      selected <- parseFilePaths(roots, input$file_select)
      if (nrow(selected) == 0) return()
      filepath <- as.character(selected$datapath[1])

      tryCatch({
        project_state <- readRDS(filepath)

        # Update all reactive values with loaded state
        rv$text <- project_state$text
        rv$annotations <- project_state$annotations
        rv$codes <- project_state$codes
        rv$code_tree <- project_state$code_tree
        rv$code_colors <- project_state$code_colors
        rv$memos <- project_state$memos
        rv$code_descriptions <- project_state$code_descriptions
        rv$history <- project_state$history
        rv$history_index <- project_state$history_index
        rv$current_project <- basename(filepath)
        rv$project_modified <- FALSE

        # Update UI elements
        updateTextAreaInput(session, "text_input", value = rv$text)
        session$sendCustomMessage("clearSelection", list())

        showNotification("Project loaded successfully", type = "message")
      }, error = function(e) {
        showNotification(paste("Error loading project:", e$message), type = "error")
      })

      removeModal()
    })

    observeEvent(input$load_without_saving, {
      removeModal()
      load_selected_project()
    })

    load_selected_project <- function() {
      project_state <- load_project_state(paste0(input$project_to_load, ".rds"))
      if (!is.null(project_state)) {
        # Update all reactive values with loaded state
        rv$text <- project_state$text
        rv$annotations <- project_state$annotations
        rv$codes <- project_state$codes
        rv$code_tree <- project_state$code_tree
        rv$code_colors <- project_state$code_colors
        rv$memos <- project_state$memos
        rv$code_descriptions <- project_state$code_descriptions
        rv$history <- project_state$history
        rv$history_index <- project_state$history_index
        rv$current_project <- input$project_to_load
        rv$project_modified <- FALSE

        # Update UI elements
        updateTextAreaInput(session, "text_input", value = rv$text)
        session$sendCustomMessage("clearSelection", list())
      }
      removeModal()
    }

    # Add modified state tracking
    observe({
      rv$project_modified <- TRUE
    }, priority = 1000) # High priority to catch all changes

    # Add New Project functionality
    observeEvent(input$new_project, {
      if (rv$project_modified) {
        showModal(modalDialog(
          title = "Save Current Project?",
          "Would you like to save the current project before creating a new one?",
          footer = tagList(
            actionButton("save_before_new", "Save First"),
            actionButton("new_without_saving", "Don't Save"),
            modalButton("Cancel")
          )
        ))
      } else {
        confirm_new_project()
      }
    })

    confirm_new_project <- function() {
      showModal(modalDialog(
        title = "Confirm New Project",
        "Are you sure you want to create a new project? This will clear all current work.",
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_new_project", "Create New Project")
        )
      ))
    }

    # Handle new project confirmation
    observeEvent(input$confirm_new_project, {
      removeModal()
      create_new_project(rv, session)
    })

    observeEvent(input$save_before_new, {
      removeModal()
      showModal(modalDialog(
        title = "Save Project",
        textInput("project_name", "Project Name:",
                  value = rv$current_project %||% ""),
        selectInput("save_location", "Save Location:",
                    choices = c("Default Location" = "default",
                                "Custom Location" = "custom")),
        conditionalPanel(
          condition = "input.save_location == 'custom'",
          textInput("custom_save_path", "Custom Save Path:",
                    value = getwd())
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_save_before_new", "Save")
        )
      ))
    })

    observeEvent(input$confirm_save_before_new, {
      req(input$project_name)

      # Save current project
      if (input$save_location == "default") {
        save_path <- get_project_dir()
      } else {
        save_path <- input$custom_save_path
        if (!dir.exists(save_path)) {
          dir.create(save_path, recursive = TRUE)
        }
      }

      filename <- if (!grepl("\\.rds$", input$project_name)) {
        paste0(input$project_name, ".rds")
      } else {
        input$project_name
      }

      filepath <- file.path(save_path, filename)

      project_state <- list(
        text = rv$text,
        annotations = rv$annotations,
        codes = rv$codes,
        code_tree = rv$code_tree,
        code_colors = rv$code_colors,
        memos = rv$memos,
        code_descriptions = rv$code_descriptions,
        history = rv$history,
        history_index = rv$history_index
      )

      handle_error(
        expr = {
          saveRDS(project_state, file = filepath)
          showNotification(paste("Project saved successfully to", filepath),
                           type = "message")
          # Proceed to create new project after successful save
          removeModal()
          create_new_project(rv, session)
        },
        error_msg = paste("Failed to save project to", filepath)
      )
    })

    observeEvent(input$new_without_saving, {
      removeModal()
      create_new_project(rv, session)
    })

    # Helper function to create new project
    create_new_project <- function(rv, session) {
      rv$text <- ""
      rv$annotations <- data.frame(
        start = integer(),
        end = integer(),
        text = character(),
        code = character(),
        memo = character(),
        stringsAsFactors = FALSE
      )
      rv$codes <- character()
      rv$code_tree <- Node$new("Root")
      rv$code_colors <- character()
      rv$memos <- list()
      rv$code_descriptions <- list()
      rv$history <- list(list(text = "", annotations = data.frame()))
      rv$history_index <- 1
      rv$current_project <- NULL
      rv$project_modified <- FALSE
      rv$action_history <- list()
      rv$action_index <- 0
      rv$merged_codes <- list()

      # Clear UI elements
      updateTextAreaInput(session, "text_input", value = "")
      session$sendCustomMessage("clearSelection", list())

      showNotification("New project created", type = "message")
    }

    # Enhance error handling for file operations
    observeEvent(input$import_text, {
      req(input$file_input)

      handle_error(
        expr = {
          imported_text <- readtext(input$file_input$datapath)
          rv$text <- imported_text$text
          rv$history <- c(rv$history[1:rv$history_index],
                          list(list(text = rv$text, annotations = rv$annotations)))
          rv$history_index <- rv$history_index + 1
        },
        success_msg = "Text imported successfully",
        error_msg = "Failed to import text file"
      )
    })

    # Save Records handler
    observeEvent(input$save_records, {
      save_records_interactive(rv, input, session, roots)
    })

    # Save records confirmation handler
    observeEvent(input$confirm_save_records, {
      req(input$save_filename)
      req(input$records_directory_select)

      # Get selected directory path
      dir_path <- parseDirPath(roots, input$records_directory_select)

      # Construct full filepath
      filename <- input$save_filename
      if (!grepl(paste0("\\.", input$save_format, "$"), filename)) {
        filename <- paste0(filename, ".", input$save_format)
      }
      filepath <- file.path(dir_path, filename)

      # Save records
      tryCatch({
        dir.create(dirname(filepath), recursive = TRUE, showWarnings = FALSE)

        if (input$save_format == "csv") {
          # Save as CSV
          write.csv(rv$annotations, file = filepath, row.names = FALSE)
        } else {
          # Save as JSON
          write_json(rv$annotations, filepath)
        }

        showNotification(paste("Records saved successfully to", filepath), type = "message")
      }, error = function(e) {
        showNotification(paste("Error saving records:", e$message), type = "error")
      })

      removeModal()
    })

    # Undo functionality
    observeEvent(input$undo, {
      if (rv$action_index > 0) {
        action <- rv$action_history[[rv$action_index]]
        apply_action(rv, action, reverse = TRUE)
        rv$action_index <- rv$action_index - 1

        # Update UI
        output$text_display <- renderUI({
          HTML(update_text_display())
        })
      }
    })

    # Redo functionality
    observeEvent(input$redo, {
      if (rv$action_index < length(rv$action_history)) {
        rv$action_index <- rv$action_index + 1
        action <- rv$action_history[[rv$action_index]]
        apply_action(rv, action, reverse = FALSE)

        # Update UI
        output$text_display <- renderUI({
          HTML(update_text_display())
        })
      }
    })

    # Helper function to save state
    save_state <- function() {
      current_state <- list(
        text = rv$text,
        annotations = rv$annotations,
        codes = rv$codes,
        code_tree = rv$code_tree,
        code_colors = rv$code_colors,
        memos = rv$memos,
        code_descriptions = rv$code_descriptions
      )

      # Remove any future states if we're not at the end of the history
      if (rv$history_index < length(rv$history)) {
        rv$history <- rv$history[1:rv$history_index]
      }

      rv$history <- c(rv$history, list(current_state))
      rv$history_index <- length(rv$history)
    }

    # Modify the JavaScript for text selection
    observeEvent(input$select, {
      runjs("
            var startChar = null;
            var endChar = null;
            var isSelecting = false;
            var activeWindow = 'main'; // Track which window is active for selection

            function initializeSelection() {
              // Remove existing event listeners
              $('#text_display, #floating_text_content').off('mousedown mousemove');
              $(document).off('mouseup');

              // Add mousedown event listener for character selection
              $('#text_display, #floating_text_content').on('mousedown', '.char', function(e) {
                e.preventDefault();
                if (!isSelecting) return;

                startChar = endChar = parseInt($(this).attr('id').replace('char_', '').replace('float_char_', ''));
                updateHighlight();
              });

              // Add mousemove event listener for selection dragging
              $('#text_display, #floating_text_content').on('mousemove', '.char', function(e) {
                if (e.buttons === 1 && isSelecting) {
                  endChar = parseInt($(this).attr('id').replace('char_', '').replace('float_char_', ''));
                  updateHighlight();
                }
              });

              // Add mouseup event listener to finalize selection
              $(document).on('mouseup', function() {
                if (isSelecting && startChar !== null && endChar !== null) {
                  updateHighlight();
                  updateShiny();
                  isSelecting = false;
                }
              });
            }

            function updateHighlight() {
              if (startChar === null || endChar === null) return;

              // Clear all highlights first
              $('.char').removeClass('highlighted');

              var start = Math.min(startChar, endChar);
              var end = Math.max(startChar, endChar);

              // Update highlights in both windows
              for (var i = start; i <= end; i++) {
                $('#char_' + i + ', #float_char_' + i).addClass('highlighted');
              }
            }

            function updateShiny() {
              var selectedText = $('.highlighted').text();
              Shiny.setInputValue('selected_text', {
                text: selectedText,
                start: Math.min(startChar, endChar),
                end: Math.max(startChar, endChar)
              });
            }

            // Add event listener for floating window toggle
            $('#floating_text_window').on('show hide', function() {
              setTimeout(initializeSelection, 100); // Short delay to ensure DOM is updated
            });

            Shiny.addCustomMessageHandler('startSelecting', function(message) {
              isSelecting = true;
              initializeSelection();
            });

            Shiny.addCustomMessageHandler('clearSelection', function(message) {
              $('.char').removeClass('highlighted');
              startChar = endChar = null;
              isSelecting = false;
              Shiny.setInputValue('selected_text', null);
            });

            // Initialize selection handlers when page loads
            $(document).ready(function() {
              initializeSelection();

              // Add toggle button handler
              $('#toggle_text_window').click(function() {
                setTimeout(initializeSelection, 100);
              });
            });
      ")
      session$sendCustomMessage("startSelecting", list())
    })

    # Handle text selection
    observeEvent(input$selected_text, {
      if (!is.null(input$selected_text)) {
        rv$selected_start <- input$selected_text$start
        rv$selected_end <- input$selected_text$end
      }
    })

    # Clear button
    observeEvent(input$clear, {
      rv$selected_start <- NULL
      rv$selected_end <- NULL
      updateTextInput(session, "code", value = "")
      updateTextAreaInput(session, "memo", value = "")
      session$sendCustomMessage("clearSelection", list())
    })

    # Update the save function
    observeEvent(input$save, {
      if (!is.null(rv$selected_start) && !is.null(rv$selected_end)) {
        new_annotation <- data.frame(
          start = rv$selected_start,
          end = rv$selected_end,
          text = substr(rv$text, rv$selected_start, rv$selected_end),
          code = input$code,
          memo = input$memo,
          stringsAsFactors = FALSE
        )

        # Create and apply the action
        add_action <- create_action(
          type = "add_annotation",
          data = new_annotation,
          reverse_data = new_annotation  # Same data used for reverse action
        )

        apply_action(rv, add_action)
        add_action(rv, add_action)

        # Update codes list
        rv$codes <- unique(c(rv$codes, input$code))

        # Assign color if needed
        if (!(input$code %in% names(rv$code_colors))) {
          rv$code_colors[input$code] <- sprintf("#%06X", sample(0:16777215, 1))
        }

        # Clear inputs
        updateTextInput(session, "code", value = "")
        updateTextAreaInput(session, "memo", value = "")
        rv$selected_start <- NULL
        rv$selected_end <- NULL
        session$sendCustomMessage("clearSelection", list())

        # Update UI
        output$text_display <- renderUI({
          HTML(update_text_display())
        })
      }
    })

    # Apply Code button
    observeEvent(input$apply_code, {
      if (!is.null(rv$selected_start) && !is.null(rv$selected_end)) {
        showModal(modalDialog(
          title = "Apply Code",
          selectInput("code_to_apply", "Select a code to apply:", choices = rv$codes),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("confirm_apply_code", "Apply")
          )
        ))
      } else {
        showNotification("Please select text before applying a code.", type = "warning")
      }
    })

    # Confirm Apply Code
    observeEvent(input$confirm_apply_code, {
      removeModal()
      if (!is.null(rv$selected_start) && !is.null(rv$selected_end)) {
        new_annotation <- data.frame(
          start = rv$selected_start,
          end = rv$selected_end,
          text = substr(rv$text, rv$selected_start, rv$selected_end),
          code = input$code_to_apply,
          memo = "",
          stringsAsFactors = FALSE
        )
        rv$annotations <- rbind(rv$annotations, new_annotation)
        updateTextInput(session, "code", value = input$code_to_apply)
        showNotification("Code applied successfully!", type = "message")
        save_state()
      }
    })

    # Display Code button
    observeEvent(input$display_code, {
      sorted_annotations <- rv$annotations[order(rv$annotations$start), ]
      displayed_text <- ""
      last_end <- 0

      for (i in 1:nrow(sorted_annotations)) {
        if (sorted_annotations$start[i] > last_end + 1) {
          displayed_text <- paste0(displayed_text,
                                   substr(rv$text, last_end + 1, sorted_annotations$start[i] - 1))
        }
        displayed_text <- paste0(displayed_text,
                                 "<span class='code-display'>[", sorted_annotations$code[i], "]</span>",
                                 substr(rv$text, sorted_annotations$start[i], sorted_annotations$end[i]))
        last_end <- sorted_annotations$end[i]
      }

      if (last_end < nchar(rv$text)) {
        displayed_text <- paste0(displayed_text, substr(rv$text, last_end + 1, nchar(rv$text)))
      }

      showModal(modalDialog(
        title = "Coded Text Display",
        tags$div(style = "white-space: pre-wrap; line-height: 1.5;", HTML(displayed_text)),
        size = "l",
        easyClose = TRUE
      ))
    })

    # Export Codes
    observeEvent(input$export_codes, {
      codes_df <- data.frame(
        code = get_code_names(rv$code_tree),
        path = get_code_paths(rv$code_tree),
        color = rv$code_colors[get_code_names(rv$code_tree)]
      )
      write.csv(codes_df, file = "exported_codes.csv", row.names = FALSE)
      showNotification("Codes exported successfully", type = "message")
    })

    # Helper function to get code paths
    get_code_paths <- function(node) {
      if (node$isRoot) {
        return(character(0))
      } else {
        return(c(paste(node$path, collapse = "/"), unlist(lapply(node$children, get_code_paths))))
      }
    }

    # Update the display of annotations in the Records tab
    output$annotations <- renderDT({
      datatable(rv$annotations, options = list(pageLength = 5))
    })

    # Import Annotations
    observeEvent(input$import_annotations, {
      showModal(modalDialog(
        title = "Import Annotations",
        fileInput("annotation_file", "Choose JSON File", accept = c("application/json", ".json")),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_import_annotations", "Import")
        )
      ))
    })

    observeEvent(input$confirm_import_annotations, {
      req(input$annotation_file)
      imported_annotations <- fromJSON(input$annotation_file$datapath)
      rv$annotations <- rbind(rv$annotations, imported_annotations)
      removeModal()
      save_state()
    })

    # Export Annotations
    observeEvent(input$export_annotations, {
      write_json(rv$annotations, "exported_annotations.json")
      showNotification("Annotations exported successfully", type = "message")
    })

    # Event handler for Code Frequency button
    observeEvent(input$code_frequency, {
      req(nrow(rv$annotations) > 0)
      plot <- generate_code_frequency_plot(rv$annotations)
      output$code_freq_plot <- renderPlot({ plot })
      showModal(modalDialog(
        title = "Code Frequency",
        plotOutput("code_freq_plot"),
        size = "l",
        easyClose = TRUE
      ))
    })

    # Event handler for Code Co-occurrence button
    observeEvent(input$code_co_occurrence, {
      req(nrow(rv$annotations) > 0)

      # Generate the enhanced analysis
      analysis_results <- generate_code_co_occurrence_analysis(rv$annotations)

      # Store results in reactive values for access across multiple outputs
      rv$co_occurrence_results <- analysis_results

      # Create the modal dialog with multiple tabs
      showModal(modalDialog(
        title = "Code Co-occurrence Analysis",

        tabsetPanel(
          id = "co_occurrence_tabs",

          # Network visualization tab
          tabPanel("Network View",
                   plotOutput("code_co_occurrence_network", height = "500px"),
                   hr(),
                   helpText("Line thickness indicates Jaccard similarity strength",
                            "Line opacity shows phi coefficient magnitude")),

          # Heatmap visualization tab
          tabPanel("Heatmap View",
                   plotOutput("code_co_occurrence_heatmap", height = "500px"),
                   hr(),
                   helpText("Darker colors indicate stronger co-occurrence relationships")),

          # Statistics tab
          tabPanel("Statistics",
                   h4("Summary Statistics"),
                   tableOutput("co_occurrence_summary"),
                   hr(),
                   h4("Detailed Co-occurrence Matrix"),
                   DTOutput("co_occurrence_table"))
        ),

        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })

    output$code_co_occurrence_network <- renderPlot({
      req(rv$co_occurrence_results)
      print(rv$co_occurrence_results$network_plot)
    })

    output$code_co_occurrence_heatmap <- renderPlot({
      req(rv$co_occurrence_results)
      print(rv$co_occurrence_results$heatmap_plot)
    })

    output$co_occurrence_summary <- renderTable({
      req(rv$co_occurrence_results)
      summary_df <- data.frame(
        Metric = c("Total Number of Codes",
                   "Maximum Co-occurrence Count",
                   "Maximum Jaccard Similarity",
                   "Mean Jaccard Similarity",
                   "Number of Significant Pairs (|\u03c6| > 0.3)"),
        Value = c(rv$co_occurrence_results$summary$total_codes,
                  rv$co_occurrence_results$summary$max_co_occurrence,
                  round(rv$co_occurrence_results$summary$max_jaccard, 3),
                  round(rv$co_occurrence_results$summary$mean_jaccard, 3),
                  rv$co_occurrence_results$summary$significant_pairs)
      )
      summary_df
    })

    output$co_occurrence_table <- renderDT({
      req(rv$co_occurrence_results)
      # Convert matrices to data frames for display
      co_df <- as.data.frame(rv$co_occurrence_results$co_occurrence)
      jaccard_df <- as.data.frame(round(rv$co_occurrence_results$jaccard_similarity, 3))
      phi_df <- as.data.frame(round(rv$co_occurrence_results$phi_coefficient, 3))

      # Add row names as a column
      co_df$Code <- rownames(co_df)
      jaccard_df$Code <- rownames(jaccard_df)
      phi_df$Code <- rownames(phi_df)

      # Move Code column to front
      co_df <- co_df[, c(ncol(co_df), 1:(ncol(co_df)-1))]

      datatable(co_df,
                options = list(
                  pageLength = 10,
                  scrollX = TRUE,
                  dom = 'Bfrtip',
                  buttons = c('copy', 'csv', 'excel')
                ),
                caption = "Raw Co-occurrence Counts") %>%
        DT::formatStyle(names(co_df)[-1],
                    background = styleInterval(c(0, 2, 5, 10),
                                               c('white', '#f7fbff', '#deebf7', '#9ecae1', '#3182bd')))
    })

    # Word Cloud button
    observeEvent(input$word_cloud, {
      word_cloud <- generate_word_cloud(rv$text)
      output$word_cloud_plot <- renderPlot({ word_cloud })
      showModal(modalDialog(
        title = "Word Cloud",
        plotOutput("word_cloud_plot", height = "500px"),
        size = "l",
        easyClose = TRUE
      ))
    })

    # Text Summary button
    observeEvent(input$text_summary, {
      summary <- generate_text_summary(rv$text, rv$annotations)
      output$text_summary_table <- renderTable({
        data.frame(Metric = names(summary), Value = unlist(summary))
      })
      showModal(modalDialog(
        title = "Text Summary",
        tableOutput("text_summary_table"),
        size = "m",
        easyClose = TRUE
      ))
    })

    # Memo Linking
    #observeEvent(input$link_memo, {
    #  showModal(modalDialog(
    #    title = "Link Memo",
    #    selectInput("memo_link_type", "Link to:",
    #                choices = c("Code", "Document")),
    #    uiOutput("memo_link_options"),
    #    textAreaInput("memo_text", "Memo:"),
    #    footer = tagList(
    #      modalButton("Cancel"),
    #      actionButton("confirm_link_memo", "Link")
    #    )
    # ))
    #})

    output$memo_link_options <- renderUI({
      if (input$memo_link_type == "Code") {
        selectInput("memo_link_code", "Select Code:",
                    choices = rv$codes)  # Use rv$codes instead of get_code_names(rv$code_tree)
      } else {
        # For document-level memo, no additional input is needed
        NULL
      }
    })

    # Update the Link Memo functionality
    #observeEvent(input$confirm_link_memo, {
    #  new_memo <- list(
    #    type = input$memo_link_type,
    #    link = if (input$memo_link_type == "Code") input$memo_link_code else "Document",
    #    text = input$memo_text
    #  )

    #  if (new_memo$type == "Code") {
    #    # Find all annotations with the selected code and update their memos
    #    code_indices <- which(rv$annotations$code == new_memo$link)
    #    if (length(code_indices) > 0) {
    #      rv$annotations$memo[code_indices] <- sapply(rv$annotations$memo[code_indices],
    #                                                  function(memo) concatenate_memos(memo, new_memo$text))
    #    }
    #  } else {
    #    # For document-level memos, we'll add them to all annotations
    #    rv$annotations$memo <- sapply(rv$annotations$memo,
    #                                  function(memo) concatenate_memos(memo, new_memo$text))
    #  }

    #  rv$memos <- c(rv$memos, list(new_memo))
    #  removeModal()
    #  save_state()
    #})

    # Helper function to concatenate memos without extra semicolons
    concatenate_memos <- function(existing_memo, new_memo) {
      if (existing_memo == "") {
        return(new_memo)
      } else {
        return(paste(existing_memo, new_memo, sep = "; "))
      }
    }

    # Code Book Generation
    #observeEvent(input$generate_codebook, {
    #  codebook <- lapply(get_code_names(rv$code_tree), function(code) {
    #    code_annotations <- rv$annotations[rv$annotations$code == code, ]
    #   list(
    #      code = code,
    #      description = rv$code_descriptions[[code]] %||% "",
    #      example_quotes = if (nrow(code_annotations) > 0) {
    #        sample(code_annotations$text, min(3, nrow(code_annotations)))
    #      } else {
    #        character(0)
    #      }
    #    )
    #  })

    #  # Generate Markdown for the codebook
    #  codebook_md <- sapply(codebook, function(entry) {
    #    paste0(
    #      "## ", entry$code, "\n\n",
    #      "**Description:** ", entry$description, "\n\n",
    #      "**Example Quotes:**\n",
    #      paste0("- ", entry$example_quotes, collapse = "\n"), "\n\n"
    #    )
    #  })

    #  # Write the codebook to a file
    #  writeLines(c("# Code Book\n\n", codebook_md), "codebook.md")
    # showNotification("Code book generated successfully", type = "message")
    #})

    # Handle file uploads for comparison
    observeEvent(input$comparison_file, {
      req(input$comparison_file)

      # Load all uploaded files
      comparison_data <- lapply(input$comparison_file$datapath, function(path) {
        tryCatch({
          ext <- tolower(tools::file_ext(path))

          if (ext == "csv") {
            # Handle CSV files
            df <- read.csv(path, stringsAsFactors = FALSE)
          } else if (ext == "json") {
            # Handle JSON files
            df <- fromJSON(path)
          } else {
            showNotification(paste("Unsupported file format:", ext), type = "error")
            return(NULL)
          }

          # Ensure proper column types and structure
          if (is.data.frame(df)) {
            # Check required columns
            required_cols <- c("start", "end", "code")
            if (!all(required_cols %in% colnames(df))) {
              missing_cols <- setdiff(required_cols, colnames(df))
              showNotification(paste("Missing required columns:",
                                     paste(missing_cols, collapse = ", ")),
                               type = "error")
              return(NULL)
            }

            # Convert columns to proper types
            df$start <- as.numeric(as.character(df$start))
            df$end <- as.numeric(as.character(df$end))
            df$code <- as.character(df$code)

            # Remove rows with NA values in required columns
            df <- df[complete.cases(df[, required_cols]), ]

            # Ensure data frame has at least one row
            if (nrow(df) == 0) {
              showNotification("No valid annotations found after cleaning", type = "warning")
              return(NULL)
            }

            return(df)
          }
          return(NULL)

        }, error = function(e) {
          showNotification(paste("Error loading file:", e$message), type = "error")
          return(NULL)
        })
      })

      # Remove any NULL entries from failed loads
      comparison_data <- comparison_data[!sapply(comparison_data, is.null)]

      if (length(comparison_data) >= 2) {
        rv$comparison_data <- comparison_data
        showNotification("Comparison data loaded successfully", type = "message")
      } else {
        showNotification("Need at least two valid annotation sets for comparison",
                         type = "warning")
      }
    })

    observeEvent(input$comparison_file1, {
      req(input$comparison_file1)

      # Process the first file
      df1 <- tryCatch({
        process_comparison_file(input$comparison_file1$datapath)
      }, error = function(e) {
        showNotification(paste("Error processing first file:", e$message), type = "error")
        return(NULL)
      })

      if (!is.null(df1)) {
        rv$comparison_file1 <- df1
        showNotification("First file loaded successfully", type = "message")
      }
    })

    observeEvent(input$comparison_file2, {
      req(input$comparison_file2)

      # Process the second file
      df2 <- tryCatch({
        process_comparison_file(input$comparison_file2$datapath)
      }, error = function(e) {
        showNotification(paste("Error processing second file:", e$message), type = "error")
        return(NULL)
      })

      if (!is.null(df2)) {
        rv$comparison_file2 <- df2
        showNotification("Second file loaded successfully", type = "message")
      }
    })

    # Helper function to process comparison files
    process_comparison_file <- function(filepath) {
      ext <- tolower(tools::file_ext(filepath))

      df <- if (ext == "csv") {
        read.csv(filepath, stringsAsFactors = FALSE)
      } else if (ext == "json") {
        fromJSON(filepath)
      } else {
        stop("Unsupported file format")
      }

      # If the file is from Records tab (has text and memo columns), reformat it
      if (all(c("start", "end", "text", "code", "memo") %in% colnames(df))) {
        df <- df[, c("start", "end", "code")]
      }

      # Check required columns
      required_cols <- c("start", "end", "code")
      if (!all(required_cols %in% colnames(df))) {
        missing_cols <- setdiff(required_cols, colnames(df))
        stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
      }

      # Convert columns to proper types
      df$start <- as.numeric(as.character(df$start))
      df$end <- as.numeric(as.character(df$end))
      df$code <- as.character(df$code)

      # Remove rows with NA values
      df <- df[complete.cases(df[, required_cols]), ]

      if (nrow(df) == 0) {
        stop("No valid annotations found after cleaning")
      }

      return(df)
    }

    # Add file info displays
    output$file1_info <- renderUI({
      if (!is.null(rv$comparison_file1)) {
        div(
          style = "margin: 10px 0; padding: 10px; background-color: #e8f4e8; border-radius: 5px;",
          tags$p(
            icon("check-circle", class = "text-success"),
            strong("File 1 loaded:"),
            br(),
            paste("Number of annotations:", nrow(rv$comparison_file1))
          )
        )
      }
    })

    output$file2_info <- renderUI({
      if (!is.null(rv$comparison_file2)) {
        div(
          style = "margin: 10px 0; padding: 10px; background-color: #e8f4e8; border-radius: 5px;",
          tags$p(
            icon("check-circle", class = "text-success"),
            strong("File 2 loaded:"),
            br(),
            paste("Number of annotations:", nrow(rv$comparison_file2))
          )
        )
      }
    })

    # Add reset functionality
    observeEvent(input$reset_comparison, {
      rv$comparison_file1 <- NULL
      rv$comparison_file2 <- NULL
      rv$comparison_data <- NULL
      rv$comparison_results <- NULL

      # Reset file inputs (this requires a bit of JavaScript)
      runjs("
    document.getElementById('comparison_file1').value = '';
    document.getElementById('comparison_file2').value = '';
  ")

      showNotification("Comparison files have been reset", type = "message")
    })

    # Run comparison analysis
    observeEvent(input$run_comparison, {
      if (is.null(rv$comparison_file1) || is.null(rv$comparison_file2)) {
        showNotification("Please upload both files before running comparison",
                         type = "warning")
        return()
      }

      # Run comparison with the two files
      rv$comparison_data <- list(rv$comparison_file1, rv$comparison_file2)

      withProgress(message = 'Running comparison analysis...', {
        rv$comparison_results <- tryCatch({
          results <- generate_comparison_analysis(rv$comparison_data)
          plots <- generate_comparison_plots(results)
          c(results, list(plots = plots))
        }, error = function(e) {
          showNotification(paste("Error in comparison analysis:", e$message),
                           type = "error")
          NULL
        })
      })
    })

    observe({
      req(rv$comparison_results)

      tryCatch({
        # Update the UI elements based on comparison results
        output$comparison_summary <- renderText({
          req(rv$comparison_results)

          # Summarize key differences in coding approaches
          differences <- rv$comparison_results$pattern_comparison

          # Format coverage differences
          coverage_text <- format_coverage_differences(differences$coverage_differences)

          # Format code application differences
          code_text <- format_code_differences(differences$code_differences)

          # Format overlap pattern differences
          overlap_text <- format_overlap_differences(differences$combination_differences)

          # Format sequence differences
          sequence_text <- format_sequence_differences(differences$sequence_differences)

          # Combine all text
          paste0(
            "Qualitative Comparison Summary\n",
            "===========================\n\n",
            "Coverage Patterns:\n", coverage_text, "\n\n",
            "Code Application Patterns:\n", code_text, "\n\n",
            "Code Overlap Patterns:\n", overlap_text, "\n\n",
            "Sequence Patterns:\n", sequence_text
          )
        })

        output$comparison_plot <- renderPlot({
          req(input$plot_type)
          if (is.null(rv$comparison_results$plots[[input$plot_type]])) {
            plot(0, 0, type = "n",
                 main = "No data to display",
                 xlab = "", ylab = "")
          } else {
            print(rv$comparison_results$plots[[input$plot_type]])
          }
        })
      }, error = function(e) {
        showNotification(paste("Error updating comparison results:", e$message),
                         type = "error")
      })
    })

    # Render comparison summary
    output$comparison_summary <- renderText({
      req(rv$comparison_results)

      # Summarize key differences in coding approaches
      differences <- rv$comparison_results$pattern_comparison

      # Format coverage differences
      coverage_text <- format_coverage_differences(differences$coverage_differences)

      # Format code application differences
      code_text <- format_code_differences(differences$code_differences)

      # Format overlap pattern differences
      overlap_text <- format_overlap_differences(differences$combination_differences)

      # Format sequence differences
      sequence_text <- format_sequence_differences(differences$sequence_differences)

      # Combine all text
      paste0(
        "Qualitative Comparison Summary\n",
        "===========================\n\n",
        "Coverage Patterns:\n", coverage_text, "\n\n",
        "Code Application Patterns:\n", code_text, "\n\n",
        "Code Overlap Patterns:\n", overlap_text, "\n\n",
        "Sequence Patterns:\n", sequence_text
      )
    })

    # Render visualizations
    output$comparison_plot <- renderPlot({
      req(rv$comparison_results, input$plot_type)

      # Get the appropriate plot based on selected type
      plot_result <- NULL
      if (!is.null(rv$comparison_results$plots)) {
        plot_result <- switch(input$plot_type,
                              "distribution" = rv$comparison_results$plots$distribution,
                              "overlap" = rv$comparison_results$plots$overlap,
                              "sequence" = rv$comparison_results$plots$sequence)
      }

      # If no plot is available, show empty plot with message
      if (is.null(plot_result)) {
        plot.new()
        title(main = "No data available for selected visualization")
      } else {
        print(plot_result)
      }
    }, height = function() {
      # Dynamically adjust height based on number of coders
      if (!is.null(rv$comparison_results)) {
        n_coders <- length(rv$comparison_results$coding_strategies)
        return(200 * n_coders)  # 200 pixels per coder
      }
      return(400)  # Default height
    })

    # Update the detailed analysis output
    output$coverage_details <- renderText({
      req(rv$comparison_results)

      coverage <- rv$comparison_results$comparison$coverage_differences
      paste0(
        "Total Codes Range: ", paste(coverage$total_codes_range, collapse=" - "), "\n",
        "Unique Codes Range: ", paste(coverage$unique_codes_range, collapse=" - ")
      )
    })

    output$application_details <- renderText({
      req(rv$comparison_results)

      codes <- rv$comparison_results$comparison$code_differences
      paste0(
        "Shared Codes: ", paste(codes$shared_codes, collapse=", "), "\n",
        "Usage Patterns:\n",
        apply(codes$usage_matrix, 1, function(row) {
          paste0("  ", names(row)[1], ": ", paste(row, collapse=" vs "), "\n")
        })
      )
    })

    output$pattern_details <- renderText({
      req(rv$comparison_results)

      overlaps <- rv$comparison_results$comparison$overlap_differences
      paste0(
        "Total Overlaps Range: ", paste(overlaps$total_overlaps_range, collapse=" - "), "\n",
        "Unique Pairs Range: ", paste(overlaps$unique_pairs_range, collapse=" - ")
      )
    })

    # Update plot type choices
    observeEvent(input$comparison_metrics, {
      req(rv$comparison_results)

      # Update available plot types based on selected metrics
      plot_choices <- list(
        "Code Distribution" = "distribution",
        "Code Overlaps" = "overlap",
        "Code Sequences" = "sequence"
      )

      updateSelectInput(session, "plot_type",
                        choices = plot_choices)
    })

  }

  runApp(shinyApp(ui, server))
}

#' @importFrom utils write.csv packageVersion
#' @importFrom stats runif
#' @importFrom grDevices rgb rainbow recordPlot
#' @importFrom graphics par barplot plot points text lines
#' @importFrom shiny runApp shinyApp fluidPage actionButton observeEvent renderUI
#'   showNotification showModal modalDialog removeModal updateTextAreaInput
#'   updateTextInput tabPanel fileInput renderTable renderPlot plotOutput
#'   tableOutput textInput textAreaInput selectInput checkboxGroupInput
#'   tags icon reactive reactiveValues isolate req
#' @importFrom shinyjs useShinyjs toggle runjs
#' @importFrom data.tree Node as.Node
#' @importFrom jsonlite fromJSON toJSON write_json
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar
#'   dashboardBody tabBox box
#' @importFrom DT renderDT datatable DTOutput
#' @importFrom readtext readtext
#' @importFrom tools R_user_dir
#' @importFrom shinyFiles shinyFileChoose shinyFilesButton shinyDirButton shinyDirChoose parseDirPath parseFilePaths
NULL

#' \%||\% operator
#'
#' @name grapes-or-or-grapes
#' @aliases %||%
#' @title Null coalescing operator
#' @description Provides null coalescing functionality, returning the first non-NULL argument
#' @param a First value to check
#' @param b Second value (default) to use if first is NULL
#' @return Returns \code{a} if not NULL, otherwise returns \code{b}
#' @keywords internal
#' @examples
#' \dontrun{
#' x <- NULL
#' y <- 5
#' x %||% y  # Returns 5
#'
#' x <- 10
#' x %||% y  # Returns 10
#' }
`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Handle errors with custom messages
#'
#' @description
#' Provides error handling with customizable success, error, and completion messages.
#' Wraps expressions in a tryCatch block and displays appropriate notifications.
#'
#' @param expr Expression to evaluate
#' @param success_msg Optional character string for success notification
#' @param error_msg Optional character string for error notification
#' @param finally_msg Optional character string for completion notification
#'
#' @return Result of the expression or NULL if error occurs
#'
#' @importFrom shiny showNotification
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' result <- handle_error(
#'   expr = { 1 + 1 },
#'   success_msg = "Calculation completed"
#' )
#'
#' # With error handling
#' result <- handle_error(
#'   expr = { 1 / 0 },
#'   error_msg = "Division by zero detected",
#'   finally_msg = "Operation finished"
#' )
#' }
#'
#' @keywords internal
handle_error <- function(expr, success_msg = NULL, error_msg = NULL, finally_msg = NULL) {
  tryCatch({
    result <- expr
    if (!is.null(success_msg)) {
      showNotification(success_msg, type = "message")
    }
    return(result)
  }, error = function(e) {
    msg <- if (is.null(error_msg)) paste("Error:", e$message) else error_msg
    showNotification(msg, type = "error")
    return(NULL)
  }, finally = {
    if (!is.null(finally_msg)) {
      showNotification(finally_msg, type = "message")
    }
  })
}

#' Display interactive project save dialog
#'
#' @description
#' Shows modal dialog for saving project with directory selection and
#' project name input.
#'
#' @param rv ReactiveValues object containing project state
#' @param input Shiny input values
#' @param session Shiny session object
#' @param volumes List of available storage volumes
#'
#' @return Invisible NULL, called for side effect
#'
#' @examples
#' \dontrun{
#' # In Shiny server function:
#' observeEvent(input$save_button, {
#'   save_project_interactive(rv, input, session, volumes)
#' })
#' }
#' @keywords internal
save_project_interactive <- function(rv, input, session, volumes) {
  showModal(modalDialog(
    title = "Save Project",
    textInput("project_name", "Project Name:",
              value = rv$current_project %||% ""),
    # Use regular file input instead of shinyFilesButton
    div(style = "margin: 10px 0;",
        shinyDirButton("directory_select",
                       label = "Choose Directory",
                       title = "Select Directory to Save Project")
    ),
    verbatimTextOutput("selected_dir"),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("confirm_save_project", "Save")
    )
  ))
}

# Interactive annotated text save dialog
save_annotated_text_interactive <- function(rv, input, session, volumes) {
  showModal(modalDialog(
    title = "Save Annotated Text",
    textInput("save_filename", "Enter filename:"),
    selectInput("save_format", "Select file format:",
                choices = c("HTML" = "html", "Text File" = "txt")),
    div(style = "margin: 10px 0;",
        shinyDirButton("text_directory_select",
                       label = "Choose Directory",
                       title = "Select Directory to Save Text")
    ),
    verbatimTextOutput("selected_text_dir"),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("confirm_save_annotations", "Save")
    )
  ))
}

# Interactive records save dialog
save_records_interactive <- function(rv, input, session, volumes) {
  showModal(modalDialog(
    title = "Save Records",
    textInput("save_filename", "Enter filename:"),
    selectInput("save_format", "Select file format:",
                choices = c("CSV" = "csv", "JSON" = "json")),
    div(style = "margin: 10px 0;",
        shinyDirButton("records_directory_select",
                       label = "Choose Directory",
                       title = "Select Directory to Save Records")
    ),
    verbatimTextOutput("selected_records_dir"),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("confirm_save_records", "Save")
    )
  ))
}

#' Display interactive project load dialog
#'
#' @description
#' Shows modal dialog for loading project with file selection functionality.
#'
#' @param rv ReactiveValues object for project state
#' @param input Shiny input values
#' @param session Shiny session object
#' @param roots List of root directories for file selection
#'
#' @return Invisible NULL, called for side effect
#'
#' @examples
#' \dontrun{
#' # In Shiny server function:
#' observeEvent(input$load_button, {
#'   load_project_interactive(rv, input, session, roots)
#' })
#' }
#' @keywords internal
load_project_interactive <- function(rv, input, session, roots) {
  showModal(modalDialog(
    title = "Load Project",
    div(style = "margin: 10px 0;",
        shinyFilesButton("file_select",
                         label = "Choose Project File",
                         title = "Select Project File",
                         multiple = FALSE)
    ),
    tags$p("Selected file:"),
    verbatimTextOutput("selected_file"),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("confirm_load_project", "Load")
    )
  ))
}

# Helper function to get available volumes on Windows
getVolumes <- function() {
  function() {
    volumes <- c("C:" = "C:/")
    for (letter in LETTERS[-3]) {
      drive <- paste0(letter, ":/")
      if (dir.exists(drive)) {
        volumes <- c(volumes, stats::setNames(drive, toupper(paste0(letter, ":"))))
      }
    }
    return(volumes)
  }
}

#' Create and manage undo/redo action
#'
#' @description
#' Creates an action object for the undo/redo system, containing information about
#' the type of action, the data involved, and how to reverse the action.
#'
#' @param type Character string specifying the type of action
#' @param data List containing the action data
#' @param reverse_data Optional list containing data for reversing the action
#'
#' @return List containing:
#'   \itemize{
#'     \item type: Action type identifier
#'     \item data: Action data
#'     \item reverse_data: Data for reversing the action
#'     \item timestamp: Time the action was created
#'   }
#'
#' @examples
#' \dontrun{
#' # Create an add annotation action
#' action <- create_action(
#'   type = "add_annotation",
#'   data = list(
#'     start = 1,
#'     end = 10,
#'     code = "important"
#'   )
#' )
#'
#' # Create a merge codes action
#' merge_action <- create_action(
#'   type = "merge_codes",
#'   data = list(
#'     old_codes = c("code1", "code2"),
#'     new_code = "merged_code"
#'   )
#' )
#' }
#'
#' @keywords internal
create_action <- function(type, data, reverse_data = NULL) {
  list(
    type = type,
    data = data,
    reverse_data = reverse_data,
    timestamp = Sys.time()
  )
}

#' Add action to history
#'
#' @param rv Reactive values object
#' @param action Action to add
#' @keywords internal
#' @examples
#' \dontrun{
#' # Initialize reactive values
#' rv <- reactiveValues(
#'   action_history = list(),
#'   action_index = 0
#' )
#'
#' # Create and add an action
#' action <- create_action(
#'   type = "add_annotation",
#'   data = list(start = 1, end = 10, code = "example")
#' )
#' add_action(rv, action)
#' }
add_action <- function(rv, action) {
  # Remove any future actions if we're not at the end
  if (rv$action_index < length(rv$action_history)) {
    rv$action_history <- rv$action_history[1:rv$action_index]
  }

  # Add the new action
  rv$action_history[[rv$action_index + 1]] <- action
  rv$action_index <- rv$action_index + 1
}

#' Apply or reverse an action
#'
#' @description
#' Applies or reverses an action in the undo/redo system. Handles different types of
#' actions including adding/removing annotations and merging/unmerging codes.
#'
#' @param rv ReactiveValues object containing application state
#' @param action List containing action information
#' @param reverse Logical indicating whether to reverse the action
#'
#' @return Invisible rv (ReactiveValues object)
#'
#' @examples
#' \dontrun{
#' # Create and apply an action
#' action <- create_action(
#'   type = "add_annotation",
#'   data = list(start = 1, end = 10, code = "code1")
#' )
#' apply_action(rv, action)
#'
#' # Reverse the action
#' apply_action(rv, action, reverse = TRUE)
#' }
#'
#' @keywords internal
apply_action <- function(rv, action, reverse = FALSE) {
  data <- if (reverse) action$reverse_data else action$data

  switch(action$type,
         "add_annotation" = {
           if (reverse) {
             # Remove annotation
             if(nrow(rv$annotations) > 0) {
               rv$annotations <- rv$annotations[-which(
                 rv$annotations$start == data$start &
                   rv$annotations$end == data$end &
                   rv$annotations$code == data$code
               ), ]
             }
           } else {
             # Add annotation
             if(is.null(rv$annotations)) {
               rv$annotations <- data.frame(
                 start = integer(),
                 end = integer(),
                 code = character(),
                 stringsAsFactors = FALSE
               )
             }
             rv$annotations <- rbind(rv$annotations, data)
           }
         },
         "merge_codes" = {
           if (reverse) {
             # Reverse the merge
             indices <- which(rv$annotations$code == data$new_code)
             if (length(indices) > 0) {
               # Restore original codes
               original_codes <- data$old_codes
               for (i in seq_along(indices)) {
                 rv$annotations$code[indices[i]] <- original_codes[i %% length(original_codes) + 1]
               }
             }
             # Restore code colors
             for (old_code in data$old_codes) {
               if (!is.null(data$old_colors[[old_code]])) {
                 rv$code_colors[old_code] <- data$old_colors[[old_code]]
               }
             }
             rv$code_colors <- rv$code_colors[names(rv$code_colors) != data$new_code]
           } else {
             # Store old colors before merge
             old_colors <- list()
             for (code in data$old_codes) {
               old_colors[[code]] <- rv$code_colors[code]
             }

             # Update annotations with new code
             rv$annotations$code[rv$annotations$code %in% data$old_codes] <- data$new_code

             # Update code colors
             rv$code_colors[data$new_code] <- sprintf("#%06X", sample(0:16777215, 1))
             # Remove old code colors
             rv$code_colors <- rv$code_colors[!names(rv$code_colors) %in% data$old_codes]

             # Store old colors in reverse data
             data$old_colors <- old_colors
           }
         })

  invisible(rv)
}

#' Get all code names from hierarchy
#'
#' @description
#' Recursively extracts all code names from a code hierarchy tree structure,
#' traversing through all nodes and collecting their names.
#'
#' @param node Root node of the code hierarchy (data.tree Node object)
#'
#' @return Character vector containing all code names in the hierarchy
#'
#' @examples
#' \dontrun{
#' # Create a simple code hierarchy
#' root <- Node$new("Root")
#' theme1 <- root$AddChild("Theme1")
#' theme1$AddChild("Code1")
#' theme1$AddChild("Code2")
#'
#' # Get all code names
#' codes <- get_code_names(root)
#' print(codes)  # c("Root", "Theme1", "Code1", "Code2")
#' }
#'
#' @keywords internal
get_code_names <- function(node) {
  if (node$isLeaf) {
    return(node$name)
  } else {
    return(c(node$name, unlist(lapply(node$children, get_code_names))))
  }
}

#' Save project state to file
#'
#' @description
#' Saves the current project state including text, annotations, codes, code tree,
#' and other metadata to an RDS file. Creates the projects directory if it doesn't exist.
#'
#' @param state List containing project state elements:
#'   \itemize{
#'     \item text: Character string of the current text
#'     \item annotations: Data frame of annotations
#'     \item codes: Character vector of codes
#'     \item code_tree: Node object representing code hierarchy
#'     \item code_colors: Named vector of code colors
#'     \item memos: List of memos
#'     \item code_descriptions: List of code descriptions
#'   }
#' @param filename Character string specifying the filename for saving
#'
#' @return Invisible NULL. Called for side effect of saving project state.
#'
#' @examples
#' \dontrun{
#' state <- list(
#'   text = "Sample text",
#'   annotations = data.frame(
#'     start = 1,
#'     end = 5,
#'     code = "code1",
#'     stringsAsFactors = FALSE
#'   ),
#'   codes = "code1",
#'   code_tree = Node$new("Root"),
#'   code_colors = c(code1 = "#FF0000"),
#'   memos = list(),
#'   code_descriptions = list()
#' )
#' save_project_state(state, "my_project")
#' }
#'
#' @importFrom utils packageVersion
#' @keywords internal
save_project_state <- function(state, filename) {
  # Create the projects directory if it doesn't exist
  project_dir <- get_project_dir()
  if (is.null(project_dir)) return(invisible(NULL))

  # Add .rds extension if not present
  if (!grepl("\\.rds$", filename)) {
    filename <- paste0(filename, ".rds")
  }

  # Clean the path and get full filepath
  filepath <- file.path(project_dir, basename(filename))

  # Add version information
  state$version <- utils::packageVersion("textAnnotatoR")

  # Save state to RDS file
  handle_error(
    expr = saveRDS(state, file = filepath),
    success_msg = paste("Project saved successfully to", filepath),
    error_msg = "Failed to save project"
  )

  invisible(NULL)
}

#' Get project directory path
#'
#' @description
#' Retrieves or creates the project directory path where all project files will be stored.
#' Creates the directory if it doesn't exist.
#'
#' @return Character string containing the project directory path, or NULL if creation fails
#'
#' @examples
#' \dontrun{
#' # Get project directory
#' project_dir <- get_project_dir()
#'
#' # Check if directory exists
#' if (!is.null(project_dir)) {
#'   list.files(project_dir)
#' }
#' }
#'
#' @importFrom shiny showNotification
#' @keywords internal
get_project_dir <- function() {
  project_dir <- handle_error(
    expr = {
      data_dir <- init_data_dir()
      project_dir <- file.path(data_dir, "projects")
      if (!dir.exists(project_dir)) {
        dir.create(project_dir, recursive = TRUE)
      }
      project_dir
    },
    error_msg = "Failed to create or access project directory"
  )
  return(project_dir)
}

#' Load project state from file
#'
#' @description
#' Loads a previously saved project state from an RDS file. Performs version checking
#' and data structure validation during the loading process.
#'
#' @param filename Character string specifying the filename to load
#'
#' @return List containing the loaded project state, or NULL if loading fails
#'
#' @importFrom data.tree as.Node
#' @importFrom utils packageVersion
#'
#' @examples
#' \dontrun{
#' # Load a project
#' project_state <- load_project_state("project_name")
#'
#' # Check loaded components
#' str(project_state$text)
#' str(project_state$annotations)
#' }
#'
#' @keywords internal
load_project_state <- function(filename) {
  # Add .rds extension if not present
  if (!grepl("\\.rds$", filename)) {
    filename <- paste0(filename, ".rds")
  }

  # Get the projects directory and create full filepath
  project_dir <- get_project_dir()
  filepath <- file.path(project_dir, basename(filename))

  if (!file.exists(filepath)) {
    showNotification(paste("Project file not found:", filepath), type = "error")
    return(NULL)
  }

  handle_error(
    expr = {
      state <- readRDS(filepath)

      # Version check
      current_version <- utils::packageVersion("textAnnotatoR")
      if (!is.null(state$version) && state$version > current_version) {
        warning("Project was created with a newer version of textAnnotatoR")
      }

      # Convert list back to data.tree object if necessary
      if (!is.null(state$code_tree) && !inherits(state$code_tree, "Node")) {
        state$code_tree <- as.Node(state$code_tree)
      }

      return(state)
    },
    error_msg = paste("Failed to load project from", filepath)
  )
}

#' Load selected project state
#'
#' @description
#' Loads a previously saved project state and updates all reactive values.
#'
#' @param rv Reactive values object to update
#' @param input Shiny input object
#' @param session The current Shiny session
#' @return Invisible NULL
#' @keywords internal
#' @examples
#' \dontrun{
#' # In a Shiny server function:
#' server <- function(input, output, session) {
#'   rv <- reactiveValues(
#'     text = "",
#'     annotations = data.frame(),
#'     codes = character(),
#'     code_tree = Node$new("Root")
#'   )
#'
#'   observeEvent(input$load_button, {
#'     load_selected_project(rv, input, session)
#'   })
#' }
#' }
load_selected_project <- function(rv, input, session, project_name) {
  project_state <- load_project_state(paste0(project_name, ".rds"))
  if (!is.null(project_state)) {
    # Update all reactive values with loaded state
    rv$text <- project_state$text
    rv$annotations <- project_state$annotations
    rv$codes <- project_state$codes
    rv$code_tree <- project_state$code_tree
    rv$code_colors <- project_state$code_colors
    rv$memos <- project_state$memos
    rv$code_descriptions <- project_state$code_descriptions
    rv$history <- project_state$history
    rv$history_index <- project_state$history_index
    rv$current_project <- input$project_to_load
    rv$project_modified <- FALSE

    # Update UI elements
    updateTextAreaInput(session, "text_input", value = rv$text)
    session$sendCustomMessage("clearSelection", list())

    showNotification("Project loaded successfully", type = "message")
  }
  shiny::removeModal()
}

#' Update text display with annotations
#'
#' @description
#' Creates an HTML representation of the text with annotations, highlighting codes
#' with their assigned colors.
#'
#' @param rv Reactive values object containing text and annotations
#' @return HTML string containing the formatted text with annotations
#' @keywords internal
update_text_display <- function(rv) {
  if (nrow(rv$annotations) == 0) {
    return(paste0("<span class='char' id='char_", 1:nchar(rv$text), "'>",
                  strsplit(rv$text, "")[[1]], "</span>", collapse = ""))
  }

  sorted_annotations <- rv$annotations[order(rv$annotations$start), ]
  displayed_text <- ""
  last_end <- 0

  for (i in 1:nrow(sorted_annotations)) {
    if (sorted_annotations$start[i] > last_end + 1) {
      displayed_text <- paste0(displayed_text,
                               paste0("<span class='char' id='char_", (last_end + 1):(sorted_annotations$start[i] - 1), "'>",
                                      strsplit(substr(rv$text, last_end + 1, sorted_annotations$start[i] - 1), "")[[1]],
                                      "</span>", collapse = ""))
    }
    code_color <- rv$code_colors[sorted_annotations$code[i]]
    if (is.null(code_color)) {
      code_color <- "#CCCCCC"  # Default color if not found
    }
    displayed_text <- paste0(displayed_text,
                             "<span class='code-display' style='background-color: ", code_color, ";' data-code='", sorted_annotations$code[i], "' data-start='", sorted_annotations$start[i], "' data-end='", sorted_annotations$end[i], "'>",
                             "[", sorted_annotations$code[i], "]",
                             paste0("<span class='char' id='char_", sorted_annotations$start[i]:sorted_annotations$end[i], "'>",
                                    strsplit(substr(rv$text, sorted_annotations$start[i], sorted_annotations$end[i]), "")[[1]],
                                    "</span>", collapse = ""),
                             "</span>")
    last_end <- sorted_annotations$end[i]
  }

  if (last_end < nchar(rv$text)) {
    displayed_text <- paste0(displayed_text,
                             paste0("<span class='char' id='char_", (last_end + 1):nchar(rv$text), "'>",
                                    strsplit(substr(rv$text, last_end + 1, nchar(rv$text)), "")[[1]],
                                    "</span>", collapse = ""))
  }

  return(displayed_text)
}

#' Concatenate memo texts
#'
#' @description
#' Combines existing and new memo texts with proper separators,
#' handling empty memos appropriately.
#'
#' @param existing_memo Character string containing current memo text
#' @param new_memo Character string containing memo text to append
#'
#' @return Character string of combined memo text
#'
#' @examples
#' \dontrun{
#' # Combine non-empty memos
#' concatenate_memos("First note", "Second note")
#' # Returns: "First note; Second note"
#'
#' # Handle empty existing memo
#' concatenate_memos("", "New note")
#' # Returns: "New note"
#' }
#' @keywords internal
concatenate_memos <- function(existing_memo, new_memo) {
  if (existing_memo == "") {
    return(new_memo)
  } else {
    return(paste(existing_memo, new_memo, sep = "; "))
  }
}

#' Save annotated text as HTML document
#'
#' @description
#' Creates an HTML document containing the annotated text with proper styling
#' for code highlights and formatting.
#'
#' @param filename Character string specifying output file path
#' @param rv ReactiveValues object containing:
#'   \itemize{
#'     \item text: Original text content
#'     \item annotations: Data frame of annotations
#'     \item code_colors: Named character vector of code colors
#'   }
#'
#' @return Invisible NULL, called for side effect
#'
#' @examples
#' \dontrun{
#' # Create HTML with annotations
#' rv <- list(
#'   text = "Sample text",
#'   annotations = data.frame(
#'     start = 1, end = 6,
#'     code = "code1"
#'   ),
#'   code_colors = c(code1 = "#FF0000")
#' )
#' save_as_html("output.html", rv)
#' }
#' @keywords internal
save_as_html <- function(filename, rv) {
  # Get the current state of the text display
  html_content <- update_text_display(rv)

  # Create a complete HTML document
  full_html <- paste0(
    "<!DOCTYPE html>\n<html>\n<head>\n",
    "<style>\n",
    ".code-display { padding: 2px 5px; margin-right: 5px; border-radius: 3px; font-weight: bold; color: black; }\n",
    "</style>\n",
    "</head>\n<body>\n",
    "<h1>Annotated Text</h1>\n",
    "<div id='annotated_text'>\n",
    html_content,
    "\n</div>\n",
    "</body>\n</html>"
  )

  # Write the HTML content to a file
  writeLines(full_html, filename)
}

#' Save annotated text as plain text
#'
#' @description
#' Creates a plain text file containing the annotated text with code markers.
#'
#' @param filename Character string specifying output file path
#' @param rv ReactiveValues object containing:
#'   \itemize{
#'     \item text: Original text content
#'     \item annotations: Data frame of annotations
#'   }
#'
#' @return Invisible NULL, called for side effect
#'
#' @examples
#' \dontrun{
#' rv <- list(
#'   text = "Sample text",
#'   annotations = data.frame(
#'     start = 1, end = 6,
#'     code = "code1"
#'   )
#' )
#' save_as_text("output.txt", rv)
#' }
#' @keywords internal
save_as_text <- function(filename, rv) {
  # Get the annotated text
  annotated_text <- create_plain_text_annotations(rv$text, rv$annotations)

  # Write the content to a file
  writeLines(annotated_text, filename)
}

#' Create plain text version of annotations
#'
#' @description
#' Converts annotated text to plain text format with code markers. Each annotation
#' is represented as a code identifier and annotated text wrapped in square brackets.
#' Multiple annotations are preserved and shown in order of appearance in the text.
#'
#' @param text Character string containing the original text
#' @param annotations Data frame of annotations with columns:
#'   \itemize{
#'     \item start: Numeric vector of starting positions
#'     \item end: Numeric vector of ending positions
#'     \item code: Character vector of code names
#'   }
#'
#' @return Character string containing formatted text with code markers
#'
#' @examples
#' \dontrun{
#' # Simple example with one annotation
#' text <- "This is a sample text"
#' annotations <- data.frame(
#'   start = 1,
#'   end = 4,
#'   code = "code1",
#'   stringsAsFactors = FALSE
#' )
#' result <- create_plain_text_annotations(text, annotations)
#' # Result will be: "[code1: This] is a sample text"
#'
#' # Example with multiple annotations
#' text <- "The quick brown fox"
#' annotations <- data.frame(
#'   start = c(1, 10),
#'   end = c(3, 15),
#'   code = c("article", "adjective"),
#'   stringsAsFactors = FALSE
#' )
#' result <- create_plain_text_annotations(text, annotations)
#' # Result will be: "[article: The] quick [adjective: brown fox]"
#' }
#'
#' @keywords internal
create_plain_text_annotations <- function(text, annotations) {
  if (nrow(annotations) == 0) {
    return(text)
  }

  sorted_annotations <- annotations[order(annotations$start), ]
  plain_text <- ""
  last_end <- 0

  for (i in 1:nrow(sorted_annotations)) {
    if (sorted_annotations$start[i] > last_end + 1) {
      plain_text <- paste0(plain_text, substr(text, last_end + 1, sorted_annotations$start[i] - 1))
    }
    plain_text <- paste0(plain_text,
                         "[", sorted_annotations$code[i], ": ",
                         substr(text, sorted_annotations$start[i], sorted_annotations$end[i]),
                         "]")
    last_end <- sorted_annotations$end[i]
  }

  if (last_end < nchar(text)) {
    plain_text <- paste0(plain_text, substr(text, last_end + 1, nchar(text)))
  }

  return(plain_text)
}

#' Initialize new project
#'
#' @description
#' Creates new project by resetting all reactive values to defaults
#' and clearing UI elements.
#'
#' @param rv ReactiveValues object to reset containing:
#'   \itemize{
#'     \item text: Text content
#'     \item annotations: Annotation data frame
#'     \item codes: Vector of codes
#'     \item code_tree: Hierarchy Node object
#'     \item All other project state values
#'   }
#' @param session Shiny session object
#'
#' @return Invisible NULL, called for side effect
#'
#' @examples
#' \dontrun{
#' # In Shiny server:
#' observeEvent(input$new_project, {
#'   create_new_project(rv, session)
#' })
#' }
#' @keywords internal
create_new_project <- function(rv, session) {
  rv$text <- ""
  rv$annotations <- data.frame(
    start = integer(),
    end = integer(),
    text = character(),
    code = character(),
    memo = character(),
    stringsAsFactors = FALSE
  )
  rv$codes <- character()
  rv$code_tree <- Node$new("Root")
  rv$code_colors <- character()
  rv$memos <- list()
  rv$code_descriptions <- list()
  rv$history <- list(list(text = "", annotations = data.frame()))
  rv$history_index <- 1
  rv$current_project <- NULL
  rv$project_modified <- FALSE
  rv$action_history <- list()
  rv$action_index <- 0
  rv$merged_codes <- list()

  # Clear UI elements
  updateTextAreaInput(session, "text_input", value = "")
  session$sendCustomMessage("clearSelection", list())

  showNotification("New project created", type = "message")
}

#' Generate code frequency visualization
#'
#' @description
#' Creates a barplot visualization showing the frequency of each code in the annotations.
#' The plot displays codes on the x-axis and their frequency counts on the y-axis.
#'
#' @param annotations Data frame containing text annotations with columns:
#'   \itemize{
#'     \item start: numeric, starting position of annotation
#'     \item end: numeric, ending position of annotation
#'     \item code: character, code applied to the annotation
#'   }
#'
#' @return A recordedplot object containing the code frequency visualization
#'
#' @importFrom graphics par barplot
#' @importFrom grDevices recordPlot
#'
#' @examples
#' \dontrun{
#' annotations <- data.frame(
#'   start = c(1, 10, 20),
#'   end = c(5, 15, 25),
#'   code = c("code1", "code2", "code1")
#' )
#' plot <- generate_code_frequency_plot(annotations)
#' print(plot)
#' }
#'
#' @keywords internal
generate_code_frequency_plot <- function(annotations) {
  code_freq <- table(annotations$code)
  code_freq_sorted <- sort(code_freq, decreasing = TRUE)

  par(mar = c(8, 4, 2, 2))
  barplot(code_freq_sorted,
          main = "Code Frequency",
          xlab = "",
          ylab = "Frequency",
          col = "steelblue",
          las = 2)
  return(recordPlot())
}

#' Generate code co-occurrence statistics and visualization
#'
#' @description
#' Performs a comprehensive analysis of code co-occurrences in the text, including
#' calculation of various similarity metrics and generation of network and heatmap
#' visualizations.
#'
#' @param annotations Data frame containing text annotations with columns:
#'   \itemize{
#'     \item start: numeric, starting position of annotation
#'     \item end: numeric, ending position of annotation
#'     \item code: character, code applied to the annotation
#'   }
#'
#' @return List containing:
#'   \itemize{
#'     \item co_occurrence: Matrix of raw co-occurrence counts
#'     \item jaccard_similarity: Matrix of Jaccard similarity coefficients
#'     \item phi_coefficient: Matrix of Phi coefficients
#'     \item network_plot: Network visualization of code relationships
#'     \item heatmap_plot: Heatmap visualization of code co-occurrences
#'     \item summary: List of summary statistics
#'   }
#'
#' @importFrom graphics par plot points text lines image axis
#' @importFrom grDevices rgb colorRampPalette recordPlot
#' @importFrom stats cor
#'
#' @examples
#' \dontrun{
#' annotations <- data.frame(
#'   start = c(1, 5, 10),
#'   end = c(8, 15, 20),
#'   code = c("code1", "code2", "code1")
#' )
#' results <- generate_code_co_occurrence_analysis(annotations)
#' print(results$summary)
#' }
#'
#' @keywords internal
generate_code_co_occurrence_analysis <- function(annotations) {
  # Get unique codes
  codes <- unique(annotations$code)
  n_codes <- length(codes)

  # Initialize matrices
  co_matrix <- matrix(0, nrow = n_codes, ncol = n_codes,
                      dimnames = list(codes, codes))
  jaccard_matrix <- matrix(0, nrow = n_codes, ncol = n_codes,
                           dimnames = list(codes, codes))
  phi_matrix <- matrix(0, nrow = n_codes, ncol = n_codes,
                       dimnames = list(codes, codes))

  # Create binary occurrence vectors for each code
  code_occurrences <- lapply(codes, function(code) {
    intervals <- annotations[annotations$code == code, c("start", "end")]
    sort(unique(unlist(apply(intervals, 1, function(x) seq(x[1], x[2])))))
  })
  names(code_occurrences) <- codes

  # Calculate co-occurrence and similarity matrices
  for (i in 1:n_codes) {
    for (j in 1:n_codes) {
      if (i != j) {
        # Get occurrence vectors
        code1_pos <- code_occurrences[[i]]
        code2_pos <- code_occurrences[[j]]

        # Calculate co-occurrence (overlap)
        overlap <- length(intersect(code1_pos, code2_pos))
        co_matrix[i, j] <- overlap

        # Calculate Jaccard similarity
        union_size <- length(unique(c(code1_pos, code2_pos)))
        jaccard_matrix[i, j] <- if(union_size > 0) overlap / union_size else 0

        # Calculate Phi coefficient
        n11 <- overlap  # co-occurrence count
        n00 <- nchar(max(annotations$end)) - length(unique(c(code1_pos, code2_pos)))  # neither code
        n10 <- length(code1_pos) - overlap  # code1 only
        n01 <- length(code2_pos) - overlap  # code2 only
        n <- n11 + n10 + n01 + n00

        # Phi coefficient calculation
        if (n > 0) {
          expected <- (length(code1_pos) * length(code2_pos)) / n
          phi_matrix[i, j] <- (n11 - expected) /
            sqrt((length(code1_pos) * length(code2_pos) *
                    (n - length(code1_pos)) * (n - length(code2_pos))) / n)
        }
      }
    }
  }

  # Generate network visualization
  network_plot <- function() {
    if (n_codes <= 1) {
      plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1),
           main = if(n_codes == 0) "No codes available" else "Only one code present")
      if (n_codes == 1) {
        points(0.5, 0.5, pch = 16, col = "red", cex = 2)
        text(0.5, 0.5, labels = codes[1], pos = 3, offset = 0.5)
      }
      return(recordPlot())
    }

    # Calculate node positions using circular layout
    angles <- seq(0, 2 * pi, length.out = n_codes + 1)[-1]
    x <- 0.5 + 0.4 * cos(angles)
    y <- 0.5 + 0.4 * sin(angles)

    # Set up plot
    par(mar = c(1, 1, 2, 1))
    plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1),
         main = "Code Co-occurrence Network")

    # Draw edges
    for (i in 1:n_codes) {
      for (j in 1:n_codes) {
        if (i < j && jaccard_matrix[i, j] > 0) {
          # Scale line width based on Jaccard similarity
          lwd <- 1 + 5 * jaccard_matrix[i, j]
          # Scale opacity based on phi coefficient
          alpha <- 0.3 + 0.7 * abs(phi_matrix[i, j])
          lines(c(x[i], x[j]), c(y[i], y[j]),
                lwd = lwd,
                col = grDevices::rgb(0, 0, 1, alpha = alpha))
        }
      }
    }

    # Draw nodes and labels
    points(x, y, pch = 16, col = "red", cex = 2)
    text(x, y, labels = codes, pos = 3, offset = 0.5)

    return(grDevices::recordPlot())
  }

  # Heatmap plot function
  heatmap_plot <- function() {
    if (n_codes == 0) {
      plot(1, type = "n", xlab = "", ylab = "",
           main = "No codes available for heatmap")
      return(grDevices::recordPlot())
    }

    # Set up the plotting area
    par(mar = c(8, 8, 2, 2))

    # Create the heatmap
    graphics::image(1:n_codes, 1:n_codes, jaccard_matrix,
                    main = "Code Co-occurrence Heatmap",
                    xlab = "", ylab = "",
                    axes = FALSE,
                    col = grDevices::colorRampPalette(c("white", "steelblue"))(100))

    # Add axes with code labels
    graphics::axis(1, 1:n_codes, codes, las = 2)
    graphics::axis(2, 1:n_codes, codes, las = 2)

    return(grDevices::recordPlot())
  }

  # Return results
  return(list(
    co_occurrence = co_matrix,
    jaccard_similarity = jaccard_matrix,
    phi_coefficient = phi_matrix,
    network_plot = network_plot(),
    heatmap_plot = heatmap_plot(),
    summary = list(
      total_codes = n_codes,
      max_co_occurrence = max(co_matrix),
      max_jaccard = max(jaccard_matrix),
      mean_jaccard = mean(jaccard_matrix[upper.tri(jaccard_matrix)]),
      significant_pairs = sum(abs(phi_matrix) > 0.3, na.rm = TRUE) / 2
    )
  ))
}

#' Generate word cloud visualization
#'
#' @description
#' Creates a simple word cloud visualization from the input text, showing the most
#' frequent words with size proportional to their frequency.
#'
#' @param text Character string containing the text to visualize
#'
#' @return A plot object containing the word cloud visualization
#'
#' @importFrom graphics plot text
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' text <- "This is a sample text with repeated words. Sample text for visualization."
#' cloud <- generate_word_cloud(text)
#' }
generate_word_cloud <- function(text) {
  words <- unlist(strsplit(tolower(text), "\\W+"))
  word_freq <- sort(table(words[nchar(words) > 3]), decreasing = TRUE)
  word_freq <- word_freq[1:min(100, length(word_freq))]

  plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1),
       main = "Word Cloud")

  n <- length(word_freq)
  angles <- runif(n, 0, 2 * pi)
  x <- 0.5 + 0.4 * cos(angles)
  y <- 0.5 + 0.4 * sin(angles)

  sizes <- 1 + 3 * (word_freq - min(word_freq)) / (max(word_freq) - min(word_freq))
  text(x, y, labels = names(word_freq), cex = sizes,
       col = rainbow(n, s = 0.7, v = 0.7))

  return(recordPlot())
}

#' Generate text summary statistics
#'
#' @description
#' Calculates basic summary statistics for the annotated text, including word counts,
#' character counts, annotation counts, and unique code counts.
#'
#' @param text Character string containing the text being analyzed
#' @param annotations Data frame of annotations with columns:
#'   \itemize{
#'     \item start: numeric, starting position of annotation
#'     \item end: numeric, ending position of annotation
#'     \item code: character, code applied to the annotation
#'   }
#'
#' @return A list containing summary statistics:
#'   \itemize{
#'     \item total_words: total number of words in the text
#'     \item total_characters: total number of characters
#'     \item total_sentences: number of sentences (approximated by punctuation)
#'     \item total_paragraphs: number of paragraphs (non-empty lines)
#'     \item total_annotations: number of annotations
#'     \item unique_codes: number of unique codes used
#'   }
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' text <- "This is a sample text.\nIt has two paragraphs."
#' annotations <- data.frame(
#'   start = c(1, 10),
#'   end = c(4, 15),
#'   code = c("code1", "code2")
#' )
#' summary <- generate_text_summary(text, annotations)
#' }
generate_text_summary <- function(text, annotations) {
  # Count paragraphs (sequences separated by blank lines)
  paragraphs <- strsplit(text, "\n")[[1]]
  # Count actual paragraphs (non-empty lines)
  paragraph_count <- sum(nzchar(trimws(paragraphs)))

  list(
    total_words = length(unlist(strsplit(text, "\\W+"))),
    total_characters = nchar(text),
    total_sentences = length(unlist(strsplit(text, "[.!?]+\\s+"))),
    total_paragraphs = paragraph_count,
    total_annotations = nrow(annotations),
    unique_codes = length(unique(annotations$code))
  )
}

#' Add theme to code hierarchy
#'
#' @description
#' Adds a new theme to the code hierarchy tree. Themes can be used to organize and
#' group related codes in a hierarchical structure.
#'
#' @param node Root node of the hierarchy tree
#' @param theme_name Character string specifying the name of the new theme
#' @param description Optional character string providing a description of the theme
#'
#' @return Updated node with new theme added
#'
#' @importFrom data.tree Node
#'
#' @examples
#' \dontrun{
#' # Create root node
#' root <- Node$new("Root")
#'
#' # Add a new theme
#' root <- add_theme(root, "Methods", "Research methodology codes")
#'
#' # Add a sub-theme
#' root <- add_theme(root, "Qualitative Methods", "Qualitative research approaches")
#' }
#'
#' @keywords internal
add_theme <- function(node, theme_name, description = "") {
  # Check if theme already exists
  if (!is.null(node$children[[theme_name]])) {
    stop("Theme already exists")
  }

  # Create new theme node
  new_theme <- node$AddChild(theme_name)
  new_theme$description <- description
  new_theme$type <- "theme"
  new_theme$created <- Sys.time()

  return(node)
}

#' Process comparison file
#'
#' @description
#' Processes uploaded comparison files, handling different file formats (CSV, JSON)
#' and ensuring proper data structure and types for comparison analysis.
#'
#' @param filepath Character string specifying the path to the comparison file
#'
#' @return Data frame containing processed annotations with columns:
#'   \itemize{
#'     \item start: numeric, starting position of annotation
#'     \item end: numeric, ending position of annotation
#'     \item code: character, code applied to the annotation
#'   }
#'
#' @importFrom jsonlite fromJSON
#' @importFrom utils read.csv
#'
#' @examples
#' \dontrun{
#' # Process a CSV file
#' annotations_csv <- process_comparison_file("annotations.csv")
#'
#' # Process a JSON file
#' annotations_json <- process_comparison_file("annotations.json")
#' }
#'
#' @keywords internal
process_comparison_file <- function(filepath) {
  ext <- tolower(tools::file_ext(filepath))

  df <- if (ext == "csv") {
    read.csv(filepath, stringsAsFactors = FALSE)
  } else if (ext == "json") {
    fromJSON(filepath)
  } else {
    stop("Unsupported file format")
  }

  # If the file is from Records tab (has text and memo columns), reformat it
  if (all(c("start", "end", "text", "code", "memo") %in% colnames(df))) {
    df <- df[, c("start", "end", "code")]
  }

  # Check required columns
  required_cols <- c("start", "end", "code")
  if (!all(required_cols %in% colnames(df))) {
    missing_cols <- setdiff(required_cols, colnames(df))
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  # Convert columns to proper types
  df$start <- as.numeric(as.character(df$start))
  df$end <- as.numeric(as.character(df$end))
  df$code <- as.character(df$code)

  # Remove rows with NA values
  df <- df[complete.cases(df[, required_cols]), ]

  if (nrow(df) == 0) {
    stop("No valid annotations found after cleaning")
  }

  return(df)
}

#' Compare coding patterns between different documents or coders
#'
#' @description
#' Performs a comprehensive comparison of coding patterns between different sets of
#' annotations, analyzing differences in coverage, code application, overlaps, and
#' code sequences.
#'
#' @param annotations_list A list of data frames, where each data frame contains
#'        annotations with columns:
#'   \itemize{
#'     \item start: numeric, starting position of annotation
#'     \item end: numeric, ending position of annotation
#'     \item code: character, code applied to the annotation
#'   }
#'
#' @return A list containing comparison results and analysis:
#'   \itemize{
#'     \item coding_strategies: list of analyzed coding patterns for each input
#'     \item comparison: list of comparative analyses between coding patterns
#'   }
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' # Create sample annotation sets
#' annotations1 <- data.frame(
#'   start = c(1, 10),
#'   end = c(5, 15),
#'   code = c("code1", "code2")
#' )
#' annotations2 <- data.frame(
#'   start = c(2, 12),
#'   end = c(6, 18),
#'   code = c("code1", "code3")
#' )
#' results <- generate_comparison_analysis(list(annotations1, annotations2))
#' }
generate_comparison_analysis <- function(annotations_list) {
  if (!is.list(annotations_list) || length(annotations_list) < 2) {
    stop("Need at least two annotation sets for comparison")
  }

  # Process each annotation set with error handling
  coding_strategies <- lapply(annotations_list, function(annotations) {
    tryCatch({
      if (!is.data.frame(annotations)) {
        stop("Invalid data format: input must be a data frame")
      }

      # Validate and clean data
      required_cols <- c("start", "end", "code")
      if (!all(required_cols %in% colnames(annotations))) {
        stop(paste("Missing required columns:",
                   paste(setdiff(required_cols, colnames(annotations)), collapse = ", ")))
      }

      # Ensure proper types and handle NA values
      annotations$start <- as.numeric(as.character(annotations$start))
      annotations$end <- as.numeric(as.character(annotations$end))
      annotations$code <- as.character(annotations$code)

      # Remove invalid rows
      valid_rows <- !is.na(annotations$start) &
        !is.na(annotations$end) &
        !is.na(annotations$code) &
        annotations$start <= annotations$end

      if (sum(valid_rows) == 0) {
        stop("No valid annotations found after cleaning")
      }

      annotations <- annotations[valid_rows, ]

      # Calculate coverage statistics with error handling
      coverage <- tryCatch({
        list(
          distribution = list(
            frequencies = table(annotations$code)
          )
        )
      }, error = function(e) {
        list(distribution = list(frequencies = table(character(0))))
      })

      # Calculate co-occurrence statistics with error handling
      co_occurrences <- tryCatch({
        list(
          combinations = list(
            frequencies = calculate_co_occurrences(annotations)
          )
        )
      }, error = function(e) {
        list(combinations = list(frequencies = table(character(0))))
      })

      # Calculate sequence statistics with error handling
      sequences <- tryCatch({
        list(
          transitions = calculate_transitions(annotations[order(annotations$start), ])
        )
      }, error = function(e) {
        list(transitions = list())
      })

      return(list(
        coverage = coverage,
        co_occurrences = co_occurrences,
        sequences = sequences
      ))
    }, error = function(e) {
      # Return empty results if processing fails
      list(
        coverage = list(distribution = list(frequencies = table(character(0)))),
        co_occurrences = list(combinations = list(frequencies = table(character(0)))),
        sequences = list(transitions = list())
      )
    })
  })

  # Calculate comparison metrics with error handling
  comparison <- tryCatch({
    list(
      coverage_differences = compare_coverage(coding_strategies),
      code_differences = compare_codes(coding_strategies),
      overlap_differences = compare_overlaps(coding_strategies)
    )
  }, error = function(e) {
    list(
      coverage_differences = "Error calculating differences",
      code_differences = "Error comparing codes",
      overlap_differences = "Error analyzing overlaps"
    )
  })

  return(list(
    coding_strategies = coding_strategies,
    comparison = comparison
  ))
}

#' Helper function to calculate co-occurrences
#' @param annotations Data frame of annotations
#' @return Table of co-occurrence frequencies
calculate_co_occurrences <- function(annotations) {
  if (!is.data.frame(annotations) || nrow(annotations) <= 1) {
    return(table(character(0)))
  }

  tryCatch({
    co_occurrences <- c()

    # Sort annotations by start position
    annotations <- annotations[order(annotations$start), ]

    # Find overlapping annotations
    for (i in 1:(nrow(annotations)-1)) {
      for (j in (i+1):nrow(annotations)) {
        if (annotations$start[j] <= annotations$end[i]) {
          pair <- sort(c(annotations$code[i], annotations$code[j]))
          co_occurrences <- c(co_occurrences, paste(pair, collapse=" & "))
        } else {
          break  # No more overlaps possible with current i
        }
      }
    }

    return(table(co_occurrences))
  }, error = function(e) {
    return(table(character(0)))
  })
}

#' Helper function to calculate transitions
#' @param annotations Data frame of annotations
#' @return List of transitions
calculate_transitions <- function(annotations) {
  if (!is.data.frame(annotations) || nrow(annotations) <= 1) {
    return(list())
  }

  tryCatch({
    # Sort annotations by start position
    annotations <- annotations[order(annotations$start), ]

    # Create transitions list
    transitions <- vector("list", nrow(annotations) - 1)
    for (i in 1:(nrow(annotations)-1)) {
      transitions[[i]] <- c(
        from = annotations$code[i],
        to = annotations$code[i+1]
      )
    }

    return(transitions)
  }, error = function(e) {
    return(list())
  })
}

#' Compare code usage between coders
#'
#' @description
#' Compares how different coders use codes by analyzing shared codes and their
#' usage patterns across coding strategies.
#'
#' @param coding_strategies List of coding strategies, where each strategy contains
#'        code frequency information
#'
#' @return List containing:
#'   \itemize{
#'     \item shared_codes: Character vector of codes used across strategies
#'     \item usage_matrix: Matrix showing code usage across strategies
#'   }
#'
#' @examples
#' \dontrun{
#' strategy1 <- list(
#'   coverage = list(
#'     distribution = list(
#'       frequencies = c(code1 = 2, code2 = 1)
#'     )
#'   )
#' )
#' strategy2 <- list(
#'   coverage = list(
#'     distribution = list(
#'       frequencies = c(code2 = 2, code3 = 1)
#'     )
#'   )
#' )
#' comparison <- compare_codes(list(strategy1, strategy2))
#' }
#'
#' @keywords internal
compare_codes <- function(coding_strategies) {
  tryCatch({
    all_codes <- unique(unlist(lapply(coding_strategies, function(strategy) {
      names(strategy$coverage$distribution$frequencies)
    })))

    if (length(all_codes) == 0) {
      return(list(
        shared_codes = character(0),
        usage_matrix = matrix(0, nrow = 0, ncol = 0)
      ))
    }

    code_usage <- sapply(coding_strategies, function(strategy) {
      freqs <- strategy$coverage$distribution$frequencies
      sapply(all_codes, function(code) {
        if (code %in% names(freqs)) freqs[code] else 0
      })
    })

    return(list(
      shared_codes = all_codes,
      usage_matrix = code_usage
    ))
  }, error = function(e) {
    return(list(
      shared_codes = character(0),
      usage_matrix = matrix(0, nrow = 0, ncol = 0)
    ))
  })
}

#' Compare overlap patterns between coders
#'
#' @description
#' Analyzes how different coders overlap in their code applications by comparing
#' overlap patterns and frequencies across coding strategies.
#'
#' @param coding_strategies List of coding strategies, where each strategy contains
#'        overlap information
#'
#' @return List containing:
#'   \itemize{
#'     \item total_overlaps_range: Range of total overlaps across strategies
#'     \item unique_pairs_range: Range of unique code pairs across strategies
#'   }
#'
#' @examples
#' \dontrun{
#' strategy1 <- list(
#'   co_occurrences = list(
#'     combinations = list(
#'       frequencies = c("code1-code2" = 2)
#'     )
#'   )
#' )
#' strategy2 <- list(
#'   co_occurrences = list(
#'     combinations = list(
#'       frequencies = c("code2-code3" = 1)
#'     )
#'   )
#' )
#' results <- compare_overlaps(list(strategy1, strategy2))
#' }
#'
#' @keywords internal
compare_overlaps <- function(coding_strategies) {
  tryCatch({
    overlap_stats <- lapply(coding_strategies, function(strategy) {
      freqs <- strategy$co_occurrences$combinations$frequencies
      list(
        total_overlaps = if(length(freqs) > 0) sum(freqs) else 0,
        unique_pairs = length(freqs)
      )
    })

    total_overlaps <- sapply(overlap_stats, `[[`, "total_overlaps")
    unique_pairs <- sapply(overlap_stats, `[[`, "unique_pairs")

    return(list(
      total_overlaps_range = if(length(total_overlaps) > 0) range(total_overlaps) else c(0, 0),
      unique_pairs_range = if(length(unique_pairs) > 0) range(unique_pairs) else c(0, 0)
    ))
  }, error = function(e) {
    return(list(
      total_overlaps_range = c(0, 0),
      unique_pairs_range = c(0, 0)
    ))
  })
}


#' Analyze coverage patterns in annotations
#'
#' @description
#' Analyzes how codes are distributed throughout the text, including clustering
#' patterns and coding density.
#'
#' @param annotations Data frame containing text annotations with columns:
#'   \itemize{
#'     \item start: numeric, starting position of annotation
#'     \item end: numeric, ending position of annotation
#'     \item code: character, code applied to the annotation
#'   }
#'
#' @return List containing:
#'   \itemize{
#'     \item clusters: List of annotation clusters
#'     \item density: List containing overall density metrics
#'     \item distribution: List containing code frequencies and positions
#'   }
#'
#' @examples
#' \dontrun{
#' annotations <- data.frame(
#'   start = c(1, 5, 20),
#'   end = c(3, 8, 25),
#'   code = c("code1", "code2", "code1")
#' )
#' results <- analyze_coverage(annotations)
#' }
#'
#' @keywords internal
analyze_coverage <- function(annotations) {
  if (!is.data.frame(annotations) || nrow(annotations) == 0) {
    return(list(
      clusters = list(),
      density = list(overall_density = 0),
      distribution = list(
        frequencies = integer(0),
        positions = list()
      )
    ))
  }

  # Ensure numeric values and handle NAs
  annotations$start <- as.numeric(as.character(annotations$start))
  annotations$end <- as.numeric(as.character(annotations$end))
  annotations$code <- as.character(annotations$code)

  valid_rows <- !is.na(annotations$start) &
    !is.na(annotations$end) &
    !is.na(annotations$code) &
    annotations$start <= annotations$end

  annotations <- annotations[valid_rows, ]

  if (nrow(annotations) == 0) {
    return(list(
      clusters = list(),
      density = list(overall_density = 0),
      distribution = list(
        frequencies = integer(0),
        positions = list()
      )
    ))
  }

  # Sort annotations by position
  sorted_anns <- annotations[order(annotations$start), ]

  # Calculate code frequencies
  code_freq <- table(sorted_anns$code)

  # Calculate code positions with error handling
  code_pos <- tryCatch({
    tapply(sorted_anns$start, sorted_anns$code, function(x) list(positions = x))
  }, error = function(e) {
    list()
  })

  # Calculate density with error handling
  total_length <- max(sorted_anns$end) - min(sorted_anns$start)
  total_coded <- sum(sorted_anns$end - sorted_anns$start + 1)
  density <- if (total_length > 0) total_coded / total_length else 0

  return(list(
    clusters = find_annotation_clusters(sorted_anns),
    density = list(overall_density = density),
    distribution = list(
      frequencies = code_freq,
      positions = code_pos
    )
  ))
}

#' Analyze code application patterns
#'
#' @description
#' Analyzes patterns in how codes are applied in the annotations.
#'
#' @param annotations Data frame containing code annotations with columns:
#'   \itemize{
#'     \item start: numeric, starting position
#'     \item end: numeric, ending position
#'     \item code: character, code identifier
#'   }
#'
#' @return List containing:
#'   \itemize{
#'     \item patterns: List of code patterns
#'     \item summary: Summary statistics
#'   }
#'
#' @keywords internal
analyze_code_patterns <- function(annotations) {
  if (!is.data.frame(annotations) || nrow(annotations) == 0) {
    return(list(
      patterns = list(),
      summary = list(total_codes = 0)
    ))
  }

  # Group annotations by code with error handling
  code_groups <- tryCatch({
    split(annotations, annotations$code)
  }, error = function(e) {
    list()
  })

  # Analyze patterns for each code
  code_patterns <- lapply(code_groups, function(code_anns) {
    tryCatch({
      lengths <- code_anns$end - code_anns$start + 1
      list(
        typical_length = mean(lengths, na.rm = TRUE),
        length_variation = stats::sd(lengths, na.rm = TRUE),
        code_count = nrow(code_anns)
      )
    }, error = function(e) {
      list(
        typical_length = 0,
        length_variation = 0,
        code_count = 0
      )
    })
  })

  return(list(
    patterns = code_patterns,
    summary = list(
      total_codes = length(code_patterns),
      unique_codes = length(unique(annotations$code))
    )
  ))
}

#' Analyze code co-occurrence patterns
#'
#' @description
#' Analyzes how different codes co-occur within the annotated text by examining overlapping
#' annotations and calculating various metrics of co-occurrence strength.
#'
#' @param annotations A data frame containing text annotations with columns:
#'   \itemize{
#'     \item start: numeric, starting position of annotation
#'     \item end: numeric, ending position of annotation
#'     \item code: character, code applied to the annotation
#'   }
#'
#' @return A list containing co-occurrence analysis results:
#'   \itemize{
#'     \item combinations: list containing frequency table of code co-occurrences
#'     \item characteristics: list with average overlap length and total overlap count
#'   }
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' annotations <- data.frame(
#'   start = c(1, 5, 10),
#'   end = c(8, 15, 20),
#'   code = c("code1", "code2", "code1")
#' )
#' results <- analyze_co_occurrences(annotations)
#' }
analyze_co_occurrences <- function(annotations) {
  if (!is.data.frame(annotations) || nrow(annotations) <= 1) {
    return(list(
      combinations = list(frequencies = integer(0)),
      characteristics = list(avg_length = 0, total_overlaps = 0)
    ))
  }

  # Find overlapping annotations with error handling
  overlaps <- find_overlapping_codes(annotations)

  # Analyze overlap patterns
  if (length(overlaps) > 0) {
    combinations <- tryCatch({
      table(sapply(overlaps, function(x) {
        if (!is.null(x$code1) && !is.null(x$code2)) {
          paste(sort(c(x$code1, x$code2)), collapse = "-")
        } else {
          NA
        }
      }))
    }, error = function(e) {
      table(character(0))
    })

    lengths <- sapply(overlaps, function(x) {
      if (!is.null(x$overlap_start) && !is.null(x$overlap_end)) {
        x$overlap_end - x$overlap_start + 1
      } else {
        NA
      }
    })

    avg_length <- mean(lengths, na.rm = TRUE)
    if (is.nan(avg_length)) avg_length <- 0
  } else {
    combinations <- table(character(0))
    avg_length <- 0
  }

  return(list(
    combinations = list(frequencies = combinations),
    characteristics = list(
      avg_length = avg_length,
      total_overlaps = length(overlaps)
    )
  ))
}

#' Analyze sequences and transitions between codes
#'
#' @description
#' Analyzes how codes are sequenced in the text by examining transitions
#' between consecutive codes and identifying repeated patterns.
#'
#' @param annotations Data frame of text annotations with columns:
#'   \itemize{
#'     \item start: numeric, starting position of annotation
#'     \item end: numeric, ending position of annotation
#'     \item code: character, code applied to the annotation
#'   }
#'
#' @return List containing:
#'   \itemize{
#'     \item transitions: List of transitions between consecutive codes
#'     \item patterns: List of identified repeated code sequences
#'   }
#'
#' @examples
#' \dontrun{
#' annotations <- data.frame(
#'   start = c(1, 10, 20),
#'   end = c(5, 15, 25),
#'   code = c("code1", "code2", "code1")
#' )
#' results <- analyze_sequences(annotations)
#' }
#' @keywords internal
analyze_sequences <- function(annotations) {
  if (!is.data.frame(annotations) || nrow(annotations) <= 1) {
    return(list(
      transitions = list(),
      patterns = list()
    ))
  }

  # Ensure proper types
  annotations$start <- as.numeric(annotations$start)
  annotations$end <- as.numeric(annotations$end)
  annotations$code <- as.character(annotations$code)

  # Remove any rows with NA values
  valid_rows <- stats::complete.cases(annotations[, c("start", "end", "code")])
  annotations <- annotations[valid_rows, ]

  if (nrow(annotations) <= 1) {
    return(list(
      transitions = list(),
      patterns = list()
    ))
  }

  # Sort annotations by position
  sorted_anns <- annotations[order(annotations$start), ]

  # Find transitions
  transitions <- list()
  for (i in 1:(nrow(sorted_anns)-1)) {
    transitions <- c(transitions,
                     list(c(from = sorted_anns$code[i],
                            to = sorted_anns$code[i+1])))
  }

  return(list(
    transitions = transitions,
    patterns = find_repeated_sequences(sorted_anns)
  ))
}

#' Compare coding patterns between different coders
#' @keywords internal
compare_patterns <- function(coding_strategies) {
  if (length(coding_strategies) < 2) {
    return(list(
      coverage_differences = "Insufficient data for comparison",
      code_differences = "Insufficient data for comparison",
      combination_differences = "Insufficient data for comparison",
      sequence_differences = "Insufficient data for comparison"
    ))
  }

  # Compare coverage patterns
  coverage_diff <- tryCatch({
    # Extract densities safely
    densities <- sapply(coding_strategies, function(x) {
      if (!is.null(x$coverage$density$overall_density)) {
        x$coverage$density$overall_density
      } else {
        0
      }
    })

    list(
      density_variation = diff(range(densities)),
      density_summary = sprintf("Coverage density varies from %.2f to %.2f",
                                min(densities), max(densities))
    )
  }, error = function(e) {
    list(
      density_variation = 0,
      density_summary = "Unable to calculate coverage differences"
    )
  })

  # Compare code application patterns
  code_diff <- tryCatch({
    patterns <- lapply(coding_strategies, function(x) {
      if (!is.null(x$code_patterns) && length(x$code_patterns) > 0) {
        x$code_patterns
      } else {
        list()
      }
    })

    list(
      length_variation = "Analysis completed",
      pattern_summary = sprintf("Analyzed %d coding patterns", length(patterns))
    )
  }, error = function(e) {
    list(
      length_variation = "Unable to analyze patterns",
      pattern_summary = "Error in pattern analysis"
    )
  })

  return(list(
    coverage_differences = coverage_diff,
    code_differences = code_diff,
    combination_differences = "Comparison completed",
    sequence_differences = "Comparison completed"
  ))
}

#' Generate comparison visualizations
#'
#' @description
#' Creates a set of visualizations for comparing coding patterns between different
#' coders, including distribution comparisons, overlap patterns, and sequence patterns.
#'
#' @param comparison_results List containing results from generate_comparison_analysis:
#'   \itemize{
#'     \item coding_strategies: List of analyzed coding patterns
#'     \item comparison: List of comparative analyses
#'   }
#'
#' @return List containing plot objects:
#'   \itemize{
#'     \item distribution: Plot comparing code distribution patterns
#'     \item overlap: Plot showing code overlap patterns
#'     \item sequence: Plot displaying code sequence patterns
#'   }
#'
#' @importFrom graphics par barplot text title
#' @importFrom grDevices recordPlot
#'
#' @examples
#' \dontrun{
#' # Create sample comparison results
#' results <- generate_comparison_analysis(list(
#'   annotations1 = data.frame(start = c(1, 5), end = c(3, 8), code = c("A", "B")),
#'   annotations2 = data.frame(start = c(2, 6), end = c(4, 9), code = c("A", "C"))
#' ))
#'
#' # Generate visualizations
#' plots <- generate_comparison_plots(results)
#' print(plots$distribution)
#' }
#'
#' @keywords internal
generate_comparison_plots <- function(comparison_results) {
  if (is.null(comparison_results) || length(comparison_results$coding_strategies) < 2) {
    return(list(
      distribution = NULL,
      overlap = NULL,
      sequence = NULL
    ))
  }

  # Distribution comparison plot
  distribution_plot <- function() {
    # Calculate total height needed
    n_plots <- length(comparison_results$coding_strategies)

    # Set up the plotting area with adjusted margins
    par(mfrow = c(n_plots, 1),
        # Adjust margins (bottom, left, top, right) to be smaller
        mar = c(3, 2, 2, 1),
        # Add outer margins (bottom, left, top, right)
        oma = c(2, 2, 1, 1))

    for (i in seq_along(comparison_results$coding_strategies)) {
      strategy <- comparison_results$coding_strategies[[i]]
      if (!is.null(strategy$coverage$distribution$frequencies)) {
        freqs <- strategy$coverage$distribution$frequencies
        bp <- barplot(freqs,
                      main = paste("Coder", i),
                      las = 2,           # Rotate labels
                      cex.names = 0.7,   # Reduce label size
                      cex.axis = 0.7,    # Reduce axis text size
                      col = "steelblue",
                      ylim = c(0, max(freqs) * 1.2)) # Add some space at top
        # Add value labels on top of bars
        text(x = bp, y = freqs, labels = freqs, pos = 3, cex = 0.6)
      }
    }
    title(main = "Code Distribution Comparison",
          outer = TRUE,
          line = 0)
    recordPlot()
  }

  # Code overlap patterns plot
  overlap_plot <- function() {
    n_plots <- length(comparison_results$coding_strategies)

    par(mfrow = c(n_plots, 1),
        mar = c(3, 2, 2, 1),
        oma = c(2, 2, 1, 1))

    for (i in seq_along(comparison_results$coding_strategies)) {
      strategy <- comparison_results$coding_strategies[[i]]
      if (!is.null(strategy$co_occurrences$combinations$frequencies)) {
        freqs <- strategy$co_occurrences$combinations$frequencies
        if (length(freqs) > 0) {
          bp <- barplot(freqs,
                        main = paste("Coder", i),
                        las = 2,
                        cex.names = 0.7,
                        cex.axis = 0.7,
                        col = "lightgreen",
                        ylim = c(0, max(freqs) * 1.2))
          text(x = bp, y = freqs, labels = freqs, pos = 3, cex = 0.6)
        } else {
          plot.new()
          title(main = paste("Coder", i, "- No code co-occurrences"))
        }
      }
    }
    title(main = "Code Co-occurrence Comparison",
          outer = TRUE,
          line = 0)
    recordPlot()
  }

  # Sequence patterns plot
  sequence_plot <- function() {
    n_plots <- length(comparison_results$coding_strategies)

    par(mfrow = c(n_plots, 1),
        mar = c(3, 2, 2, 1),
        oma = c(2, 2, 1, 1))

    for (i in seq_along(comparison_results$coding_strategies)) {
      strategy <- comparison_results$coding_strategies[[i]]
      if (!is.null(strategy$sequences$transitions) &&
          length(strategy$sequences$transitions) > 0) {
        # Convert transitions to table
        trans_table <- table(sapply(strategy$sequences$transitions,
                                    function(x) paste(x["from"], "->", x["to"])))
        if (length(trans_table) > 0) {
          bp <- barplot(trans_table,
                        main = paste("Coder", i),
                        las = 2,
                        cex.names = 0.7,
                        cex.axis = 0.7,
                        col = "salmon",
                        ylim = c(0, max(trans_table) * 1.2))
          text(x = bp, y = trans_table, labels = trans_table, pos = 3, cex = 0.6)
        } else {
          plot.new()
          title(main = paste("Coder", i, "- No code sequences"))
        }
      }
    }
    title(main = "Code Sequence Comparison",
          outer = TRUE,
          line = 0)
    recordPlot()
  }

  # Generate and return all plots
  tryCatch({
    list(
      distribution = distribution_plot(),
      overlap = overlap_plot(),
      sequence = sequence_plot()
    )
  }, error = function(e) {
    warning("Error generating plots: ", e$message)
    list(
      distribution = NULL,
      overlap = NULL,
      sequence = NULL
    )
  })
}

#' Plot code distribution visualization
#'
#' @description
#' Creates a barplot visualization showing the distribution of codes in the annotations.
#' The plot includes rotated labels for better readability and handles empty or NULL
#' input data gracefully.
#'
#' @param distribution List containing code distribution information:
#'   \itemize{
#'     \item frequencies: Named numeric vector containing code frequencies
#'   }
#' @param main Character string specifying the plot title
#' @param ... Additional arguments passed to barplot()
#'
#' @return Invisible NULL, called for side effect of creating plot
#'
#' @importFrom graphics barplot text par
#' @importFrom grDevices recordPlot
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' # Create sample distribution data
#' dist <- list(
#'   frequencies = c(
#'     "Code1" = 5,
#'     "Code2" = 3,
#'     "Code3" = 7
#'   )
#' )
#'
#' # Create basic plot
#' plot_code_distribution(dist)
#'
#' # Create plot with custom title
#' plot_code_distribution(dist, main = "Code Distribution")
#'
#' # Handle empty distribution
#' plot_code_distribution(list(frequencies = integer(0)))
#' }
plot_code_distribution <- function(distribution, main = "", ...) {
  if (is.null(distribution) || length(distribution$frequencies) == 0) {
    plot(0, 0, type = "n",
         main = main,
         xlab = "No distribution data available",
         ylab = "")
    return()
  }

  # Create barplot with rotated labels
  bp <- barplot(distribution$frequencies,
                main = main,
                xlab = "",
                ylab = "Frequency",
                las = 2,
                cex.names = 0.8,
                ...)

  # Add labels below
  text(x = bp,
       y = par("usr")[3] - 0.1,
       labels = names(distribution$frequencies),
       xpd = TRUE,
       srt = 45,
       adj = 1,
       cex = 0.7)
}

#' Add code to theme in hierarchy
#'
#' @description
#' Adds a new code to a specific theme in the code hierarchy. The code can be added
#' to the root level or nested within existing themes.
#'
#' @param node Root node of the hierarchy tree
#' @param code_name Character string specifying the name of the code to add
#' @param theme_path Character vector specifying the path to the target theme
#' @param description Optional character string providing a description of the code
#'
#' @return Updated node with new code added
#'
#' @examples
#' \dontrun{
#' # Create root node
#' root <- Node$new("Root")
#'
#' # Add code to root level
#' root <- add_code_to_theme(root, "Interview", character(0),
#'                          "Interview transcript marker")
#'
#' # Add code to specific theme
#' root <- add_code_to_theme(root, "Grounded Theory", c("Methods", "Qualitative"),
#'                          "Grounded theory methodology")
#' }
#'
#' @keywords internal
add_code_to_theme <- function(node, code_name, theme_path, description = "") {
  # If theme_path is empty, add to root
  if(length(theme_path) == 0) {
    new_code <- node$AddChild(code_name)
    new_code$description <- description
    new_code$type <- "code"
    new_code$created <- Sys.time()
    return(node)
  }

  # Navigate to the target theme
  current_node <- node
  for (theme in theme_path) {
    if (is.null(current_node$children[[theme]])) {
      stop(paste("Theme not found:", theme))
    }
    current_node <- current_node$children[[theme]]
  }

  # Add the code
  if (!is.null(current_node$children[[code_name]])) {
    stop("Code already exists in this theme")
  }

  new_code <- current_node$AddChild(code_name)
  new_code$description <- description
  new_code$type <- "code"
  new_code$created <- Sys.time()

  return(node)
}

#' Move item in code hierarchy
#'
#' @description
#' Moves a code or theme to a new location in the hierarchy while preserving its
#' properties and child nodes. Checks for circular references and maintains the
#' integrity of the hierarchy structure.
#'
#' @param node Root node of the hierarchy tree
#' @param item_path Character vector specifying the current path to the item
#' @param new_parent_path Character vector specifying the path to the new parent
#'
#' @return Updated node hierarchy with item moved to new location
#'
#' @examples
#' \dontrun{
#' # Create hierarchy
#' root <- Node$new("Root")
#' theme1 <- root$AddChild("Theme1")
#' theme2 <- root$AddChild("Theme2")
#'
#' # Move Theme1 to be a child of Theme2
#' root <- move_item(root, c("Theme1"), c("Theme2"))
#' }
#'
#' @keywords internal
move_item <- function(node, item_path, new_parent_path) {
  # Find the item to move
  item_node <- node$find(name = tail(item_path, 1),
                         filterFun = function(x) length(x$path) == length(item_path))

  if (is.null(item_node)) {
    stop("Item not found")
  }

  # Find the new parent
  new_parent <- node
  for (path_element in new_parent_path) {
    new_parent <- new_parent$children[[path_element]]
    if (is.null(new_parent)) {
      stop("New parent path not found")
    }
  }

  # Check for circular reference
  if (is_ancestor(item_node, new_parent)) {
    stop("Cannot move a node to its own descendant")
  }

  # Store item data
  item_data <- list(
    name = item_node$name,
    description = item_node$description,
    type = item_node$type,
    created = item_node$created,
    children = item_node$children
  )

  # Remove item from old location
  item_node$parent$RemoveChild(item_node$name)

  # Add item to new location
  new_item <- new_parent$AddChild(item_data$name)
  new_item$description <- item_data$description
  new_item$type <- item_data$type
  new_item$created <- item_data$created

  # Restore children if any
  if (length(item_data$children) > 0) {
    for (child in item_data$children) {
      restore_node(new_item, child)
    }
  }

  return(node)
}

#' Restore a node and its children in the hierarchy
#'
#' @description
#' Helper function to recursively restore a node and all its children
#' when moving items in the code hierarchy.
#'
#' @param parent Parent Node object where the node will be restored
#' @param node_data List containing node data to restore:
#'   \itemize{
#'     \item name: Character string of node name
#'     \item type: Character string specifying node type
#'     \item description: Character string of node description
#'     \item created: POSIXct creation timestamp
#'     \item children: List of child nodes
#'   }
#'
#' @return New Node object with restored data and children
#'
#' @importFrom data.tree Node
#'
#' @examples
#' \dontrun{
#' # Create parent node
#' parent <- Node$new("Parent")
#'
#' # Create node data
#' node_data <- list(
#'   name = "Child",
#'   type = "theme",
#'   description = "A child node",
#'   created = Sys.time(),
#'   children = list()
#' )
#'
#' # Restore node
#' new_node <- restore_node(parent, node_data)
#' }
#'
#' @keywords internal
restore_node <- function(parent, node_data) {
  new_node <- parent$AddChild(node_data$name)
  new_node$type <- node_data$type
  new_node$description <- node_data$description
  new_node$created <- node_data$created

  if (!is.null(node_data$children) && length(node_data$children) > 0) {
    for (child in node_data$children) {
      restore_node(new_node, child)
    }
  }

  return(new_node)
}

#' Check if one node is an ancestor of another
#'
#' @description
#' Helper function to check if a node is an ancestor of another node
#' in the code hierarchy, preventing circular references when moving items.
#'
#' @param potential_ancestor Node object to check as potential ancestor
#' @param node Node object to check ancestry against
#'
#' @return Logical indicating whether potential_ancestor is an ancestor of node
#'
#' @examples
#' \dontrun{
#' # Create hierarchy
#' root <- Node$new("Root")
#' child <- root$AddChild("Child")
#' grandchild <- child$AddChild("Grandchild")
#'
#' # Check ancestry
#' is_ancestor(root, grandchild)  # TRUE
#' is_ancestor(grandchild, root)  # FALSE
#' }
#'
#' @keywords internal
is_ancestor <- function(potential_ancestor, node) {
  current <- node
  while (!is.null(current$parent)) {
    if (identical(current$parent, potential_ancestor)) {
      return(TRUE)
    }
    current <- current$parent
  }
  return(FALSE)
}

#' Export code hierarchy to JSON format
#'
#' @description
#' Converts the code hierarchy tree structure into a JSON string representation
#' that can be saved or transmitted while preserving all node properties and
#' relationships.
#'
#' @param node Root node of the hierarchy tree
#'
#' @return JSON string representation of the hierarchy
#'
#' @importFrom jsonlite toJSON
#'
#' @examples
#' \dontrun{
#' # Create sample hierarchy
#' root <- Node$new("Root")
#' theme1 <- root$AddChild("Theme1")
#' theme1$AddChild("Code1")
#'
#' # Export to JSON
#' json_string <- export_hierarchy(root)
#' writeLines(json_string, "hierarchy.json")
#' }
#'
#' @keywords internal
export_hierarchy <- function(node) {
  hierarchy_list <- as.list(node)
  toJSON(hierarchy_list, pretty = TRUE, auto_unbox = TRUE)
}

#' Import code hierarchy from JSON format
#'
#' @description
#' Reconstructs a code hierarchy tree structure from its JSON string representation,
#' restoring all node properties and relationships.
#'
#' @param json_string JSON string representation of the hierarchy
#'
#' @return Node object representing the reconstructed hierarchy
#'
#' @importFrom jsonlite fromJSON
#' @importFrom data.tree as.Node
#'
#' @examples
#' \dontrun{
#' # Read JSON file
#' json_string <- readLines("hierarchy.json")
#'
#' # Import hierarchy
#' root <- import_hierarchy(json_string)
#' print(root)
#' }
#'
#' @keywords internal
import_hierarchy <- function(json_string) {
  hierarchy_list <- fromJSON(json_string)
  as.Node(hierarchy_list)
}

#' Generate visual representation of code hierarchy
#'
#' @description
#' Creates an HTML tree visualization of the code hierarchy with proper
#' indentation, icons, and interactive elements.
#'
#' @param node Root node of hierarchy tree with attributes:
#'   \itemize{
#'     \item name: character, node name
#'     \item type: character, "theme" or "code"
#'     \item description: character, node description
#'     \item children: list of child nodes
#'   }
#'
#' @return Character string containing HTML markup for tree visualization
#'
#' @examples
#' \dontrun{
#' root <- Node$new("Root")
#' theme1 <- root$AddChild("Theme1")
#' theme1$type <- "theme"
#' code1 <- theme1$AddChild("Code1")
#' code1$type <- "code"
#' html <- visualize_hierarchy(root)
#' }
#' @keywords internal
visualize_hierarchy <- function(node) {
  if (is.null(node)) return("Empty hierarchy")

  print_tree <- function(node, indent = 0) {
    if (is.null(node)) return(character(0))

    # Get node type symbol using Unicode escape sequences for emoji
    symbol <- if (!is.null(node$type) && node$type == "theme")
      "\U0001F4C2" else "\U0001F4C4"  # Folder emoji: 📂, File emoji: 📄

    # Create the line for this node with proper data attributes and classes
    name_display <- if (!is.null(node$type)) {
      class_name <- if (node$type == "theme") "theme-item" else "code-item"
      sprintf('<span class="%s" data-name="%s" data-type="%s">%s</span>',
              class_name, node$name, node$type, node$name)
    } else {
      sprintf('<span>%s</span>', node$name)
    }

    # Add description preview if available
    description_preview <- if (!is.null(node$description) && node$description != "") {
      sprintf(' - <span class="description-preview">%s</span>',
              substr(node$description, 1, 30))
    } else {
      ""
    }

    line <- paste0(
      paste(rep("  ", indent), collapse = ""),
      symbol, " ",
      name_display,
      description_preview
    )

    # Start with this node's line
    lines <- line

    # Add all children's lines
    if (!is.null(node$children) && length(node$children) > 0) {
      sorted_children <- sort(names(node$children))
      child_lines <- lapply(sorted_children, function(child_name) {
        print_tree(node$children[[child_name]], indent + 1)
      })
      lines <- c(lines, unlist(child_lines))
    }

    return(lines)
  }

  # Combine all lines and wrap in a div for proper styling
  paste(
    '<div class="hierarchy-container">',
    paste(print_tree(node), collapse = "\n"),
    '</div>'
  )
}

# Helper function to find a node by name
find_node_by_name <- function(node, target_name) {
  if (is.null(node) || is.null(target_name)) return(NULL)

  if (!is.null(node$name) && node$name == target_name) {
    return(node)
  }

  if (!is.null(node$children)) {
    for (child in node$children) {
      result <- find_node_by_name(child, target_name)
      if (!is.null(result)) {
        return(result)
      }
    }
  }

  return(NULL)
}

#' Calculate hierarchy statistics
#'
#' @description
#' Calculates various statistics about the code hierarchy including the total number
#' of themes and codes, maximum depth, and distribution of codes across themes.
#'
#' @param node Root node of the hierarchy tree
#'
#' @return A list containing hierarchy statistics:
#'   \itemize{
#'     \item total_themes: Total number of themes in the hierarchy
#'     \item total_codes: Total number of codes in the hierarchy
#'     \item max_depth: Maximum depth of the hierarchy tree
#'     \item codes_per_theme: List showing number of codes in each theme
#'     \item average_codes_per_theme: Average number of codes per theme
#'   }
#'
#' @examples
#' \dontrun{
#' # Create sample hierarchy
#' root <- Node$new("Root")
#' theme1 <- root$AddChild("Theme1")
#' theme1$AddChild("Code1")
#' theme1$AddChild("Code2")
#'
#' # Calculate statistics
#' stats <- calculate_hierarchy_stats(root)
#' print(stats$total_codes)  # Should print 2
#' }
#'
#' @keywords internal
calculate_hierarchy_stats <- function(node) {
  if (is.null(node)) {
    return(list(
      total_themes = 0,
      total_codes = 0,
      max_depth = 0,
      codes_per_theme = list(),
      average_codes_per_theme = 0
    ))
  }

  n_themes <- 0
  n_codes <- 0
  max_depth <- 0
  codes_per_theme <- list()

  traverse_node <- function(node, depth = 0) {
    if (is.null(node)) return()

    # Check if node type exists and is a character
    node_type <- if (!is.null(node$type) && is.character(node$type)) node$type else "unknown"

    if (node_type == "theme") {
      n_themes <<- n_themes + 1
      # Count codes that are direct children of this theme
      if (!is.null(node$name) && !is.null(node$children)) {
        codes_in_theme <- sum(sapply(node$children, function(x) {
          child_type <- if (!is.null(x$type) && is.character(x$type)) x$type else "unknown"
          child_type == "code"
        }))
        codes_per_theme[[node$name]] <<- codes_in_theme
      }
    } else if (node_type == "code") {
      n_codes <<- n_codes + 1
    }

    max_depth <<- max(max_depth, depth)

    if (!is.null(node$children) && length(node$children) > 0) {
      lapply(node$children, function(child) traverse_node(child, depth + 1))
    }
  }

  # Start traversal from root node
  traverse_node(node)

  # Calculate average codes per theme
  avg_codes <- if (n_themes > 0) n_codes / n_themes else 0

  # Return statistics
  list(
    total_themes = n_themes,
    total_codes = n_codes,
    max_depth = max_depth,
    codes_per_theme = codes_per_theme,
    average_codes_per_theme = avg_codes
  )
}

#' Find clusters of annotations in text
#'
#' @description
#' Identifies clusters of annotations that are close together in the text,
#' helping to identify dense coding regions.
#'
#' @param annotations Data frame containing sorted text annotations with columns:
#'   \itemize{
#'     \item start: numeric, starting position of annotation
#'     \item end: numeric, ending position of annotation
#'     \item code: character, code applied to the annotation
#'   }
#'
#' @return List of annotation clusters, where each cluster contains annotations
#'         that are within a specified distance of each other
#'
#' @examples
#' \dontrun{
#' annotations <- data.frame(
#'   start = c(1, 5, 100),
#'   end = c(3, 8, 105),
#'   code = c("code1", "code2", "code3")
#' )
#' clusters <- find_annotation_clusters(annotations)
#' }
#'
#' @keywords internal
find_annotation_clusters <- function(annotations) {
  # Sort annotations by position
  sorted_anns <- annotations[order(annotations$start), ]

  # Find clusters where annotations are close together
  clusters <- list()
  current_cluster <- list()

  for (i in 1:(nrow(sorted_anns) - 1)) {
    if (nrow(sorted_anns) == 0) break

    current <- sorted_anns[i, ]
    next_ann <- sorted_anns[i + 1, ]

    # Add current annotation to cluster
    current_cluster <- append(current_cluster, list(current))

    # If gap to next annotation is large, start new cluster
    if ((next_ann$start - current$end) > 50) {  # Adjust threshold as needed
      if (length(current_cluster) > 0) {
        clusters <- append(clusters, list(current_cluster))
      }
      current_cluster <- list()
    }
  }

  # Add last cluster if exists
  if (length(current_cluster) > 0) {
    clusters <- append(clusters, list(current_cluster))
  }

  return(clusters)
}

analyze_coding_density <- function(annotations) {
  if (nrow(annotations) == 0) return(list())

  # Calculate density metrics
  total_length <- max(annotations$end) - min(annotations$start)
  total_coded <- sum(annotations$end - annotations$start + 1)
  density <- total_coded / total_length

  # Identify dense regions
  dense_regions <- list()
  if (nrow(annotations) > 1) {
    for (i in 1:(nrow(annotations)-1)) {
      if ((annotations$start[i+1] - annotations$end[i]) < 20) {  # Adjust threshold
        dense_regions <- append(dense_regions,
                                list(c(annotations$start[i], annotations$end[i+1])))
      }
    }
  }

  return(list(
    overall_density = density,
    dense_regions = dense_regions
  ))
}

analyze_code_distribution <- function(annotations) {
  if (nrow(annotations) == 0) return(list())

  # Calculate distribution of codes across the text
  code_counts <- table(annotations$code)
  code_positions <- tapply(annotations$start, annotations$code,
                           function(x) list(positions = x))

  return(list(
    frequencies = code_counts,
    positions = code_positions
  ))
}

#' Analyze context around code applications
#'
#' @description
#' Examines the surrounding context where codes are applied by looking at
#' preceding and following annotations to understand code relationships.
#'
#' @param code_anns Data frame containing annotations for specific code:
#'   \itemize{
#'     \item start: numeric, starting position
#'     \item end: numeric, ending position
#'     \item code: character, code identifier
#'   }
#' @param all_anns Data frame containing all annotations in the text
#'
#' @return List of contexts for each code instance:
#'   \itemize{
#'     \item before: Preceding annotation if exists
#'     \item after: Following annotation if exists
#'   }
#'
#' @examples
#' \dontrun{
#' all_annotations <- data.frame(
#'   start = c(1, 10, 20, 30),
#'   end = c(5, 15, 25, 35),
#'   code = c("code1", "code2", "code1", "code3")
#' )
#' code1_anns <- all_annotations[all_annotations$code == "code1", ]
#' contexts <- analyze_code_context(code1_anns, all_annotations)
#' }
#' @keywords internal
analyze_code_context <- function(code_anns, all_anns) {
  if (nrow(code_anns) == 0) return(list())

  contexts <- lapply(1:nrow(code_anns), function(i) {
    current <- code_anns[i, ]

    # Find preceding and following annotations
    preceding <- all_anns[all_anns$end < current$start, ]
    following <- all_anns[all_anns$start > current$end, ]

    # Get closest annotations
    before <- if (nrow(preceding) > 0) {
      preceding[which.max(preceding$end), ]
    } else NULL

    after <- if (nrow(following) > 0) {
      following[which.min(following$start), ]
    } else NULL

    list(
      before = before,
      after = after
    )
  })

  return(contexts)
}

#' Analyze memo usage patterns
#'
#' @description
#' Examines how memos are used with codes by analyzing memo frequency,
#' content, and patterns in memo application across code instances.
#'
#' @param code_anns Data frame containing code annotations with columns:
#'   \itemize{
#'     \item memo: character, memo text associated with annotation
#'     \item code: character, code identifier
#'   }
#'
#' @return List containing:
#'   \itemize{
#'     \item memo_frequency: Proportion of annotations with memos
#'     \item has_memos: Logical vector indicating memo presence
#'   }
#'
#' @examples
#' \dontrun{
#' annotations <- data.frame(
#'   code = c("code1", "code1", "code2"),
#'   memo = c("note 1", "", "note 2")
#' )
#' patterns <- analyze_memo_patterns(annotations)
#' }
#' @keywords internal
analyze_memo_patterns <- function(code_anns) {
  if (nrow(code_anns) == 0) return(list())

  # Extract and analyze memo patterns
  has_memo <- !is.na(code_anns$memo) & code_anns$memo != ""
  memo_count <- sum(has_memo)

  return(list(
    memo_frequency = memo_count / nrow(code_anns),
    has_memos = has_memo
  ))
}

#' Find overlapping code annotations
#'
#' @description
#' Identifies pairs of annotations that overlap in the text and returns their
#' intersection points and associated codes.
#'
#' @param annotations A data frame containing text annotations with columns:
#'   \itemize{
#'     \item start: numeric, starting position of annotation
#'     \item end: numeric, ending position of annotation
#'     \item code: character, code applied to the annotation
#'   }
#'
#' @return A list of overlapping code pairs, each containing:
#'   \itemize{
#'     \item code1: first code in the overlap
#'     \item code2: second code in the overlap
#'     \item overlap_start: starting position of overlap
#'     \item overlap_end: ending position of overlap
#'   }
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' annotations <- data.frame(
#'   start = c(1, 5, 10),
#'   end = c(8, 15, 20),
#'   code = c("code1", "code2", "code1")
#' )
#' overlaps <- find_overlapping_codes(annotations)
#' }
find_overlapping_codes <- function(annotations) {
  if (!is.data.frame(annotations) || nrow(annotations) <= 1) {
    return(list())
  }

  # Ensure proper types and handle NAs
  annotations$start <- as.numeric(as.character(annotations$start))
  annotations$end <- as.numeric(as.character(annotations$end))
  annotations$code <- as.character(annotations$code)

  valid_rows <- !is.na(annotations$start) &
    !is.na(annotations$end) &
    !is.na(annotations$code) &
    annotations$start <= annotations$end

  annotations <- annotations[valid_rows, ]

  if (nrow(annotations) <= 1) {
    return(list())
  }

  overlaps <- list()
  for (i in 1:(nrow(annotations)-1)) {
    for (j in (i+1):nrow(annotations)) {
      # Check for overlap
      if (annotations$start[i] <= annotations$end[j] &&
          annotations$end[i] >= annotations$start[j]) {
        overlaps <- c(overlaps, list(list(
          code1 = annotations$code[i],
          code2 = annotations$code[j],
          overlap_start = max(annotations$start[i], annotations$start[j]),
          overlap_end = min(annotations$end[i], annotations$end[j])
        )))
      }
    }
  }

  return(overlaps)
}

analyze_code_combinations <- function(overlaps) {
  if (length(overlaps) == 0) return(list())

  # Count frequency of code pairs
  combinations <- table(sapply(overlaps, function(x) {
    paste(sort(c(x$code1, x$code2)), collapse = "-")
  }))

  return(list(
    frequencies = combinations
  ))
}

analyze_overlap_characteristics <- function(overlaps) {
  if (length(overlaps) == 0) return(list())

  # Calculate overlap lengths
  lengths <- sapply(overlaps, function(x) {
    x$overlap_end - x$overlap_start + 1
  })

  return(list(
    avg_length = mean(lengths),
    total_overlaps = length(overlaps)
  ))
}

#' Find transitions between codes
#'
#' @description
#' Identifies and analyzes transitions between consecutive code applications
#' to understand coding sequence patterns.
#'
#' @param annotations Data frame of sorted annotations with columns:
#'   \itemize{
#'     \item start: numeric, starting position
#'     \item end: numeric, ending position
#'     \item code: character, code identifier
#'   }
#'
#' @return List of code transitions, each containing:
#'   \itemize{
#'     \item from: Source code
#'     \item to: Target code
#'   }
#'
#' @examples
#' \dontrun{
#' annotations <- data.frame(
#'   start = c(1, 10, 20),
#'   end = c(5, 15, 25),
#'   code = c("code1", "code2", "code3")
#' )
#' transitions <- find_code_transitions(annotations)
#' }
#' @keywords internal
find_code_transitions <- function(annotations) {
  if (nrow(annotations) <= 1) return(list())

  # Find sequences of codes
  transitions <- list()
  for (i in 1:(nrow(annotations)-1)) {
    transitions <- append(transitions,
                          list(c(from = annotations$code[i],
                                 to = annotations$code[i+1])))
  }

  return(transitions)
}

#' Find repeated patterns in code sequences
#'
#' @description
#' Identifies repeating patterns of 2-3 codes in sequence to uncover recurring
#' coding structures.
#'
#' @param annotations Data frame of sorted annotations with columns:
#'   \itemize{
#'     \item code: character, code identifier
#'   }
#'
#' @return Named list of pattern frequencies where:
#'   \itemize{
#'     \item names: Code patterns (e.g. "code1-code2")
#'     \item values: Number of occurrences
#'   }
#'
#' @examples
#' \dontrun{
#' annotations <- data.frame(
#'   code = c("A", "B", "A", "B", "C")
#' )
#' patterns <- find_repeated_sequences(annotations)
#' # Returns list showing A-B pattern occurs twice
#' }
#' @keywords internal
find_repeated_sequences <- function(annotations) {
  if (nrow(annotations) <= 1) return(list())

  # Look for repeated patterns in code sequences
  code_sequence <- annotations$code

  # Look for patterns of length 2-3
  patterns <- list()
  for (len in 2:min(3, length(code_sequence))) {
    for (i in 1:(length(code_sequence) - len + 1)) {
      pattern <- code_sequence[i:(i+len-1)]
      pattern_str <- paste(pattern, collapse = "-")
      patterns[[pattern_str]] <- sum(sapply(
        seq_along(code_sequence),
        function(j) {
          if (j + len - 1 > length(code_sequence)) return(FALSE)
          all(code_sequence[j:(j+len-1)] == pattern)
        }
      ))
    }
  }

  # Remove patterns that only occur once
  patterns <- patterns[patterns > 1]

  return(patterns)
}

#' Compare coverage patterns between coding strategies
#'
#' @description
#' Analyzes and compares the coverage patterns between different coding strategies,
#' including total codes used and unique code counts.
#'
#' @param coding_strategies List of coding strategies, where each strategy contains:
#'   \itemize{
#'     \item coverage: List containing distribution information
#'     \item frequencies: Table of code frequencies
#'   }
#'
#' @return List containing:
#'   \itemize{
#'     \item total_codes_range: Numeric vector with min and max total codes
#'     \item unique_codes_range: Numeric vector with min and max unique codes
#'   }
#'
#' @examples
#' \dontrun{
#' strategy1 <- list(
#'   coverage = list(
#'     distribution = list(
#'       frequencies = table(c("code1", "code1", "code2"))
#'     )
#'   )
#' )
#' strategy2 <- list(
#'   coverage = list(
#'     distribution = list(
#'       frequencies = table(c("code2", "code3", "code3"))
#'     )
#'   )
#' )
#' results <- compare_coverage(list(strategy1, strategy2))
#' }
#'
#' @keywords internal
compare_coverage <- function(coding_strategies) {
  tryCatch({
    coverage_stats <- lapply(coding_strategies, function(strategy) {
      freqs <- strategy$coverage$distribution$frequencies
      list(
        total_codes = sum(freqs),
        unique_codes = length(freqs),
        code_frequencies = freqs
      )
    })

    total_codes <- sapply(coverage_stats, `[[`, "total_codes")
    unique_codes <- sapply(coverage_stats, `[[`, "unique_codes")

    return(list(
      total_codes_range = if(length(total_codes) > 0) range(total_codes) else c(0, 0),
      unique_codes_range = if(length(unique_codes) > 0) range(unique_codes) else c(0, 0)
    ))
  }, error = function(e) {
    return(list(
      total_codes_range = c(0, 0),
      unique_codes_range = c(0, 0)
    ))
  })
}

#' Compare code application patterns between coders
#'
#' @description
#' Analyzes and compares how different coders apply codes by examining code segment
#' lengths and memo usage patterns across coding strategies.
#'
#' @param patterns_list List of coding patterns from different coders, where each
#'        pattern contains:
#'   \itemize{
#'     \item typical_length: numeric, average length of code segments
#'     \item memo_patterns: list containing memo usage statistics
#'   }
#'
#' @return List containing:
#'   \itemize{
#'     \item length_variation: Character string describing variation in code segment lengths
#'     \item memo_usage_summary: Character string describing differences in memo usage
#'   }
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' patterns1 <- list(
#'   code1 = list(typical_length = 10,
#'                memo_patterns = list(memo_frequency = 0.5))
#' )
#' patterns2 <- list(
#'   code1 = list(typical_length = 15,
#'                memo_patterns = list(memo_frequency = 0.3))
#' )
#' results <- compare_code_patterns(list(patterns1, patterns2))
#' }
compare_code_patterns <- function(patterns_list) {
  if (length(patterns_list) < 2) return("Need at least two sets for comparison")

  # Compare code application patterns
  lengths <- lapply(patterns_list, function(x) {
    sapply(x, function(p) p$typical_length)
  })

  # Compare memo usage
  memo_usage <- lapply(patterns_list, function(x) {
    sapply(x, function(p) p$memo_patterns$memo_frequency)
  })

  list(
    length_variation = "Variation in code segment lengths across coders",
    memo_usage_summary = "Differences in memo usage patterns"
  )
}

#' Compare code co-occurrence patterns between coders
#'
#' @description
#' Analyzes how different coders overlap in their code applications by comparing
#' the frequency and patterns of code co-occurrences.
#'
#' @param overlaps_list List of overlap patterns from different coders, where each
#'        entry contains:
#'   \itemize{
#'     \item combinations: List containing frequency table of code co-occurrences
#'   }
#'
#' @return List containing:
#'   \itemize{
#'     \item overlap_variation: Numeric value indicating range of overlap counts
#'     \item summary: Character string describing variation in overlapping pairs
#'   }
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' overlaps1 <- list(combinations = list(frequencies = c("A-B" = 2)))
#' overlaps2 <- list(combinations = list(frequencies = c("A-B" = 1, "B-C" = 3)))
#' results <- compare_co_occurrences(list(overlaps1, overlaps2))
#' }
compare_co_occurrences <- function(overlaps_list) {
  if (length(overlaps_list) < 2) return("Need at least two sets for comparison")

  # Compare overlap patterns
  overlap_counts <- sapply(overlaps_list, function(x) {
    length(x$combinations$frequencies)
  })

  list(
    overlap_variation = diff(range(overlap_counts)),
    summary = sprintf("Number of overlapping code pairs varies from %d to %d",
                      min(overlap_counts), max(overlap_counts))
  )
}

#' Compare code sequence patterns between coders
#'
#' @description
#' Analyzes how different coders sequence their codes by comparing the patterns
#' and frequency of code transitions.
#'
#' @param sequences_list List of sequence patterns from different coders, where each
#'        entry contains:
#'   \itemize{
#'     \item transitions: List of code transitions observed in the text
#'   }
#'
#' @return List containing:
#'   \itemize{
#'     \item sequence_variation: Numeric value indicating range of transition counts
#'     \item summary: Character string describing variation in code transitions
#'   }
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' seq1 <- list(transitions = list(c("A", "B"), c("B", "C")))
#' seq2 <- list(transitions = list(c("A", "B")))
#' results <- compare_sequences(list(seq1, seq2))
#' }
compare_sequences <- function(sequences_list) {
  if (length(sequences_list) < 2) return("Need at least two sets for comparison")

  # Compare sequence patterns
  transition_counts <- sapply(sequences_list, function(x) {
    length(x$transitions)
  })

  list(
    sequence_variation = diff(range(transition_counts)),
    summary = sprintf("Number of code transitions varies from %d to %d",
                      min(transition_counts), max(transition_counts))
  )
}

#' Format coverage difference analysis results
#'
#' @description
#' Formats the results of coverage difference analysis into a human-readable
#' string, handling both character and list inputs appropriately.
#'
#' @param differences Either a character string containing direct analysis results
#'        or a list containing:
#'   \itemize{
#'     \item density_summary: Character string summarizing density differences
#'   }
#'
#' @return Character string containing formatted coverage analysis results
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' # With character input
#' format_coverage_differences("Direct analysis result")
#'
#' # With list input
#' differences <- list(density_summary = "Coverage density varies from 0.2 to 0.8")
#' format_coverage_differences(differences)
#' }
format_coverage_differences <- function(differences) {
  if (is.character(differences)) return(differences)
  if (is.list(differences) && !is.null(differences$density_summary)) {
    return(differences$density_summary)
  }
  return("Coverage analysis completed")
}

#' Format code difference analysis results
#'
#' @description
#' Formats the results of code difference analysis into a human-readable
#' string, handling both character and list inputs appropriately.
#'
#' @param differences Either a character string containing direct analysis results
#'        or a list containing:
#'   \itemize{
#'     \item pattern_summary: Character string summarizing code pattern differences
#'   }
#'
#' @return Character string containing formatted code analysis results
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' # With character input
#' format_code_differences("Direct analysis result")
#'
#' # With list input
#' differences <- list(pattern_summary = "Analyzed 5 coding patterns")
#' format_code_differences(differences)
#' }
format_code_differences <- function(differences) {
  if (is.character(differences)) return(differences)
  if (is.list(differences) && !is.null(differences$pattern_summary)) {
    return(differences$pattern_summary)
  }
  return("Code pattern analysis completed")
}

#' Format overlap difference analysis results
#'
#' @description
#' Formats the results of overlap difference analysis into a human-readable
#' string, processing both character and complex input types.
#'
#' @param differences Either a character string containing direct analysis results
#'        or a complex analysis object
#'
#' @return Character string containing formatted overlap analysis results
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' # With character input
#' format_overlap_differences("Direct analysis result")
#'
#' # With complex input
#' format_overlap_differences(list(some_results = "data"))
#' }
format_overlap_differences <- function(differences) {
  if (is.character(differences)) return(differences)
  return("Overlap analysis completed")
}

#' Format sequence difference analysis results
#'
#' @description
#' Formats the results of sequence difference analysis into a human-readable
#' string, processing both character and complex input types.
#'
#' @param differences Either a character string containing direct analysis results
#'        or a complex analysis object
#'
#' @return Character string containing formatted sequence analysis results
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' # With character input
#' format_sequence_differences("Direct analysis result")
#'
#' # With complex input
#' format_sequence_differences(list(some_results = "data"))
#' }
format_sequence_differences <- function(differences) {
  if (is.character(differences)) return(differences)
  return("Sequence analysis completed")
}

#' Plot code overlap patterns
#'
#' @description
#' Creates a barplot visualization of code overlap patterns, showing the frequency
#' of different code co-occurrences with rotated labels for better readability.
#'
#' @param overlaps List containing overlap information:
#'   \itemize{
#'     \item combinations: List containing frequencies of code co-occurrences
#'   }
#' @param main Character string for plot title
#' @param ... Additional arguments passed to barplot()
#'
#' @return Invisible NULL, called for side effect of creating plot
#'
#' @importFrom graphics barplot text par
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' overlaps <- list(
#'   combinations = list(
#'     frequencies = c("A-B" = 2, "B-C" = 3)
#'   )
#' )
#' plot_overlap_patterns(overlaps, main = "Code Overlaps")
#' }
plot_overlap_patterns <- function(overlaps, main = "", ...) {
  if (is.null(overlaps) || length(overlaps$combinations$frequencies) == 0) {
    plot(0, 0, type = "n",
         main = main,
         xlab = "No overlaps available",
         ylab = "")
    return()
  }

  # Create barplot with rotated labels
  bp <- barplot(overlaps$combinations$frequencies,
                main = main,
                xlab = "",
                ylab = "Overlap Count",
                las = 2,
                cex.names = 0.8,
                ...)

  # Add labels below
  text(x = bp,
       y = par("usr")[3] - 0.1,
       labels = names(overlaps$combinations$frequencies),
       xpd = TRUE,
       srt = 45,
       adj = 1,
       cex = 0.7)
}

#' Plot code sequence patterns
#'
#' @description
#' Creates a barplot visualization of code sequence patterns, showing the frequency
#' of different code transitions with rotated labels for better readability.
#'
#' @param sequences List containing sequence information:
#'   \itemize{
#'     \item transitions: List of code transitions
#'   }
#' @param main Character string for plot title
#' @param ... Additional arguments passed to barplot()
#'
#' @return Invisible NULL, called for side effect of creating plot
#'
#' @importFrom graphics barplot text par
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' sequences <- list(
#'   transitions = list(
#'     c(from = "A", to = "B"),
#'     c(from = "B", to = "C")
#'   )
#' )
#' plot_sequence_patterns(sequences, main = "Code Sequences")
#' }
plot_sequence_patterns <- function(sequences, main = "", ...) {
  if (is.null(sequences) || length(sequences$transitions) == 0) {
    plot(0, 0, type = "n",
         main = main,
         xlab = "No sequences available",
         ylab = "")
    return()
  }

  # Convert transitions to table
  trans_table <- table(sapply(sequences$transitions,
                              function(x) paste(x[1], "->", x[2])))

  # Create barplot with adjusted margins
  bp <- barplot(trans_table,
                main = main,
                xlab = "",
                ylab = "Frequency",
                las = 2,
                cex.names = 0.8,
                ...)

  # Add labels below
  text(x = bp,
       y = par("usr")[3] - 0.1,
       labels = names(trans_table),
       xpd = TRUE,
       srt = 45,
       adj = 1,
       cex = 0.7)
}

#' JavaScript code for handling text selection and UI interactions
#'
#' @description
#' This internal JavaScript code provides functionality for text selection,
#' popup menus, and interactive UI elements in the text annotation interface.
#' It manages mouse events for text selection, highlighting, and code application.
#'
#' @details
#' The JavaScript code implements the following functionality:
#' \itemize{
#'   \item Creation and management of popup menus for code operations
#'   \item Text selection handling with mouse events
#'   \item Highlighting of selected text
#'   \item Communication with Shiny server through custom message handlers
#'   \item Event handling for code replacement, renaming, and deletion
#' }
#'
#' @section Event Handlers:
#' \itemize{
#'   \item Text selection events (mousedown, mousemove, mouseup)
#'   \item Popup menu events for code operations
#'   \item Custom Shiny message handlers for selection state
#' }
#'
#' @note
#' This is an internal function used by the textAnnotatoR package and
#' should not be called directly by users.
#'
#' @keywords internal
#' @name addJS
#' @format A character string containing JavaScript code
NULL
addJS <- "
$(document).ready(function() {
  var popupMenu = $('<div id=\"popupMenu\" style=\"position: absolute; display: none; background-color: white; border: 1px solid black; padding: 5px;\"></div>');
  $('body').append(popupMenu);

  $(document).on('click', '.code-display', function(e) {
    var code = $(this).data('code');
    var start = $(this).data('start');
    var end = $(this).data('end');
    popupMenu.html(
      '<button id=\"replaceCode\">Replace Code</button><br>' +
      '<button id=\"renameCode\">Rename Code</button><br>' +
      '<button id=\"deleteCode\">Delete Code</button>'
    );
    popupMenu.css({
      left: e.pageX + 'px',
      top: e.pageY + 'px'
    }).show();

    $('#replaceCode').click(function() {
      Shiny.setInputValue('replace_code', {code: code, start: start, end: end});
      popupMenu.hide();
    });

    $('#renameCode').click(function() {
      Shiny.setInputValue('rename_code', {code: code, start: start, end: end});
      popupMenu.hide();
    });

    $('#deleteCode').click(function() {
      Shiny.setInputValue('delete_code', {code: code, start: start, end: end});
      popupMenu.hide();
    });
  });

  $(document).on('click', function(e) {
    if (!$(e.target).closest('#popupMenu, .code-display').length) {
      popupMenu.hide();
    }
  });
});
"
