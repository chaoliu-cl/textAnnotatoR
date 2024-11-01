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

    # Create the line for this node with proper data attributes for themes
    name_display <- if (!is.null(node$type) && node$type == "theme") {
      sprintf('<span class="theme-item" data-theme-name="%s">%s</span>',
              node$name, node$name)
    } else {
      sprintf('<span class="code-item">%s</span>', node$name)
    }

    line <- paste0(
      paste(rep("  ", indent), collapse = ""),
      symbol, " ",
      name_display,
      if (!is.null(node$description) && node$description != "")
        paste0(" - ", substr(node$description, 1, 30), "...") else ""
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

  paste(print_tree(node), collapse = "\n")
}
