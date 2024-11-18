#' Get All Code Names from Tree
#'
#' Recursively extracts all code names from a hierarchical code tree structure.
#' Traverses the tree depth-first and returns a vector of all code names,
#' including both leaf nodes and internal nodes.
#'
#' @param node Root node of the code tree. Must be a data.tree Node object with
#'   the following structure:
#'   \itemize{
#'     \item name: Character string representing the code name
#'     \item children: List of child nodes (if any)
#'   }
#' @return Character vector containing all code names in the tree
#' @examples
#' \dontrun{
#' # Create a sample code tree using data.tree
#' library(data.tree)
#' root <- Node$new("Root")
#' child1 <- root$AddChild("Code1")
#' child2 <- root$AddChild("Code2")
#' child1$AddChild("Code1.1")
#' child1$AddChild("Code1.2")
#'
#' # Get all code names
#' codes <- get_code_names(root)
#' print(codes)  # c("Root", "Code1", "Code1.1", "Code1.2", "Code2")
#' }
#' @importFrom data.tree isLeaf
#' @keywords internal
get_code_names <- function(node) {
  if (node$isLeaf) {
    return(node$name)
  } else {
    return(c(node$name, unlist(lapply(node$children, get_code_names))))
  }
}


#' Get Code Paths in Tree
#'
#' Recursively extracts all full paths from a hierarchical code tree structure.
#' Each path represents the complete hierarchy from root to the given node,
#' with levels separated by forward slashes.
#'
#' @param node Node in the code tree. Must be a data.tree Node object with
#'   the following structure:
#'   \itemize{
#'     \item name: Character string representing the code name
#'     \item path: Vector of node names from root to current node
#'     \item children: List of child nodes (if any)
#'   }
#' @return Character vector of full paths for all codes, excluding the root node
#' @examples
#' \dontrun{
#' # Create a sample code tree using data.tree
#' library(data.tree)
#' root <- Node$new("Categories")
#' methods <- root$AddChild("Methods")
#' quant <- methods$AddChild("Quantitative")
#' qual <- methods$AddChild("Qualitative")
#' quant$AddChild("Survey")
#' qual$AddChild("Interview")
#'
#' # Get all code paths
#' paths <- get_code_paths(root)
#' print(paths)
#' # Returns:
#' # c("Categories/Methods",
#' #   "Categories/Methods/Quantitative",
#' #   "Categories/Methods/Quantitative/Survey",
#' #   "Categories/Methods/Qualitative",
#' #   "Categories/Methods/Qualitative/Interview")
#' }
#' @importFrom data.tree isRoot
#' @keywords internal
get_code_paths <- function(node) {
  if (node$isRoot) {
    return(character(0))
  } else {
    return(c(paste(node$path, collapse = "/"), unlist(lapply(node$children, get_code_paths))))
  }
}
