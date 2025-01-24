#' Plot Stern-Brocot Tree
#'
#' @param path A binary string (e.g., "1010") representing the traversed nodes.
#' @return A plot of the Stern-Brocot tree with the traversed nodes highlighted.
#' @importFrom igraph make_empty_graph vertices edges neighbors layout_as_tree plot
#' @examples
#' plot_stern_brocot_tree("1010")
#' @export
plot_stern_brocot_tree <- function(path) {
  depth <- nchar(path)  # Determine the depth of the tree

  # Initialize the graph with the root node
  g <- igraph::make_empty_graph() + igraph::vertices("1/1")
  fractions <- list("1/1" = c(1, 1))  # Store numerators and denominators

  for (d in 1:depth) {
    new_fractions <- list()
    for (frac in names(fractions)) {
      num <- fractions[[frac]][1]
      den <- fractions[[frac]][2]

      # Generate left and right children
      left_num <- num
      left_den <- den + num
      right_num <- num + den
      right_den <- den

      left <- paste0(left_num, "/", left_den)
      right <- paste0(right_num, "/", right_den)

      # Add vertices and edges
      if (!left %in% igraph::V(g)$name) {
        g <- g + igraph::vertices(left)
      }
      if (!right %in% igraph::V(g)$name) {
        g <- g + igraph::vertices(right)
      }
      g <- g + igraph::edges(c(frac, left), c(frac, right))

      # Update fractions for the next level
      new_fractions[[left]] <- c(left_num, left_den)
      new_fractions[[right]] <- c(right_num, right_den)
    }
    fractions <- new_fractions
  }

  # Find nodes to highlight based on the path
  traversed <- c("1/1")  # Start from root
  current <- "1/1"
  for (dir in strsplit(path, "")[[1]]) {
    children <- igraph::neighbors(g, current, mode = "out")
    children_names <- igraph::V(g)[children]$name
    if (dir == "0") {
      current <- children_names[1]  # Move to left child
    } else if (dir == "1") {
      current <- children_names[2]  # Move to right child
    }
    traversed <- c(traversed, current)
  }

  # Highlight the path
  igraph::V(g)$color <- ifelse(igraph::V(g)$name %in% traversed, "red", "lightgray")
  igraph::V(g)$label.color <- "blue"
  igraph::V(g)$size <- 20

  # Plot the tree
  igraph::plot.igraph(
    g,
    layout = igraph::layout_as_tree(g, root = "1/1", mode = "out"),
    vertex.label = igraph::V(g)$name,
    vertex.size = igraph::V(g)$size,
    vertex.color = igraph::V(g)$color,
    vertex.label.color = igraph::V(g)$label.color,
    edge.arrow.size = 0.5,
    main = paste("Stern-Brocot Tree (Traversed Path:", path, ")")
  )
}
