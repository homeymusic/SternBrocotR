###########################################################
# 1) Build and reindex the "new-only" Stern-Brocot graph
###########################################################
stern_brocot_graph <- function(depth) {
  node_map    <- new.env(parent = emptyenv())  # fraction_str -> temp ID
  intro_level <- new.env(parent = emptyenv())  # temp ID -> level
  next_id     <- 0

  get_frac_id <- function(num, den, level) {
    key <- paste0(num, "/", den)
    if (!exists(key, envir = node_map, inherits = FALSE)) {
      next_id <<- next_id + 1
      assign(key, next_id, envir = node_map)
      assign(as.character(next_id), level, envir = intro_level)
    } else {
      id <- get(key, envir = node_map)
      old_level <- get(as.character(id), envir = intro_level)
      if (level < old_level) {
        assign(as.character(id), level, envir = intro_level)
      }
    }
    get(key, envir = node_map)
  }

  edges <- data.frame(from = integer(), to = integer())

  build_tree <- function(ln, ld, rn, rd, lvl) {
    if (lvl >= depth) return()
    mn <- ln + rn
    md <- ld + rd

    left_id  <- get_frac_id(ln, ld, lvl)
    right_id <- get_frac_id(rn, rd, lvl)
    mid_id   <- get_frac_id(mn, md, lvl + 1)

    if (get(as.character(left_id), envir=intro_level) == lvl) {
      edges <<- rbind(edges, data.frame(from=left_id, to=mid_id))
    }
    if (get(as.character(right_id), envir=intro_level) == lvl) {
      edges <<- rbind(edges, data.frame(from=right_id, to=mid_id))
    }

    build_tree(ln, ld, mn, md, lvl + 1)
    build_tree(mn, md, rn, rd, lvl + 1)
  }

  # Level 0 boundary fractions
  get_frac_id(0, 1, 0)
  get_frac_id(1, 0, 0)
  build_tree(0, 1, 1, 0, 0)

  # Reindex used IDs => 1..n
  all_frac_strs <- ls(node_map)
  all_frac_ids  <- sapply(all_frac_strs, function(fs) get(fs, envir=node_map))
  used_ids      <- unique(c(edges$from, edges$to))
  used_ids      <- sort(used_ids)
  n             <- length(used_ids)

  reindex_env <- new.env(parent = emptyenv())
  for (i in seq_along(used_ids)) {
    assign(as.character(used_ids[i]), i, envir = reindex_env)
  }

  # Build final vertex data
  vlist <- vector("list", n)
  for (i in seq_len(n)) {
    old_id  <- used_ids[i]
    frac_ix <- which(all_frac_ids == old_id)
    fs      <- all_frac_strs[frac_ix]
    lv      <- get(as.character(old_id), envir = intro_level)
    vlist[[i]] <- data.frame(
      name         = i,
      fraction_str = fs,
      level        = lv,
      stringsAsFactors = FALSE
    )
  }
  vertices_df <- do.call(rbind, vlist)

  # Remap edges
  edges$from <- sapply(edges$from, function(x) get(as.character(x), envir=reindex_env))
  edges$to   <- sapply(edges$to,   function(x) get(as.character(x), envir=reindex_env))

  g <- igraph::graph_from_data_frame(
    d         = edges,
    directed  = TRUE,
    vertices  = vertices_df
  )

  list(
    graph       = g,
    node_map    = node_map,
    intro_level = intro_level
  )
}


###########################################################
# 2) Layout based on final-level fraction ordering
###########################################################
graph_layout <- function(graph, node_map, intro_level, depth) {
  fraction_value <- function(frac_str) {
    parts <- strsplit(frac_str, "/")[[1]]
    num   <- as.numeric(parts[1])
    den   <- as.numeric(parts[2])
    if (den == 0) Inf else (num / den)
  }

  vertices_df <- igraph::as_data_frame(graph, "vertices")
  is_final    <- (vertices_df$level == depth)
  final_df    <- vertices_df[ is_final, , drop=FALSE ]
  if (nrow(final_df) == 0) {
    final_df <- vertices_df
  }
  final_df$val <- sapply(final_df$fraction_str, fraction_value)
  final_df     <- final_df[ order(final_df$val), ]

  final_vals <- final_df$val
  k          <- length(final_vals)

  position_in_final <- function(val) {
    pos <- sum(final_vals < val) + 1
    if (pos < 1) pos <- 1
    if (pos > k) pos <- k
    pos
  }

  x_coords <- numeric(nrow(vertices_df))
  y_coords <- numeric(nrow(vertices_df))

  for (i in seq_len(nrow(vertices_df))) {
    fs  <- vertices_df$fraction_str[i]
    lvl <- vertices_df$level[i]
    val <- fraction_value(fs)
    x_coords[i] <- position_in_final(val)
    y_coords[i] <- -lvl
  }

  # Reorder in igraph order
  ig_order     <- igraph::V(graph)$name   # e.g. c("1","2",...)
  ig_order_num <- as.integer(ig_order)
  match_idx    <- match(ig_order_num, vertices_df$name)

  layout_mat <- cbind(x_coords[match_idx], y_coords[match_idx])
  layout_mat
}


###########################################################
# 3) Mark traveled path
###########################################################
graph_path <- function(graph, layout_mat, path) {
  # Build traveled vectors in normal R
  node_traveled_vec <- rep(FALSE, igraph::vcount(graph))
  edge_traveled_vec <- rep(FALSE, igraph::ecount(graph))

  v_levels <- igraph::V(graph)$level
  v_names  <- as.integer(igraph::V(graph)$name)
  v_xpos   <- round(layout_mat[,1])

  # Build map from (level, x) => vertex ID
  lvl_x_env <- new.env(parent = emptyenv())
  for (i in seq_along(v_levels)) {
    lv  <- v_levels[i]
    xv  <- v_xpos[i]
    vid <- v_names[i]
    assign(paste0(lv, ":", xv), vid, envir = lvl_x_env)
  }

  traveled_nodes <- integer(0)
  traveled_edges <- integer(0)

  # Start at "1/1" if path[1] == '1'
  idx_1_1 <- which(igraph::V(graph)$fraction_str == "1/1")
  if (nchar(path) > 0 && substr(path,1,1) == "1" && length(idx_1_1) == 1) {
    current_vid   <- v_names[idx_1_1]
    current_level <- v_levels[idx_1_1]
    current_x     <- v_xpos[idx_1_1]
    traveled_nodes <- c(traveled_nodes, current_vid)

    if (nchar(path) > 1) {
      for (i in 2:nchar(path)) {
        step_c     <- substr(path, i, i)
        next_level <- current_level + 1
        next_x     <- if (step_c == "1") current_x + 1 else current_x - 1
        key        <- paste0(next_level, ":", next_x)
        if (!exists(key, envir=lvl_x_env, inherits=FALSE)) {
          break
        }
        next_vid <- get(key, envir=lvl_x_env)
        traveled_nodes <- c(traveled_nodes, next_vid)

        eid <- igraph::get.edge.ids(graph, vp=c(current_vid, next_vid), directed=TRUE)
        if (eid != 0) {
          traveled_edges <- c(traveled_edges, eid)
        }

        current_vid   <- next_vid
        current_level <- next_level
        current_x     <- next_x
      }
    }
  }

  traveled_nodes <- unique(traveled_nodes)
  traveled_edges <- unique(traveled_edges)

  # Now assign them all at once
  if (length(traveled_nodes) > 0) {
    node_traveled_vec[ traveled_nodes ] <- TRUE
  }
  if (length(traveled_edges) > 0) {
    edge_traveled_vec[ traveled_edges ] <- TRUE
  }

  # Put them into the graph as a logical vector
  graph <- igraph::set_vertex_attr(graph, "traveled", value = node_traveled_vec)
  graph <- igraph::set_edge_attr(graph,   "traveled", value = edge_traveled_vec)

  list(
    graph          = graph,
    traveled_nodes = traveled_nodes,
    traveled_edges = traveled_edges
  )
}


###########################################################
# 4) MAIN WRAPPER
###########################################################
stern_brocot_tree <- function(depth, path) {
  res <- stern_brocot_graph(depth)
  g   <- res$graph
  lay <- graph_layout(g, res$node_map, res$intro_level, depth=depth)

  path_res  <- graph_path(g, lay, path)
  g_updated <- path_res$graph

  list(
    graph          = g_updated,
    layout         = lay,
    traveled_nodes = path_res$traveled_nodes,
    traveled_edges = path_res$traveled_edges
  )
}
