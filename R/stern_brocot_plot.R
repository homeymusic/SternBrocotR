# A) BUILD THE "NEW-ONLY" STERN-BROCOT, THEN RENUMBER VERTICES 1..N
stern_brocot_graph <- function(depth) {
  # fraction_str -> numeric ID (temporary)
  node_map    <- new.env(parent = emptyenv())
  # ID -> earliest level introduced
  intro_level <- new.env(parent = emptyenv())
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
    get(key, envir = node_map)  # returns the temporary ID
  }

  edges <- data.frame(from = integer(), to = integer())

  # Recursively introduce mediants
  build_tree <- function(ln, ld, rn, rd, level) {
    if (level >= depth) return()

    mn <- ln + rn
    md <- ld + rd

    left_id  <- get_frac_id(ln, ld, level)
    right_id <- get_frac_id(rn, rd, level)
    mid_id   <- get_frac_id(mn, md, level + 1)

    # If bounding fraction was introduced at 'level', connect to 'mid_id'
    if (get(as.character(left_id), envir=intro_level) == level) {
      edges <<- rbind(edges, data.frame(from=left_id, to=mid_id))
    }
    if (get(as.character(right_id), envir=intro_level) == level) {
      edges <<- rbind(edges, data.frame(from=right_id, to=mid_id))
    }

    # Recurse deeper
    build_tree(ln, ld, mn, md, level + 1)
    build_tree(mn, md, rn, rd, level + 1)
  }

  # Level 0 boundary: new fractions "0/1" and "1/0"
  get_frac_id(0, 1, 0)
  get_frac_id(1, 0, 0)

  build_tree(0, 1, 1, 0, 0)

  # ----------------------------------------------------------------
  # NOW RENUMBER VERTICES (1..n) FOR A CLEAN igraph BUILD
  # ----------------------------------------------------------------

  # Gather all fraction strings (keys) from node_map
  all_frac_strs <- ls(node_map)
  # Their temporary IDs
  all_frac_ids  <- sapply(all_frac_strs, function(fs) get(fs, envir=node_map))

  # Actually used IDs in edges
  used_ids  <- unique(c(edges$from, edges$to))
  # Sort them to fix the final order
  used_ids  <- sort(used_ids)
  n         <- length(used_ids)  # final vertex count

  # Build a map oldID -> newID in 1..n
  reindex <- new.env(parent = emptyenv())
  for (i in seq_along(used_ids)) {
    old <- used_ids[i]
    assign(as.character(old), i, envir = reindex)
  }

  # Build a "clean" vertex data frame of size n
  df_list <- vector("list", n)
  for (i in seq_len(n)) {
    old_id <- used_ids[i]

    # find the fraction_str that has old_id
    str_idx <- which(all_frac_ids == old_id)
    frac_str <- all_frac_strs[str_idx]

    lv <- get(as.character(old_id), envir=intro_level)  # old level

    df_list[[i]] <- data.frame(
      name         = i,  # new ID in 1..n
      fraction_str = frac_str,
      level        = lv,
      stringsAsFactors = FALSE
    )
  }
  vertices_df <- do.call(rbind, df_list)

  # Re-map edges$from / to => new IDs
  edges$from <- sapply(edges$from, function(x) get(as.character(x), envir=reindex))
  edges$to   <- sapply(edges$to,   function(x) get(as.character(x), envir=reindex))

  # Build the igraph
  g <- igraph::graph_from_data_frame(
    d         = edges,
    directed  = TRUE,
    vertices  = vertices_df
  )

  list(
    graph       = g,
    node_map    = node_map,       # fraction_str -> old ID
    intro_level = intro_level     # old ID -> level
  )
}


# B) Compute layout from final-level ordering
graph_layout <- function(graph, node_map, intro_level, depth) {
  fraction_value <- function(frac_str) {
    parts <- strsplit(frac_str, "/")[[1]]
    num   <- as.numeric(parts[1])
    den   <- as.numeric(parts[2])
    if (den == 0) Inf else (num / den)
  }

  vertices_df <- igraph::as_data_frame(graph, what = "vertices")

  is_final <- (vertices_df$level == depth)
  final_df <- vertices_df[is_final, , drop=FALSE]

  if (nrow(final_df) == 0) {
    final_df <- vertices_df
  }
  final_df$val <- sapply(final_df$fraction_str, fraction_value)
  final_df     <- final_df[order(final_df$val), ]

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

  ig_order <- igraph::V(graph)$name
  ig_order_num <- as.integer(ig_order)
  match_idx <- match(ig_order_num, vertices_df$name)

  layout_mat <- cbind(x_coords[match_idx], y_coords[match_idx])
  layout_mat
}


# C) Mark path
graph_path <- function(graph, layout_mat, path) {
  # Ensure logical vectors:
  igraph::V(graph)$traveled <- rep(FALSE, igraph::vcount(graph))
  igraph::E(graph)$traveled <- rep(FALSE, igraph::ecount(graph))

  v_levels <- igraph::V(graph)$level
  v_names  <- as.integer(igraph::V(graph)$name)

  v_xpos <- round(layout_mat[,1])

  lvl_x_env <- new.env(parent = emptyenv())
  for (i in seq_along(v_levels)) {
    lv  <- v_levels[i]
    xv  <- v_xpos[i]
    vid <- v_names[i]   # in [1..vcount(graph)]
    assign(paste0(lv, ":", xv), vid, envir = lvl_x_env)
  }

  traveled_nodes <- c()
  traveled_edges <- c()

  # Start if path[1] == '1' => fraction_str == "1/1"
  idx_1_1 <- which(igraph::V(graph)$fraction_str == "1/1")

  if (nchar(path) > 0 && substr(path,1,1) == "1" && length(idx_1_1) == 1) {
    current_vid   <- v_names[idx_1_1]
    current_level <- v_levels[idx_1_1]
    current_x     <- v_xpos[idx_1_1]

    traveled_nodes <- c(traveled_nodes, current_vid)

    # subsequent chars
    if (nchar(path) > 1) {
      for (i in 2:nchar(path)) {
        step_c <- substr(path, i, i)
        next_level <- current_level + 1
        next_x     <- if (step_c == "1") current_x + 1 else current_x - 1

        key <- paste0(next_level, ":", next_x)
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

  if (length(traveled_nodes) > 0) {
    igraph::V(graph)$traveled[ traveled_nodes ] <- TRUE
  }
  if (length(traveled_edges) > 0) {
    igraph::E(graph)$traveled[ traveled_edges ] <- TRUE
  }

  list(
    traveled_nodes = traveled_nodes,
    traveled_edges = traveled_edges
  )
}


# D) MAIN WRAPPER
stern_brocot_tree <- function(depth, path) {
  res <- stern_brocot_graph(depth)
  g   <- res$graph
  lay <- graph_layout(g, res$node_map, res$intro_level, depth=depth)
  path_res <- graph_path(g, lay, path)

  list(
    graph          = g,
    layout         = lay,
    traveled_nodes = path_res$traveled_nodes,
    traveled_edges = path_res$traveled_edges
  )
}
