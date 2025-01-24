stern_brocot_graph <- function(depth) {
  node_map     <- new.env(parent = emptyenv())  # fraction_str -> temp ID
  intro_level  <- new.env(parent = emptyenv())  # temp ID -> level
  parents_map  <- new.env(parent = emptyenv())  # fraction_str -> c(left_str, right_str)

  next_id <- 0

  frac_str <- function(n, d) paste0(n, "/", d)

  # Store or retrieve fraction with bounding parents
  get_frac_id <- function(num, den, level, left_str, right_str) {
    fs <- frac_str(num, den)

    if (!exists(fs, envir = node_map, inherits = FALSE)) {
      next_id <<- next_id + 1
      assign(fs, next_id, envir = node_map)
      assign(as.character(next_id), level, envir = intro_level)
      assign(fs, c(left_str, right_str), envir = parents_map)
    } else {
      # fraction known, possibly update earliest intro level
      fid <- get(fs, envir = node_map)
      old_level <- get(as.character(fid), envir = intro_level)
      if (level < old_level) {
        assign(as.character(fid), level, envir = intro_level)
      }
      # If parents_map not set, store them
      if (!exists(fs, envir = parents_map, inherits=FALSE)) {
        assign(fs, c(left_str, right_str), envir = parents_map)
      }
    }
    get(fs, envir = node_map)  # numeric ID
  }

  edges <- data.frame(from = integer(), to = integer())

  mediant_num <- function(a,b) a + b

  # The recursion function
  build_tree <- function(ln, ld, rn, rd, level) {
    if (level >= depth) return()

    mn <- ln + rn
    md <- ld + rd

    left_str  <- frac_str(ln, ld)
    right_str <- frac_str(rn, rd)

    left_id  <- get_frac_id(ln, ld, level,  "", "")  # if boundaries, no parent
    right_id <- get_frac_id(rn, rd, level,  "", "")
    mid_id   <- get_frac_id(mn, md, level+1, left_str, right_str)

    # Connect if the bounding fraction was new
    # i.e. introduced exactly at 'level'
    if (get(as.character(left_id), envir=intro_level) == level) {
      edges <<- rbind(edges, data.frame(from=left_id, to=mid_id))
    }
    if (get(as.character(right_id), envir=intro_level) == level) {
      edges <<- rbind(edges, data.frame(from=right_id, to=mid_id))
    }

    build_tree(ln, ld, mn, md, level+1)
    build_tree(mn, md, rn, rd, level+1)
  }

  # Boundaries at level=0
  get_frac_id(0, 1, 0, "","")
  get_frac_id(1, 0, 0, "","")

  build_tree(0, 1, 1, 0, 0)

  # Now reindex used IDs => 1..n
  all_frac_strs <- ls(node_map)
  all_frac_ids  <- sapply(all_frac_strs, function(fs) get(fs, envir=node_map))
  used_ids      <- unique(c(edges$from, edges$to))
  used_ids      <- sort(used_ids)
  n             <- length(used_ids)

  # Build a reindex env
  reindex_env <- new.env(parent = emptyenv())
  for (i in seq_along(used_ids)) {
    assign(as.character(used_ids[i]), i, envir=reindex_env)
  }

  # Build final vertex data
  vlist <- vector("list", n)
  for (i in seq_len(n)) {
    old_id <- used_ids[i]
    # find fraction string
    idx_fs <- which(all_frac_ids == old_id)
    fs     <- all_frac_strs[idx_fs]
    lv     <- get(as.character(old_id), envir=intro_level)
    vlist[[i]] <- data.frame(
      name         = i,
      fraction_str = fs,
      level        = lv,
      stringsAsFactors = FALSE
    )
  }
  vertices_df <- do.call(rbind, vlist)

  # remap edges
  edges$from <- sapply(edges$from, function(x) get(as.character(x), envir=reindex_env))
  edges$to   <- sapply(edges$to,   function(x) get(as.character(x), envir=reindex_env))

  # Build igraph
  g <- igraph::graph_from_data_frame(edges, directed=TRUE, vertices=vertices_df)

  list(
    graph       = g,
    node_map    = node_map,      # fraction_str->temp ID
    intro_level = intro_level,   # ID->level
    parents_map = parents_map    # fraction_str -> c(left_str, right_str)
  )
}

graph_path <- function(graph, layout_mat, path, parents_map, node_map, intro_level) {
  # Build traveled vectors
  node_traveled <- rep(FALSE, igraph::vcount(graph))
  edge_traveled <- rep(FALSE, igraph::ecount(graph))

  # build a fraction -> igraph vertex ID map
  # e.g. "1/2" -> 7, etc.
  frac2vid <- new.env(parent = emptyenv())

  fraction_strs <- igraph::V(graph)$fraction_str
  v_names       <- as.integer(igraph::V(graph)$name)  # 1..vcount
  for (i in seq_along(fraction_strs)) {
    fs <- fraction_strs[i]
    assign(fs, v_names[i], envir=frac2vid)
  }

  # Utility to parse "num/den" -> (num, den)
  parse_frac <- function(fs) {
    parts <- strsplit(fs, "/")[[1]]
    c(as.numeric(parts[1]), as.numeric(parts[2]))
  }
  # mediant of fraction strings
  mediant_str <- function(fA, fB) {
    A <- parse_frac(fA)
    B <- parse_frac(fB)
    paste0(A[1]+B[1], "/", A[2]+B[2])
  }

  # We'll store the path in a vector of fractions. Start at "1/1" if path starts with '1', or do nothing otherwise?
  # Actually: The spec says the leftmost spot is always '1' => meaning we start at "1/1"
  # If we want to be consistent with your prior code, let's do it:

  start_vid <- if (nchar(path)>0 && substr(path,1,1)=="1") {
    # find "1/1"
    if (exists("1/1", envir=frac2vid)) get("1/1", envir=frac2vid) else 0
  } else {
    0
  }

  traveled_nodes_idx <- integer(0)
  traveled_edges_idx <- integer(0)

  if (start_vid != 0) {
    traveled_nodes_idx <- c(traveled_nodes_idx, start_vid)

    current_fs <- "1/1"
    # the bounding parents for "1/1" must exist in parents_map
    # e.g. "1/1" might have c("0/1","1/0") if introduced from them
    # each step in path => child = mediant(left, F) or mediant(F,right)
    # then see if child is in the graph

    # We ignore the fact we used "1" from the path just to start at "1/1"
    # because your tests do. The next digits are the "descents"
    if (nchar(path)>1) {
      for (idx in 2:nchar(path)) {
        step_char <- substr(path, idx, idx)  # '0' or '1'

        # bounding parents of current_fs
        if (!exists(current_fs, envir=parents_map, inherits=FALSE)) {
          break
        }
        parents <- get(current_fs, envir=parents_map)  # c(left_str, right_str)
        left_fs <- parents[1]
        right_fs<- parents[2]

        # child fraction
        child_fs <- if (step_char=="0") {
          mediant_str(left_fs, current_fs)
        } else {
          mediant_str(current_fs, right_fs)
        }

        # does child_fs exist in node_map => means it's in the graph up to 'depth'
        if (!exists(child_fs, envir=node_map, inherits=FALSE)) {
          break
        }

        # get child's igraph ID
        child_vid <- get(child_fs, envir=frac2vid, inherits=FALSE)
        if (is.null(child_vid)) {
          # fraction in node_map but might not be used => break
          break
        }

        traveled_nodes_idx <- c(traveled_nodes_idx, child_vid)

        # find edge current_vid->child_vid
        eid <- igraph::get_edge_ids(graph, vp = c(get(current_fs, envir=frac2vid), child_vid), directed=TRUE)
        if (eid != 0) {
          traveled_edges_idx <- c(traveled_edges_idx, eid)
        }

        # move
        current_fs <- child_fs
      }
    }
  }

  traveled_nodes_idx <- unique(traveled_nodes_idx)
  traveled_edges_idx <- unique(traveled_edges_idx)

  if (length(traveled_nodes_idx)>0) {
    node_traveled[ traveled_nodes_idx ] <- TRUE
  }
  if (length(traveled_edges_idx)>0) {
    edge_traveled[ traveled_edges_idx ] <- TRUE
  }

  graph <- igraph::set_vertex_attr(graph, "traveled", value=node_traveled)
  graph <- igraph::set_edge_attr(graph,   "traveled", value=edge_traveled)

  list(
    graph = graph,
    traveled_nodes = traveled_nodes_idx,
    traveled_edges = traveled_edges_idx
  )
}

stern_brocot_tree <- function(depth, path) {
  res <- stern_brocot_graph(depth)
  g   <- res$graph
  lay <- graph_layout(g, res$node_map, res$intro_level, depth=depth)

  path_res <- graph_path(
    graph       = g,
    layout_mat  = lay,
    path        = path,
    parents_map = res$parents_map,
    node_map    = res$node_map,
    intro_level = res$intro_level
  )

  list(
    graph          = path_res$graph,
    layout         = lay,
    traveled_nodes = path_res$traveled_nodes,
    traveled_edges = path_res$traveled_edges
  )
}

graph_layout <- function(graph, node_map, intro_level, depth) {
  # 1) Gather all fractions in the graph + compute numeric values
  vertices_df <- igraph::as_data_frame(graph, what = "vertices")

  fraction_value <- function(frac_str) {
    parts <- strsplit(frac_str, "/")[[1]]
    num   <- as.numeric(parts[1])
    den   <- as.numeric(parts[2])
    if (den == 0) Inf else (num / den)
  }

  all_vals <- sapply(vertices_df$fraction_str, fraction_value)

  # 2) Sort them globally => each fraction gets a unique rank 1..n
  #    If "1/0" is present, it becomes the largest rank.
  ord     <- order(all_vals)  # ascending
  # Build a map from fraction_str => rank
  #   e.g. rank_map["0/1"] = 1, rank_map["1/2"] = 2, etc.
  rank_map <- setNames(seq_along(ord), vertices_df$fraction_str[ord])

  # 3) Assign (x, y) to each fraction:
  #    x = the rank among *all* fractions in numeric order
  #    y = negative of the fraction's introduction level
  n <- nrow(vertices_df)
  x_coords <- numeric(n)
  y_coords <- numeric(n)

  for (i in seq_len(n)) {
    fs   <- vertices_df$fraction_str[i]
    lvl  <- vertices_df$level[i]
    x_coords[i] <- as.numeric(rank_map[fs])
    y_coords[i] <- -lvl
  }

  # 4) Re‐order rows to match igraph’s internal vertex order
  ig_order     <- igraph::V(graph)$name          # typically "1","2",...
  ig_order_num <- as.integer(ig_order)
  match_idx    <- match(ig_order_num, vertices_df$name)

  layout_mat <- cbind(x_coords[match_idx], y_coords[match_idx])
  layout_mat
}
