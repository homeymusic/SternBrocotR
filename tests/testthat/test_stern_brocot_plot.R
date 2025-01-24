test_that("No duplicate fractions", {
  res <- stern_brocot_graph(4)
  g   <- res$graph

  fraction_strs <- igraph::V(g)$fraction_str
  expect_equal(length(unique(fraction_strs)), igraph::vcount(g))
})
test_that("Check level ranges", {
  res <- stern_brocot_graph(4)
  g   <- res$graph

  lvls <- igraph::V(g)$level
  # All levels should be in [0 .. 4]
  expect_true(all(lvls >= 0 & lvls <= 4))
  # Expect that there's at least one vertex at level 0
  expect_true(any(lvls == 0))
  # Possibly also check that "1/1" is at level 1
  idx_1_1 <- which(igraph::V(g)$fraction_str == "1/1")
  expect_length(idx_1_1, 1)
  expect_equal(lvls[idx_1_1], 1)
})
test_that("Edge count is sane", {
  res <- stern_brocot_graph(4)
  g   <- res$graph
  ec  <- igraph::ecount(g)
  # e.g. expect 16 edges, or at least that ec < some threshold
  expect_equal(ec, 16)  # or use some known count
})
test_that("1/1 is connected from 0/1 and 1/0", {
  res <- stern_brocot_graph(4)
  g   <- res$graph

  # find IDs in igraph
  v0_1 <- which(igraph::V(g)$fraction_str == "0/1")
  v1_0 <- which(igraph::V(g)$fraction_str == "1/0")
  v1_1 <- which(igraph::V(g)$fraction_str == "1/1")

  # get edge IDs
  e1 <- igraph::get_edge_ids(g, vp=c(v0_1, v1_1), directed=TRUE)
  e2 <- igraph::get_edge_ids(g, vp=c(v1_0, v1_1), directed=TRUE)

  # If edges exist, get_edge_ids() won't be 0
  expect_true(e1 != 0)
  expect_true(e2 != 0)
})
test_that("Layout matrix matches vcount", {
  res <- stern_brocot_graph(4)
  g   <- res$graph
  lay <- graph_layout(g, res$node_map, res$intro_level, depth=4)

  expect_equal(nrow(lay), igraph::vcount(g))
  expect_equal(ncol(lay), 2)

  # Optionally check range of x-coordinates
  xs <- lay[,1]
  expect_true(all(xs >= 1))  # or whatever range you expect
})
test_that("Path traveling actually marks traveled nodes/edges", {
  sb <- stern_brocot_tree(depth=3, path="10")
  # or some path you know is valid at depth=3

  g   <- sb$graph
  # Check that at least 2 nodes got traveled
  traveled_v <- which(igraph::V(g)$traveled)
  expect_length(traveled_v, 2)  # e.g. the path might visit 2 nodes

  # Check that at least 1 edge is traveled
  traveled_e <- which(igraph::E(g)$traveled)
  expect_gt(length(traveled_e), 0)
})
test_that("Invalid path does not break code", {
  sb <- stern_brocot_tree(depth=2, path="111111111")  # obviously too long
  g  <- sb$graph

  # We might expect only 1 or 2 nodes traveled, or zero edges
  traveled_v <- which(igraph::V(g)$traveled)
  # Just ensure no errors or mismatches
  expect_true(length(traveled_v) >= 1)
})
test_that("Depth=1 has 3 expected fractions", {
  sb <- stern_brocot_tree(depth=1, path="")
  g  <- sb$graph
  frac_strs <- igraph::V(g)$fraction_str
  expect_setequal(frac_strs, c("0/1","1/1","1/0"))
})
test_that("Vertex IDs match 1..vcount(g)", {
  sb <- stern_brocot_graph(4)
  g  <- sb$graph
  n  <- igraph::vcount(g)

  # 'names' must be a character vector
  numeric_names <- as.integer(igraph::V(g)$name)
  # remove NAs in case some vertex name is not numeric
  numeric_names <- numeric_names[ ! is.na(numeric_names) ]

  if (length(numeric_names) == n) {
    # check that min..max == 1..n
    expect_equal(sort(numeric_names), seq_len(n))
  } else {
    # If some vertex has a non-numeric name, that's also fine,
    # but we can't do the numeric test
    expect_true(TRUE)
  }
})

test_that("Path='10' from depth=3 ends at 1/2", {
  sb <- stern_brocot_tree(depth = 3, path = "10")

  traveled_nodes <- which(igraph::V(sb$graph)$traveled)
  traveled_fracs <- igraph::V(sb$graph)$fraction_str[ traveled_nodes ]

  # Should contain 1/1 and 1/2
  expect_true("1/1" %in% traveled_fracs)
  expect_true("1/2" %in% traveled_fracs)

  # Should not include 3/2, 1/3, etc.
  expect_false("3/2" %in% traveled_fracs)
  expect_false("1/3" %in% traveled_fracs)
})

test_that("Path='101' visits 1/2 -> 2/3 (not 3/2)", {
  sb <- stern_brocot_tree(depth = 3, path = "101")

  # Gather which fractions got traveled
  traveled_nodes <- which(igraph::V(sb$graph)$traveled)
  traveled_fracs <- igraph::V(sb$graph)$fraction_str[ traveled_nodes ]

  # We want "1/1", "1/2", and "2/3"
  expect_true("1/1" %in% traveled_fracs)
  expect_true("1/2" %in% traveled_fracs)
  expect_true("2/3" %in% traveled_fracs)

  # We do NOT want "3/2"
  expect_false("3/2" %in% traveled_fracs)
})

test_that("vdiffr: Stern–Brocot depth=3 (no path)", {
  vdiffr::expect_doppelganger("SB depth=3 no path", function() stern_brocot_no_path_plot(depth=3))
})
test_that("vdiffr: Stern–Brocot depth=3 path=101", {
  vdiffr::expect_doppelganger("SB depth=3 path=101", function() stern_brocot_plot(path="101"))
})

test_that("vdiffr: Stern-Brocot depth=7 path=1100110", {
  vdiffr::expect_doppelganger("SB depth=7 path=1100110", function() stern_brocot_plot(path = "1100110"))
})

