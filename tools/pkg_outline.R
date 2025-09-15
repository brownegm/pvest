# tools/pkg_outline.R
build_pkg_outline <- function(pkg = NULL,
                              keep_source = TRUE,
                              include_nonexported = TRUE) {
  # auto-detect package name if not provided
  if (is.null(pkg)) {
    desc_file <- file.path(rprojroot::find_package_root_file(), "DESCRIPTION")
    if (!file.exists(desc_file)) {
      stop("No DESCRIPTION file found. Run from inside a package project.")
    }
    pkg <- read.dcf(desc_file, fields = "Package")[1, 1]
  }
  
  pkgs <- c("devtools","codetools","igraph","tibble","dplyr","purrr",
            "tidyr","stringr","visNetwork","htmltools","rlang")
  
  to_install <- setdiff(pkgs, rownames(installed.packages()))
  if (length(to_install)) install.packages(to_install, quiet = TRUE)
  lapply(pkgs, require, character.only = TRUE)
  
  devtools::load_all(export_all = TRUE, helpers = FALSE, quiet = TRUE)
  
  ns   <- asNamespace(pkg)
  objs <- ls(ns, all.names = TRUE)
  is_fun <- vapply(objs, function(nm) is.function(get(nm, envir = ns)), logical(1))
  fun_names <- objs[is_fun]
  if (!include_nonexported) fun_names <- intersect(fun_names, getNamespaceExports(pkg))
  
  .clean_called <- function(x) {
    bad <- c("::",":::","$","[","[[","UseMethod","NextMethod","%>%")
    unique(setdiff(x, bad))
  }
  
  # --- S3 awareness -----------------------------------------------------
  s3tab <- tryCatch(getNamespaceInfo(ns, "S3methods"), error = function(e) NULL)
 
  s3_df <- if (!is.null(s3tab)) {
    # coerce to tibble, keep only the first 3 cols (generic, class, method)
    tib <- as.data.frame(s3tab, stringsAsFactors = FALSE)
    tibble::as_tibble(tib[, 1:3, drop = FALSE],
                      .name_repair = ~ c("generic","class","method"))
  } else {
    tibble::tibble(generic = character(), class = character(), method = character())
  }
  
  s3_df <- s3_df |>
    dplyr::mutate(
      method_name = dplyr::if_else(.data$method == "" | is.na(.data$method),
                                   paste0(.data$generic, ".", .data$class),
                                   .data$method)
    )
  
  has_method <- vapply(
    s3_df$method_name,
    function(nm) exists(nm, envir = ns, inherits = FALSE) && is.function(get(nm, envir = ns)),
    logical(1)
  )
  s3_df <- s3_df[has_method, , drop = FALSE]
  s3_method_names <- s3_df$method_name
  
  # unify function set with S3 methods
  fun_names <- union(fun_names, s3_method_names)
  
  # which generics are in our namespace vs external
  s3_generics <- unique(s3_df$generic)
  generics_in_ns <- s3_generics[vapply(s3_generics, function(nm)
    exists(nm, envir = ns, inherits = FALSE) && is.function(get(nm, envir = ns)), logical(1))]
  generics_external <- setdiff(s3_generics, generics_in_ns)
  
  # --- Nodes: metadata for internal functions ---------------------------
  node_df <- purrr::map_dfr(fun_names, function(fn) {
    f <- get(fn, envir = ns)
    sr <- utils::getSrcref(f)
    srcfile <- tryCatch(attr(sr, "srcfile")$filename, error = function(e) NA_character_)
    line1   <- tryCatch(sr[1], error = function(e) NA_integer_)
    line2   <- tryCatch(sr[3], error = function(e) NA_integer_)
    ex <- fn %in% getNamespaceExports(pkg)
    args <- tryCatch(names(formals(f)), error = function(e) character(0))
    tibble::tibble(
      name = fn, kind = "internal_fn",
      exported = ex, file = srcfile,
      line_start = line1, line_end = line2,
      n_args = length(args), args = paste(args, collapse = ", ")
    )
  })
  
  # add nodes for generics defined in our ns (if not already present)
  if (length(generics_in_ns)) {
    add_ns_generics <- setdiff(generics_in_ns, node_df$name)
    if (length(add_ns_generics)) {
      ng <- purrr::map_dfr(add_ns_generics, function(fn) {
        f <- get(fn, envir = ns)
        sr <- utils::getSrcref(f)
        srcfile <- tryCatch(attr(sr, "srcfile")$filename, error = function(e) NA_character_)
        line1   <- tryCatch(sr[1], error = function(e) NA_integer_)
        line2   <- tryCatch(sr[3], error = function(e) NA_integer_)
        ex <- fn %in% getNamespaceExports(pkg)
        args <- tryCatch(names(formals(f)), error = function(e) character(0))
        tibble::tibble(
          name = fn, kind = "generic_fn",
          exported = ex, file = srcfile,
          line_start = line1, line_end = line2,
          n_args = length(args), args = paste(args, collapse = ", ")
        )
      })
      node_df <- bind_rows(node_df, ng)
    }
  }
  
  # add placeholder nodes for external generics (no src info)
  if (length(generics_external)) {
    ext_nodes <- tibble::tibble(
      name = generics_external, kind = "external_generic",
      exported = NA, file = NA_character_,
      line_start = NA_integer_, line_end = NA_integer_,
      n_args = NA_integer_, args = ""
    )
    node_df <- bind_rows(node_df, ext_nodes)
  }
  
  # --- Edges: internal calls -------------------------------------------
  edge_calls <- purrr::map_dfr(intersect(node_df$name, fun_names), function(fn) {
    f <- get(fn, envir = ns)
    globs <- tryCatch(codetools::findGlobals(f, merge = FALSE)$functions,
                      error = function(e) character(0))
    called <- .clean_called(globs)
    # allow calls to any node we know about (internal functions + generics we injected)
    called_known <- intersect(called, node_df$name)
    if (!length(called_known)) {
      return(tibble::tibble(from = character(0), to = character(0), type = character(0)))
    }
    tibble::tibble(from = fn, to = called_known, type = "call")
  }) %>% distinct()
  
  # --- Edges: S3 generic -> method (dashed) -----------------------------
  s3_edges <- if (nrow(s3_df)) {
    tibble::tibble(from = s3_df$generic, to = s3_df$method_name, type = "s3dispatch")
  } else tibble::tibble(from = character(), to = character(), type = character())
  
  edge_df <- bind_rows(edge_calls, s3_edges) %>% distinct()
  
  # degrees & graph stats
  calls_out <- edge_df %>% count(from, name = "out_degree")
  calls_in  <- edge_df %>% count(to,   name = "in_degree")
  node_df <- node_df %>%
    left_join(calls_out, by = c("name" = "from")) %>%
    left_join(calls_in,  by = c("name" = "to")) %>%
    mutate(across(c(in_degree, out_degree), ~replace_na(., 0L)))
  
  g <- igraph::graph_from_data_frame(edge_df, directed = TRUE, vertices = node_df)
  is_dag <- igraph::is_dag(g)
  topo <- if (is_dag) igraph::as_ids(igraph::topo_sort(g, mode = "out")) else character(0)
  btwn <- igraph::betweenness(g, directed = TRUE, normalized = TRUE)
  node_df$betweenness <- as.numeric(btwn[match(node_df$name, names(btwn))])
  
  comps <- igraph::components(igraph::as_undirected(g, mode = "collapse"))
  
  node_df$cluster_id <- comps$membership[match(node_df$name, names(comps$membership))]
  
  outline_by_file <- node_df %>%
    arrange(factor(file, levels = sort(unique(file))), line_start, name) %>%
    group_by(file) %>%
    summarise(
      functions = sum(kind != "external_generic", na.rm = TRUE),
      exported = sum(exported %in% TRUE, na.rm = TRUE),
      list = paste0(
        sprintf("• %s(%s)  [%s:%s]  %s  {%s}",
                name, args,
                dplyr::coalesce(as.character(line_start), "?"),
                dplyr::coalesce(as.character(line_end), "?"),
                dplyr::if_else(exported %in% TRUE, "exported", "internal"),
                kind),
        collapse = "\n"
      ),
      .groups = "drop"
    )
  
  # --- Interactive vis --------------------------------------------------
  vn_nodes <- node_df %>%
    transmute(
      id = name,
      label = name,
      title = paste0(
        "<b>", htmltools::htmlEscape(name), "</b>",
        "<br/>kind: ", kind,
        "<br/>args: ", htmltools::htmlEscape(args),
        "<br/>exported: ", exported,
        "<br/>file: ", htmltools::htmlEscape(file),
        "<br/>lines: ", line_start, "-", line_end,
        "<br/>in/out: ", in_degree, "/", out_degree,
        "<br/>betweenness: ", sprintf("%.3f", betweenness),
        "<br/>cluster: ", cluster_id
      ),
      # size by connectivity; external generics small
      value = dplyr::if_else(kind == "external_generic", 1, pmax(1, in_degree + out_degree)),
      group = dplyr::case_when(
        kind == "external_generic" ~ "external_generic",
        exported ~ "exported",
        TRUE ~ "internal"
      )
    )
  
  vn_edges <- edge_df %>%
    rename(from = from, to = to) %>%
    mutate(dashes = type == "s3dispatch",
           title  = ifelse(type == "s3dispatch", "S3 dispatch: generic → method", "call"))
  
  net <- visNetwork::visNetwork(vn_nodes, vn_edges, height = "750px", width = "100%") %>%
    visNetwork::visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
    visNetwork::visPhysics(stabilization = TRUE, solver = "forceAtlas2Based") %>%
    visNetwork::visLegend(addNodes = data.frame(
      label = c("exported", "internal", "external_generic"),
      shape = "dot", title = c("Exported fn", "Internal fn", "Generic from other pkg")
    ))
  
  list(
    nodes = node_df,
    edges = edge_df,
    outline_by_file = outline_by_file,
    graph = g,
    is_dag = is_dag,
    topo_order = topo,
    s3 = s3_df,
    vis = net
  )
}

run_pkg_outline <- function(save_html = "pkg_callgraph.html") {
  res <- build_pkg_outline()
  message("Functions: ", nrow(res$nodes), "   Edges: ", nrow(res$edges))
  if (nrow(res$edges) == 0) message("No internal calls detected.")
  message(if (res$is_dag) "Graph is a DAG." else "Graph has cycles.")
  if (!is.null(save_html)) {
    visNetwork::visSave(res$vis, file = save_html, selfcontained = TRUE)
    message("Saved interactive call graph to: ", normalizePath(save_html))
  }
  res
}

# convenience helpers
who_calls <- function(out, fn) out$edges |> dplyr::filter(.data$to == fn, .data$type == "call") |> dplyr::pull(.data$from)
who_it_calls <- function(out, fn) out$edges |> dplyr::filter(.data$from == fn, .data$type == "call") |> dplyr::pull(.data$to)
s3_methods_of <- function(out, generic) out$s3 |> dplyr::filter(.data$generic == !!generic) |> dplyr::pull(.data$method_name)
