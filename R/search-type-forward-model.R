#' Create Model Search Definitions for S3 Methods
#'
#' Functions for creating model type definitions for objects in the Feature
#'   Selection framework. Current available search types/methods are:
#'   \itemize{
#'     \item forward stepwise model
#'     \item backward stepwise model
#'     \item forward stepwise parameter (deprecated)
#'   }
#'   This information is called within the object creation to modify the its
#'   attributes (class) to reflect the appropriate search values.
#'
#' @name search_type
#'
#' @param max_steps `integer(1)`. Maximum number of features allowed in the model.
#'
#' @param display_name `character(1)`. A title used by S3 plot methods.
#'
#' @return A list containing:
#'   \item{display_name}{The official "Title" to be used by downstream plot
#'     methods called on the `feature_select` object.}
#'   \item{max_steps}{Maximum model steps to search. Forward search only!}
#'
#' @author Kirk DeLisle & Stu Field
#'
#' @examples
#' search_type_forward_model()                 # the default = 20L
#' search_type_forward_model(max_steps = 15L)  # set to 15
#' search_type_forward_model("My Forward Search")   # set title
NULL


#' @describeIn search_type
#'   Forward model selection search
#'
#' @export
search_type_forward_model <- function(display_name = "Forward Stepwise Model Search",
                                      max_steps    = 20L) {
  as.list(environment()) |> add_class("fs_forward_model")
}


#' S3 Search method for forward searches
#'
#' @importFrom stats as.formula setNames
#' @noRd
#' @export
Search.fs_forward_model <- function(x, ...) {

  signal_rule("Using `Forward-Stepwise` model search", line_col = "magenta")
  cores <- list(...)$num_cores
  withr::local_options(list(stringsAsFactors = FALSE))

  # assume a certain number of runs within which there are cross-validated folds
  #   loop over r, f, and step ( runs, folds, and candidates )
  #   for model search, the cross-validated folds determine which
  #   parameter is chosen at any given step

  search_progress <- data.frame(step          = numeric(0),
                                cumul_markers = character(0),
                                cost          = numeric(0))
  used_candidates <- character(0)
  models      <- list()
  cost_tables <- list()

  for ( step in 1:x$search_type$max_steps ) {

    sprintf("Step %i of %s", step, x$search_type$max_steps) |>
      signal_rule(line_col = "blue")

     rem_candidates <- setdiff(x$candidate_markers, used_candidates)

     candidate_models <- lapply(rem_candidates, function(cnd) {
          frmla <- create_form(x$model_type$response,
                               paste(c(used_candidates, cnd)))
          #run.res <- foreach ( r=1:x$cross_val$runs ) %dopar% {   # dopar
          parallel::mclapply(seq_len(x$cross_val$runs), function(r) {
                    run <- sprintf("Run%d", r)
                    x$cross_val$current_run <- r
                    lapply(seq_len(x$cross_val$folds), function(f) {
                           x$cross_val$current_fold <- f
                           fold    <- sprintf("Fold%d", f)
                           mod     <- fitmodel(x, frmla = frmla)
                           cst     <- cost(mod)
                           mod_out <- if (x$keep_models)
                             mod$cross_val[[run]][[fold]]$model
                           else
                             NULL
                           list(cost = cst, model = mod_out)
                         }) |>
             setNames(sprintf("Fold%s", 1:x$cross_val$folds))
          }, mc.cores = cores) |>
          setNames(sprintf("Run%s", 1:x$cross_val$runs))
       #} |> setNames(sprintf("Run%s", 1:x$cross_val$runs))
       }) |> setNames(rem_candidates)

     #construct results table for selection of this candidate step
     cost_table <- lapply(names(candidate_models), function(cnd) {
                          sapply(names(candidate_models[[cnd]]), function(r) {
                                 sapply(names(candidate_models[[cnd]][[r]]), function(f) {
                                        candidate_models[[cnd]][[r]][[f]]$cost
                                })
                          })
                    }) |>
       setNames(names(candidate_models))

     ci95df <- sapply(cost_table, calc_CI95) |> t() |> data.frame()

     # select the best
     if ( x$cost_fxn$maximize ) {
       top_idx <- which.max(ci95df$mean)
     } else {
       top_idx <- which.min(ci95df$mean)
     }

     #print(top_idx)
     ci95top <- ci95df[top_idx, ]      # select "top" row; 1 row df with rowname
     new_par <- rownames(ci95top)
     used_candidates <- c(used_candidates, new_par)

     search_progress <- rbind(search_progress,
                              data.frame(step = step,
                                         cumul_markers = new_par,
                                         cost_lower_ci95 = ci95top$lower,
                                         cost_mean = ci95top$mean,
                                         cost_upper_ci95 = ci95top$upper))

     step_name <- sprintf("Step_%d", step)
     cost_tables[[step_name]] <- cost_table
  }

  # keep results of search
  x$cross_val$search_progress <- search_progress
  x$cross_val$cost_tables     <- cost_tables

  # update iterators
  x$cross_val$current_run  <- x$runs
  x$cross_val$current_fold <- x$folds

  invisible(x)
}


#' S3 plot method for fs_forward_model
#'
#' @importFrom ggplot2 ggplot aes theme element_text labs geom_pointrange
#' @importFrom ggplot2 element_blank scale_color_manual scale_x_continuous
#' @noRd
#' @export
plot.fs_forward_model <- function(x, ...) {

  check_complete(x)

  # progress mean/95% CI
  restbl <- x$cross_val$search_progress

  # complete cost tables
  csttbl <- x$cross_val$cost_tables

  row_nms <- list(fold = paste0("Fold", seq(x$folds)),
                  run  = paste0("Run", seq(x$runs))) |>
    expand.grid(stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  row_nms <- paste0(row_nms$fold, "_", row_nms$run)
  #print(row_nms)

  bxtbl <- liter(restbl$step, restbl$cumul_markers, function(.x, .y) {
      as.numeric(csttbl[[.x]][[.y]]) # matrix -> vector
    }) |>
    data.frame(row.names = row_nms) |>
    setNames(ifelse(is_seq(restbl$cumul_markers),
                    get_seq(restbl$cumul_markers), restbl$cumul_markers))

  box_cols  <- rep("grey", nrow(restbl))
  idx       <- get_peak_wilcox(bxtbl)
  tmp_col   <- c("red", # box colors by Wilcox signed rank test
                 col_palette$purple,
                 col_palette$lightgreen)

  for ( i in 1:length(idx) ) {
    box_cols[idx[i]] <- tmp_col[i]
  }

  p1 <- bxtbl |>
    SomaPlotr::boxplotBeeswarm(
      main = sprintf("Median %s\nWilcoxon Signed-Rank Peak Criterion",
                     x$cost_fxn$display_name),
      y.lab = x$cost_fxn$display_name,
      x.lab = paste("features added", symbl$arrow_right),
      notch = TRUE, cols = box_cols, ...) +
    ggplot2::scale_x_discrete(labels = names(bxtbl)) +
    ggplot2::theme(legend.position = "none",
                   axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))

  idx     <- get_peak_se(bxtbl)
  ci_cols <- c(Peak      = "red",
               Features  = col_palette$lightgrey,
               "1*se"    = col_palette$purple,
               "1.96*se" = col_palette$lightgreen)

  restbl$id                               <- "Features"
  restbl$id[restbl$step == idx["max"]]    <- "Peak"
  restbl$id[restbl$step == idx["p0.05"]]  <- "1*se"
  restbl$id[restbl$step == idx["p0.001"]] <- "1.96*se"

  p2 <-  restbl |>
    ggplot2::ggplot(ggplot2::aes(step, cost_mean, colour = id)) +
    ggplot2::geom_pointrange(
      ggplot2::aes(ymin = cost_lower_ci95, ymax = cost_upper_ci95),
      size = 0.75, alpha = 0.75) +
    ggplot2::scale_colour_manual(values = ci_cols) +
    ggplot2::labs(
      y = x$cost_fxn$display_name,
      x = paste("features added", symbl$arrow_right),
      title = sprintf("Mean %s %s 95%% CI\nStandard Error Peak Criterion",
                      x$cost_fxn$display_name, symbl$pm)
    ) +
    ggplot2::scale_x_continuous(
      breaks = restbl$step,
      labels = ifelse(is_seq(restbl$cumul_markers),
                      get_seq(restbl$cumul_markers), restbl$cumul_markers)
      ) +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   legend.position = "right",
                   axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))

  withr::with_package("patchwork", p1 + p2)
}
