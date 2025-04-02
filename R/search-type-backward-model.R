
#' @describeIn search_type
#'   Backward model selection search.
#'
#' @export
search_type_backward_model <- function(display_name = "Backward Stepwise Model Search") {
  as.list(environment()) |> add_class("fs_backward_model")
}

#' S3 Search method for backward searches
#'
#' @importFrom stats as.formula setNames
#' @noRd
#' @export
Search.fs_backward_model <- function(x, ...) {

  signal_rule("Using `Backward-Stepwise` model search", line_col = "magenta")
  cores <- list(...)$num_cores
  withr::local_options(list(stringsAsFactors = FALSE))

  # assume a certain number of runs within which there
  #   are cross-validated folds
  #   loop over r, f, and step ( runs, folds, and candidates )
  #   for model search, the cross-validated folds determine
  #   which feature is chosen at any given step

  search_progress <- data.frame(step          = numeric(0),
                                elim_features = character(0),
                                cost          = numeric(0))
  deleted_candidates <- character(0)
  cost_tables        <- list()

  # add the full model as the first step
  mod_candidate_features <- c("All", x$candidate_features)

  for ( step in seq_along(x$candidate_features) ) {

    sprintf("Step %i of %i", step, length(x$candidate_features)) |>
      signal_info()

    rem_candidates <- setdiff(mod_candidate_features, deleted_candidates)

    # reformulated search as a loop (compared to forward-lapply
    #   to facilitate a full model step
    candidate_costs <- list()

    for ( cnd in rem_candidates ) {
      rem_minus_one <- setdiff(rem_candidates, cnd)
      frmla <- create_form(x$model_type$response, paste(rem_minus_one))
      run_res <- parallel::mclapply(seq_len(x$cross_val$runs), function(r) {
                   x$cross_val$current_run <- r
                   lapply(seq_len(x$cross_val$folds), function(f) {
                          x$cross_val$current_fold <- f
                          .cost(.fitmodel(x, frmla = frmla))
                 }) |>
              setNames(sprintf("Fold%s", seq_len(x$folds)))
      }, mc.cores = cores) |>
      setNames(sprintf("Run%s", seq_len(x$runs)))

      candidate_costs[[cnd]] <- run_res

      if ( cnd == "All" ) {
        break
      }
    }
    # construct results table for selection of this candidate step
    cost_tbl <- lapply(candidate_costs, function(.cnd) {
      vapply(.cnd, unlist, use.names = TRUE,   # passed to base::unlist
             USE.NAMES = TRUE, FUN.VALUE = numeric(x$folds))
    })

    ci95df <- lapply(cost_tbl, calc_CI95) |> do.call(what = "rbind")

    # select the worst
    top_idx <- ifelse(x$cost_fxn$maximize,
                      which.max(ci95df$mean),
                      which.min(ci95df$mean))

    ci95top <- ci95df[top_idx, ]  # 1 row df with rowname
    new_feat <- rownames(ci95top)
    deleted_candidates <- c(deleted_candidates, new_feat)

    search_progress <- rbind(search_progress,
                             data.frame(step = step,
                                        elim_features   = new_feat,
                                        cost_lower_ci95 = ci95top$lower,
                                        cost_mean       = ci95top$mean,
                                        cost_upper_ci95 = ci95top$upper))

    step_name <- sprintf("Step_%d", step)
    cost_tables[[step_name]] <- cost_tbl
  }

  # keep results of search
  x$cross_val$search_progress <- search_progress
  x$cross_val$cost_tables     <- cost_tables

  invisible(x)
}


#' S3 plot method for fs_backward_model
#'
#' @importFrom ggplot2 ggplot aes theme element_text labs geom_pointrange
#' @importFrom ggplot2 element_blank scale_color_manual scale_x_continuous
#' @noRd
#' @export
plot.fs_backward_model <- function(x, notch = TRUE, ...) {

  check_complete(x)

  # progress mean/95% CI
  restbl     <- x$cross_val$search_progress
  top_single <- setdiff(x$candidate_features, restbl$elim_features)
  signal_info(
    "The top single feature model is:", value(get_seq(top_single))
  )

  # complete cost tables
  csttbl     <- x$cross_val$cost_tables
  row_nms <- list(fold = paste0("Fold", seq(x$folds)),
                  run  = paste0("Run", seq(x$runs))) |>
    expand.grid(stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  row_nms <- paste0(row_nms$fold, "_", row_nms$run)

  bxtbl <- liter(restbl$step, restbl$elim_features, function(.x, .y) {
      as.numeric(csttbl[[.x]][[.y]]) # convert matrix to vector
    }) |>
    data.frame(row.names = row_nms) |>
    setNames(ifelse(is_seq(restbl$elim_features),
                    get_seq(restbl$elim_features), restbl$elim_features))

  box_cols <- rep(col_palette$lightgrey, nrow(restbl))
  idx      <- get_peak_wilcox(bxtbl, type = "back")
  tmp_col  <- c("red",  # box colors by Wilcox signed rank test
                col_palette$purple, col_palette$lightgreen)

  for ( i in seq_along(idx) ) {
    box_cols[idx[i]] <- tmp_col[i]
  }

  x_lab <- paste("features removed", symbl$arrow_right)

  p1 <- bxtbl |>
    beeswarm(
      main = sprintf("Median %s\nWilcoxon Signed-Rank Peak Criterion",
                     x$cost_fxn$display_name),
      y.lab = x$cost_fxn$display_name, x.lab = x_lab,
      notch = notch, cols = box_cols, ...)

  idx     <- get_peak_se(bxtbl, type = "back")
  ci_cols <- c(Peak      = "red",   # line dots by mean - 1se; mean - 1.96se
               Features  = col_palette$lightgrey,
               "1*se"    = col_palette$purple,
               "1.96*se" = col_palette$lightgreen)

  restbl$id                               <- "Features"
  restbl$id[restbl$step == idx["max"]]    <- "Peak"
  restbl$id[restbl$step == idx["p0.05"]]  <- "1*se"
  restbl$id[restbl$step == idx["p0.001"]] <- "1.96*se"

  p2 <- restbl |>
    ggplot(aes(step, cost_mean, colour = id)) +
    geom_pointrange(
      aes(ymin = cost_lower_ci95, ymax = cost_upper_ci95),
      size = 0.75, alpha = 0.75) +
    scale_color_manual(values = ci_cols) +
    labs(
      y = x$cost_fxn$display_name, x = x_lab,
      title = sprintf("Mean %s %s 95%% CI\nStandard Error Peak Criterion",
                      x$cost_fxn$display_name, symbl$pm)
      ) +
    scale_x_continuous(
      breaks = restbl$step,
      labels = ifelse(is_seq(restbl$elim_features),
                      get_seq(restbl$elim_features), restbl$elim_features)
    ) +
    theme(legend.title    = element_blank(),
          legend.position = "right",
          axis.text.x     = element_text(angle = 45, hjust = 1))

  withr::with_package("patchwork", p1 + p2)
}
