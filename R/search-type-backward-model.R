
#' @describeIn search_type
#'   Backward model selection search
#' @export
search_type_backward_model <- function(display_name = "Backward Stepwise Model Search") {
  as.list(environment()) |> add_class("fs_backward_model")
}

#' Backward search type for feature selection models
#' @noRd
#' @importFrom stats as.formula setNames
#' @export
Search.fs_backward_model <- function(x, ...) {

  signal_rule("Using `Backward-Stepwise` model search", line_col = "magenta")
  cores <- list(...)$num_cores
  withr::local_options(list(stringsAsFactors = FALSE))

  # assume a certain number of runs within which there
  #   are cross-validated folds
  #   loop over r, f, and step ( runs, folds, and candidates )
  #   for model search, the cross-validated folds determine
  #   which parameter is chosen at any given step

  search_progress <- data.frame(step         = numeric(0),
                                elim_markers = character(0),
                                cost         = numeric(0))
  deleted_candidates <- character(0)
  models             <- list()
  cost_tables        <- list()

  #add the full model as the first step
  mod_candidate_markers <- c("Full_Model", x$candidate_markers)

  for ( step in seq_along(x$candidate_markers) ) {

    sprintf("Step %i of %i",
            #length(x$candidate_markers) + 1 - step,
            step, length(x$candidate_markers)) |>
      signal_rule(line_col = "blue")

    rem_candidates <- setdiff(mod_candidate_markers, deleted_candidates)

    # reformulated search as a loop to facilitate a full model step
    candidate.models <- list()

    for ( cnd in rem_candidates ) {
      rem_minus_one <- setdiff(rem_candidates, cnd)
      frmla <- create_form(x$model_type$response, paste(rem_minus_one))
      #run_res <- foreach ( r = 1:x$cross_val$runs ) %dopar% {
      run_res <- parallel::mclapply(seq_len(x$cross_val$runs), function(r) {
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

         candidate_models[[cnd]] <- run_res
         if ( cnd == "Full_Model" ) {
            break
         }
      }

      # construct results table for selection of this candidate step
      cost_table <- lapply(names(candidate_models), function(cnd) {
                           sapply(names(candidate_models[[cnd]]), function(r) {
                                  sapply(names(candidate_models[[cnd]][[r]]), function(f) {
                                         candidate_models[[cnd]][[r]][[f]]$cost
                                 })
                           })
                     }) |>
        setNames(names(candidate_models))

      ci95df <- sapply(cost_table, calc_CI95) |> t() |> data.frame()

      # select the worst
      if ( x$cost_fxn$maximize ) {
         top_idx <- which.max(ci95df$mean)
      } else {
         top_idx <- which.min(ci95df$mean)
      }

      #print(top_idx)
      ci95top <- ci95df[top_idx, ]      # select "top" row; 1 row df with rowname
      new_par <- rownames(ci95top)
      deleted_candidates <- c(deleted_candidates, new_par)

      search_progress <- rbind(search_progress,
                               data.frame(step = step,
                                          elim_markers = new_par,
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



#' S3 plot method for fs_backward_model
#'
#' Plotting for `fs_backward_model` class.
#'
#' @noRd
#' @importFrom ggplot2 ggplot aes theme element_text labs geom_pointrange
#' @importFrom ggplot2 element_blank scale_color_manual scale_x_continuous
#' @export
plot.fs_backward_model <- function(x, box.col = 8, ...) {

  check_complete(x)

  max_steps <- length(x$candidate_markers)

  # progress mean/95% CI
  restbl     <- x$cross_val$search_progress
  top_single <- setdiff(x$candidate_markers, restbl$elim_markers)
  signal_info(
    "The top single marker model contains:", value(top_single)
  )

  # complete cost tables
  csttbl     <- x$cross_val$cost_tables
  row_nms <- list(fold = paste0("Fold", seq(x$folds)),
                  run  = paste0("Run", seq(x$runs))) |>
    expand.grid(stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  row_nms <- paste0(row_nms$fold, "_", row_nms$run)
  #print(row_nms)

  bxtbl <- liter(restbl$step, restbl$elim_markers, function(.x, .y) {
      as.numeric(csttbl[[.x]][[.y]]) # matrix -> vector
    }) |>
    data.frame(row.names = row_nms) |>
    setNames(ifelse(is_seq(restbl$elim_markers),
                    get_seq(restbl$elim_markers), restbl$elim_markers))

  names(bxtbl)[1L] <- "Full Model"  # rename pos 1 = "Full Model"

  box_cols <- rep("grey", nrow(restbl))
  idx      <- get_peak_wilcox(bxtbl, type = "back")
  tmp_col  <- c("red",  # box colors by Wilcox signed rank test
                col_palette$purple, col_palette$lightgreen)

  for ( i in 1:length(idx) ) {
    box_cols[idx[i]] <- tmp_col[i]
  }

  p1 <- bxtbl |>
    SomaPlotr::boxplotBeeswarm(
      main = sprintf("Median %s\nWilcoxon Signed-Rank Peak Criterion",
                     x$cost_fxn$display_name),
      y.lab = x$cost_fxn$display_name,
      x.lab = paste(symbl$arrow_left, "features removed"),
      notch = TRUE, cols = box_cols, ...) +
    ggplot2::theme(legend.position = "none",
                   axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)
    )

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
    ggplot2::ggplot(ggplot2::aes(step, cost_mean, colour = id)) +
    ggplot2::geom_pointrange(
      ggplot2::aes(ymin = cost_lower_ci95, ymax = cost_upper_ci95),
      size = 0.75, alpha = 0.75) +
    ggplot2::scale_colour_manual(values = ci_cols) +
    ggplot2::labs(
      y = x$cost_fxn$display_name,
      x = paste(symbl$arrow_left, "features removed"),
      title = sprintf("Mean %s %s 95%% CI\nStandard Error Peak Criterion",
                      x$cost_fxn$display_name, symbl$pm)
      ) +
    ggplot2::scale_x_continuous(
      breaks = restbl$step,
      labels = ifelse(is_seq(restbl$elim_markers),
                      get_seq(restbl$elim_markers), restbl$elim_markers)
    ) +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   legend.position = "right",
                   axis.text.x  = ggplot2::element_text(angle = 90, hjust = 1))

  withr::with_package("patchwork", p1 + p2)
}
