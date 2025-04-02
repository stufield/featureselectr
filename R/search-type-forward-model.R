#' Create Model Search Definitions for S3 Methods
#'
#' Functions for creating model type definitions for objects in the Feature
#'   Selection framework. Current available search types/methods are:
#'   \itemize{
#'     \item forward stepwise model
#'     \item backward stepwise model
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
#' @author Stu Field, Kirk DeLisle
#'
#' @examples
#' search_type_forward_model()                    # the default = 20L
#' search_type_forward_model(max_steps = 15L)     # set to 15
#' search_type_forward_model("My Forward Search") # set title
NULL


#' @describeIn search_type
#'   Forward model selection search.
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

  search_progress <- data.frame(step           = numeric(0),
                                cumul_features = character(0),
                                cost           = numeric(0))
  used_candidates <- character(0)
  cost_tables     <- list()

  for ( step in seq_len(x$search_type$max_steps) ) {

    sprintf("Step %i of %s", step, x$search_type$max_steps) |>
      signal_info()

     rem_candidates <- setdiff(x$candidate_features, used_candidates) |>
       set_Names()

     candidate_costs <- lapply(rem_candidates, function(cnd) {
          frmla <- create_form(x$model_type$response,
                               paste(c(used_candidates, cnd)))
          parallel::mclapply(seq_len(x$cross_val$runs), function(r) {
                      x$cross_val$current_run <- r  # tmp element for: .cost .fitmodel
                      lapply(seq_len(x$cross_val$folds), function(f) {
                             x$cross_val$current_fold <- f
                             .cost(.fitmodel(x, frmla = frmla))
                      }) |>
             setNames(sprintf("Fold%s", seq_len(x$folds)))
          }, mc.cores = cores) |>
          setNames(sprintf("Run%s", seq_len(x$runs)))
       })

     # construct results table for selection of this candidate step
     cost_tbl <- lapply(candidate_costs, function(.cnd) {
       vapply(.cnd, unlist, use.names = TRUE,   # passed to base::unlist
              USE.NAMES = TRUE, FUN.VALUE = numeric(x$folds))
     })

     ci95df <- lapply(cost_tbl, calc_CI95) |> do.call(what = "rbind")

     # select the best
     top_idx <- ifelse(x$cost_fxn$maximize,
                       which.max(ci95df$mean),
                       which.min(ci95df$mean))

     ci95top <- ci95df[top_idx, ] # 1 row df
     new_feat <- rownames(ci95top)
     used_candidates <- c(used_candidates, new_feat)

     search_progress <- rbind(search_progress,
                              data.frame(step = step,
                                         cumul_features  = new_feat,
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


#' S3 plot method for fs_forward_model
#'
#' @importFrom ggplot2 ggplot aes theme element_text labs geom_pointrange
#' @importFrom ggplot2 element_blank scale_color_manual scale_x_continuous
#' @noRd
#' @export
plot.fs_forward_model <- function(x, notch = TRUE, ...) {

  check_complete(x)

  # progress mean/95% CI
  restbl <- x$cross_val$search_progress

  # complete cost tables
  csttbl <- x$cross_val$cost_tables

  row_nms <- list(fold = paste0("Fold", seq(x$folds)),
                  run  = paste0("Run", seq(x$runs))) |>
    expand.grid(stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  row_nms <- paste0(row_nms$fold, "_", row_nms$run)

  bxtbl <- liter(restbl$step, restbl$cumul_features, function(.x, .y) {
      as.numeric(csttbl[[.x]][[.y]]) # convert matrix to vector
    }) |>
    data.frame(row.names = row_nms) |>
    setNames(ifelse(is_seq(restbl$cumul_features),
                    get_seq(restbl$cumul_features), restbl$cumul_features))

  box_cols  <- rep(col_palette$lightgrey, nrow(restbl))
  idx       <- get_peak_wilcox(bxtbl)
  tmp_col   <- c("red",   # box colors by Wilcox signed rank test
                 col_palette$purple,
                 col_palette$lightgreen)

  for ( i in seq_along(idx) ) {
    box_cols[idx[i]] <- tmp_col[i]
  }

  x_lab <- paste("features added", symbl$arrow_right)

  p1 <- bxtbl |>
    beeswarm(
      main = sprintf("Median %s\nWilcoxon Signed-Rank Peak Criterion",
                     x$cost_fxn$display_name),
      y.lab = x$cost_fxn$display_name, x.lab = x_lab,
      notch = notch, cols = box_cols, ...
    )

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
    ggplot(aes(step, cost_mean, colour = id)) +
    geom_pointrange(
      aes(ymin = cost_lower_ci95, ymax = cost_upper_ci95),
      size = 0.75, alpha = 0.7) +
    scale_color_manual(values = ci_cols) +
    labs(
      y = x$cost_fxn$display_name, x = x_lab,
      title = sprintf("Mean %s %s 95%% CI\nStandard Error Peak Criterion",
                      x$cost_fxn$display_name, symbl$pm)
    ) +
    scale_x_continuous(
      breaks = restbl$step,
      labels = ifelse(is_seq(restbl$cumul_features),
                      get_seq(restbl$cumul_features), restbl$cumul_features)
    ) +
    theme(legend.title    = element_blank(),
          legend.position = "right",
          axis.text.x     = element_text(angle = 45, hjust = 1))

  withr::with_package("patchwork", p1 + p2)
}


#' @importFrom ggplot2 geom_jitter geom_boxplot position_jitter aes ggplot
#' @importFrom ggplot2 scale_fill_manual
#' @noRd
beeswarm <- function(.data, label, main, notch = TRUE,
                     y.lab, x.lab, cols, ...) {

  plot_df <- data.frame(lapply(.data, "length<-", max(lengths(.data))),
                        check.names = FALSE)
  gg_df <- tidyr::gather(plot_df, key = group, na.rm = TRUE)
  gg_df$group <- factor(gg_df$group, levels = names(plot_df))

  p <- gg_df |>
    ggplot(aes(x = group, y = value, fill = group)) +
    geom_boxplot(notch = notch, alpha = 0.65, outlier.color = NA, ...) +
    geom_jitter(position = position_jitter(width = 0.05,  seed = 101),
                alpha = 0.5, shape = 21, fill = "black", size = 2.5) +
    labs(x = x.lab, y = y.lab, title = main) +
    theme(legend.position = "none",
          axis.text.x     = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = cols)
  p
}
