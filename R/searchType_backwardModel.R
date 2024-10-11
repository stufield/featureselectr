
#' @describeIn searchType
#' Backwards model selection search
#' @export
searchType_backwardModel <- function(display.name = "Backward Stepwise Model Search") {
  as.list(environment()) |> add_class("fs_backward_model")
}


#' S3 Backward Search Type
#'
#' Backward search type for feature selection models
#'
#' @noRd
#' @importFrom stats as.formula setNames
#' @export
Search.fs_backward_model <- function(x, ...) {

  writeLines(
    signal_rule("Using `Backward-Stepwise` model search", line_col = "magenta")
  )
  cores <- list(...)$num.cores
  op <- options(stringsAsFactors = FALSE)
  on.exit(options(op))

  # assume a certain number of runs within which there
  # are cross-validated folds
  # loop over r, f, and step ( runs, folds, and candidates )
  # for model search, the cross-validated folds determine
  # which parameter is chosen at any given step

  search.progress <- data.frame(step         = numeric(0),
                                elim.markers = character(0),
                                cost         = numeric(0))
  deleted.candidates <- character(0)
  models             <- list()
  cost.tables        <- list()

  #add the full model as the first step
  mod.candidate.markers <- c("Full_Model", x$candidate.markers)

  for ( step in seq_along(x$candidate.markers) ) {

    sprintf("Step %i of %i",
            #length(x$candidate.markers) + 1 - step,
            step, length(x$candidate.markers)) |>
      signal_rule(line_col = "blue") |>
      writeLines()

    rem.candidates <- setdiff(mod.candidate.markers, deleted.candidates)

    # reformulated search as a loop to facilitate a full model step
    candidate.models <- list()

    for ( cnd in rem.candidates ) {
      rem_minus_one <- setdiff(rem.candidates, cnd)
      frmla <- sprintf("%s ~ %s",
                       x$model.type$response,
                       paste(rem_minus_one, collapse = " + ")) |> as.formula()
      #run.res <- foreach ( r = 1:x$cross.val$runs ) %dopar% {
      run.res <- parallel::mclapply(1:x$cross.val$runs, function(r) {
                                    run <- sprintf("Run%d", r)
                                    x$cross.val$current.run <- r
                                    lapply(1:x$cross.val$folds, function(f) {
                                           x$cross.val$current.fold <- f
                                           fold    <- sprintf("Fold%d", f)
                                           mod     <- fitmodel(x, frmla = frmla)
                                           cst     <- cost(mod)
                                           mod.out <- if (x$keep.models)
                                              mod$cross.val[[run]][[fold]]$model
                                           else
                                             NULL
                                           list(cost = cst, model = mod.out)
                        }) |>
              setNames(sprintf("Fold%s", 1:x$cross.val$folds))
        }, mc.cores = cores) |>
      setNames(sprintf("Run%s", 1:x$cross.val$runs))
      #} |> setNames(sprintf("Run%s", 1:x$cross.val$runs))

         candidate.models[[cnd]] <- run.res
         if ( cnd == "Full_Model" ) {
            break
         }
      }

      # construct results table for selection of this candidate step
      cost.table <- lapply(names(candidate.models), function(cnd) {
                           sapply(names(candidate.models[[cnd]]), function(r) {
                                  sapply(names(candidate.models[[cnd]][[r]]), function(f) {
                                         candidate.models[[cnd]][[r]][[f]]$cost
                                 })
                           })
                     }) |>
        setNames(names(candidate.models))

      ci95df <- sapply(cost.table, calcCI95se) |> t() |> data.frame()

      # select the worst
      if ( x$cost.fxn$maximize ) {
         top_idx <- which.max(ci95df$mean)
      } else {
         top_idx <- which.min(ci95df$mean)
      }

      #print(top_idx)
      ci95top <- ci95df[ top_idx, ]      # select "top" row; 1 row df with rowname
      new.par <- rownames(ci95top)
      deleted.candidates <- c(deleted.candidates, new.par)

      search.progress <- rbind(search.progress,
                               data.frame(step = step,
                                          elim.markers = new.par,
                                          cost.lower.ci95 = ci95top$lower,
                                          cost.mean = ci95top$mean,
                                          cost.upper.ci95 = ci95top$upper))

      step_name <- sprintf("Step_%d", step)
      cost.tables[[step_name]] <- cost.table
   }

   # keep results of search
   x$cross.val$search.progress <- search.progress
   x$cross.val$cost.tables     <- cost.tables

   # update iterators
   x$cross.val$current.run  <- x$runs
   x$cross.val$current.fold <- x$folds

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

  max.steps <- length(x$candidate.markers)

  # progress mean/95% CI
  restbl     <- x$cross.val$search.progress
  top_single <- setdiff(x$candidate.markers, restbl$elim.markers)
  signal_info(
    "The top single marker model contains:", value(top_single)
  )

  # complete cost tables
  csttbl     <- x$cross.val$cost.tables
  row_nms <- list(fold = paste0("Fold", seq(x$folds)),
                  run  = paste0("Run", seq(x$runs))) |>
    expand.grid(stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  row_nms <- paste0(row_nms$fold, "_", row_nms$run)
  #print(row_nms)

  bxtbl <- liter(restbl$step, restbl$elim.markers, function(.x, .y) {
      as.numeric(csttbl[[.x]][[.y]]) # matrix -> vector
    }) |>
    data.frame(row.names = row_nms) |>
    setNames(ifelse(is.apt(restbl$elim.markers),
                    getSeqId(restbl$elim.markers), restbl$elim.markers))

  names(bxtbl)[1L] <- "Full Model"  # rename pos 1 = "Full Model"

  box_cols <- rep("grey", nrow(restbl))
  idx      <- getPeakWilcox(bxtbl, type = "back")
  tmp_col  <- c("red",  # box colors by Wilcox signed rank test
                SomaPlotr::soma_colors$purple,
                SomaPlotr::soma_colors$lightgreen)

  for ( i in 1:length(idx) ) {
    box_cols[idx[i]] <- tmp_col[i]
  }

  p1 <- bxtbl |>
    SomaPlotr::boxplotBeeswarm(
      main = sprintf("Median %s\nWilcoxon Signed-Rank Peak Criterion",
                     x$cost.fxn$display.name),
      y.lab = x$cost.fxn$display.name,
      x.lab = paste(symbl$arrow_left, "features removed"),
      notch = TRUE, cols = box_cols, ...) +
    ggplot2::theme(legend.position = "none",
                   axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)
    )

  idx     <- getPeakSE(bxtbl, type = "back")
  ci_cols <- c(Peak      = "red",   # line dots by mean - 1se; mean - 1.96se
               Features  = SomaPlotr::soma_colors$lightgrey,
               "1*se"    = SomaPlotr::soma_colors$purple,
               "1.96*se" = SomaPlotr::soma_colors$lightgreen)

  restbl$id                               <- "Features"
  restbl$id[restbl$step == idx["max"]]    <- "Peak"
  restbl$id[restbl$step == idx["p0.05"]]  <- "1*se"
  restbl$id[restbl$step == idx["p0.001"]] <- "1.96*se"

  p2 <- restbl |>
    ggplot2::ggplot(ggplot2::aes(step, cost.mean, colour = id)) +
    ggplot2::geom_pointrange(
      ggplot2::aes(ymin = cost.lower.ci95, ymax = cost.upper.ci95),
      size = 0.75, alpha = 0.75) +
    ggplot2::scale_colour_manual(values = ci_cols) +
    ggplot2::labs(
      y = x$cost.fxn$display.name,
      x = paste(symbl$arrow_left, "features removed"),
      title = sprintf("Mean %s %s 95%% CI\nStandard Error Peak Criterion",
                      x$cost.fxn$display.name, symbl$pm)
      ) +
    SomaPlotr::theme_soma(legend.position = "right") +
    ggplot2::scale_x_continuous(
      breaks = restbl$step,
      labels = ifelse(is.apt(restbl$elim.markers),
                      getSeqId(restbl$elim.markers), restbl$elim.markers)
    ) +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   axis.text.x  = ggplot2::element_text(angle = 90, hjust = 1))

  gridExtra::grid.arrange(p1, p2, ncol = 2)
}
