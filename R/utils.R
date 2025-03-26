
get_seq <- function(x) sub("\\.", "-", sub("^seq\\.", "", x))
is_seq  <- getFromNamespace("is_seq", "helpr")

col_palette <- list(
  purple     = "#24135F",
  lightgreen = "#00A499",
  lightgrey  = "#707372",
  magenta    = "#840B55",
  lightblue  = "#006BA6",
  yellow     = "#D69A2D",
  darkgreen  = "#007A53",
  darkblue   = "#1B365D",
  darkgrey   = "#54585A",
  blue       = "#004C97"
)

calc_auc <- function(truth, predicted) {
  if ( !is.factor(truth) ) {
    truth <- as.factor(truth)
  }
  levs <- levels(truth)
  tab <- table(truth)
  stopifnot("`truth` is not binary." = length(tab) == 2L)
  idx <- lapply(as.factor(levs), function(.x) which(truth == .x))
  auc <- 0.5
  c1 <- 1L
  c2 <- 2L
  n1 <- as.numeric(tab[levs[c1]])
  n2 <- as.numeric(tab[levs[c2]])
  if ( n1 > 0 && n2 > 0 ) {
    r <- rank(c(predicted[idx[[c1]]], predicted[idx[[c2]]]))
    auc <- (sum(r[1:n1]) - n1 * (n1 + 1) / 2) / (n1 * n2)
  }
  max(auc, 1 - auc)
}
