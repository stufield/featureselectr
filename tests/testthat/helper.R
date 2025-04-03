
# set up a general sample feature_select object
data <- wranglr::simdata
data$class_response <- factor(data$class_response)

fs_obj <- feature_selection(
  data, candidate_features = helpr:::get_analytes(data),
  model_type  = model_type_lr("class_response"),
  search_type = search_type_forward_model("Feature Selection Algorithm", 15L),
  runs = 5L, folds = 5L
)
