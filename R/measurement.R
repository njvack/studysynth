# Measurement models: how items relate to latent variables.
#
# Each block in `measurement:` holds lavaan syntax with every loading fixed:
#
#   dep =~ 0.78*phq9_1 + 0.81*phq9_2 + ...
#   phq9_3 ~~ 0.15*phq9_4
#
# Loadings are standardized. For each item we build a continuous "latent
# response" y* = loadings %*% latents + error, with error variance chosen so
# y* has variance 1 for a person with unit-variance latents, then cut y* at
# the item's thresholds to get a response category.

parse_measurement <- function(block) {
  table <- lavaan::lavaanify(block$model)
  loadings <- dplyr::filter(table, op == "=~")
  factors <- unique(loadings$lhs)
  items <- unique(loadings$rhs)

  lambda <- matrix(0, length(items), length(factors), dimnames = list(items, factors))
  lambda[cbind(loadings$rhs, loadings$lhs)] <- loadings$ustart

  # Residual covariances between items (not latents, not variances).
  covs <- dplyr::filter(table, op == "~~", lhs != rhs, lhs %in% items, rhs %in% items)
  theta <- diag(pmax(1 - rowSums(lambda^2, na.rm = TRUE), 0), length(items))
  dimnames(theta) <- list(items, items)
  theta[cbind(covs$lhs, covs$rhs)] <- covs$ustart
  theta[cbind(covs$rhs, covs$lhs)] <- covs$ustart

  list(
    factors = factors,
    items = items,
    lambda = lambda,
    theta = theta,
    unfixed = loadings$rhs[is.na(loadings$ustart)],
    factor_covs = dplyr::filter(table, op == "~~", lhs != rhs, lhs %in% factors)
  )
}

item_thresholds <- function(block, item, n_choices) {
  block$thresholds[[item]] %||% block$thresholds$default %||% even_thresholds(n_choices)
}

# Returns a tibble of response codes, one column per item.
simulate_items <- function(block, project, latent_values) {
  model <- parse_measurement(block)
  n <- nrow(latent_values)
  eta <- as.matrix(latent_values[model$factors])
  errors <- MASS::mvrnorm(n, rep(0, length(model$items)), model$theta)
  y_star <- eta %*% t(model$lambda) + matrix(errors, nrow = n)

  purrr::map(model$items, \(item) {
    choices <- field_info(project, item)$choices[[1]]
    cuts <- item_thresholds(block, item, nrow(choices))
    category <- findInterval(y_star[, item], cuts) + 1
    choices$code[category]
  }) |>
    purrr::set_names(model$items) |>
    tibble::as_tibble()
}
