# Latent variables: how imaginary people differ and change.
#
# Each block in `latents:` is a mixed model written like lme4:
#
#   dep:
#     formula: dep ~ time * rand_group + (1 + time | record_id)
#     fixef: {"(Intercept)": 0, time: -0.02, rand_group2: 0, "time:rand_group2": -0.06}
#     ranef:
#       record_id: {sd: {"(Intercept)": 0.9, time: 0.04}, cor: -0.3}
#     sigma: 0.5
#
# We compute fixed part + random effects + residual on every occasion
# (visits and experience sampling prompts). Latents get correlated two ways:
# later blocks can use earlier latents as predictors, and factor
# correlations in measurement blocks (pa ~~ -0.2*na) correlate people's
# random intercepts.

add_latents <- function(spec, occasions) {
  shared <- shared_intercepts(spec, unique(occasions$record_id))
  for (name in names(spec$latents)) {
    occasions[[name]] <- simulate_latent(spec$latents[[name]], occasions, shared[[name]])
  }
  occasions
}

# Factor correlations (pa ~~ -0.2*na in a measurement block) correlate
# people's random intercepts: someone with a high typical PA tends to have a
# lower typical NA. We draw correlated standard normals here, one column per
# latent, and random_part() scales them by each latent's intercept SD.
shared_intercepts <- function(spec, ids) {
  r <- latent_correlations(spec)
  if (is.null(r)) return(list())
  z <- matrix(MASS::mvrnorm(length(ids), rep(0, ncol(r)), r), nrow = length(ids))
  purrr::map(purrr::set_names(colnames(r)), \(name) purrr::set_names(z[, which(colnames(r) == name)], ids))
}

simulate_latent <- function(block, occasions, shared = NULL) {
  formula <- stats::as.formula(block$formula)

  x <- fixed_design(formula, occasions)
  fixef <- unlist(block$fixef)
  value <- as.vector(x %*% fixef[colnames(x)])

  for (bar in lme4::findbars(formula)) {
    value <- value + random_part(bar, block$ranef, occasions, shared)
  }

  value + residuals_for(block, occasions)
}

fixed_design <- function(formula, data) {
  fixed <- stats::delete.response(stats::terms(lme4::nobars(formula)))
  stats::model.matrix(fixed, data)
}

# One random-effect term, like (1 + time | record_id).
random_part <- function(bar, ranef, occasions, shared = NULL) {
  group_name <- deparse(bar[[3]])
  z <- stats::model.matrix(stats::as.formula(paste("~", deparse(bar[[2]]))), occasions)
  groups <- do.call(paste, c(occasions[all.vars(bar[[3]])], sep = ":"))

  covariance <- ranef_covariance(ranef[[group_name]], colnames(z))
  unique_groups <- unique(groups)
  # Standard normals turned into effects with the right covariance. The
  # Cholesky factor's first row is (intercept SD, 0, ...), so a shared
  # correlated normal in the first column becomes the person's intercept.
  normals <- matrix(stats::rnorm(length(unique_groups) * ncol(z)), ncol = ncol(z))
  if (!is.null(shared) && group_name == "record_id" && colnames(z)[1] == "(Intercept)") {
    normals[, 1] <- shared[unique_groups]
  }
  effects <- normals %*% chol_or_sqrt(covariance)
  rowSums(z * effects[match(groups, unique_groups), , drop = FALSE])
}

# Upper Cholesky factor, falling back to a symmetric square root when some
# SDs are zero (chol() refuses those).
chol_or_sqrt <- function(covariance) {
  tryCatch(chol(covariance), error = \(e) {
    eig <- eigen(covariance, symmetric = TRUE)
    t(eig$vectors %*% diag(sqrt(pmax(eig$values, 0)), nrow(covariance)))
  })
}

# Turns {sd: ..., cor: ...} into a covariance matrix. `sd` can be one number
# (one term) or named by term; `cor` can be one number (two terms) or a matrix.
ranef_covariance <- function(re, terms) {
  sd <- unlist(re$sd)
  if (is.null(names(sd))) names(sd) <- terms
  sd <- sd[terms]
  cor <- re$cor %||% diag(length(terms))
  if (is.list(cor)) cor <- do.call(rbind, lapply(cor, unlist))
  if (length(cor) == 1 && length(terms) == 2) cor <- matrix(c(1, cor, cor, 1), 2)
  diag(sd, length(sd)) %*% as.matrix(cor) %*% diag(sd, length(sd))
}

# Occasion-level noise. With `autocor_per_hour`, noise carries over between
# nearby prompts and fades with the hours between them.
residuals_for <- function(block, occasions) {
  sigma <- block$sigma %||% 0
  rho <- block$autocor_per_hour
  n <- nrow(occasions)
  if (is.null(rho)) {
    return(stats::rnorm(n, 0, sigma))
  }
  out <- numeric(n)
  hours <- as.numeric(occasions$datetime) / 3600
  for (rows in split(seq_len(n), occasions$record_id)) {
    rows <- rows[order(hours[rows])]
    previous <- 0
    previous_hour <- -Inf
    for (i in rows) {
      carry <- rho^(hours[i] - previous_hour)
      previous <- carry * previous + sqrt(1 - carry^2) * stats::rnorm(1, 0, sigma)
      previous_hour <- hours[i]
      out[i] <- previous
    }
  }
  out
}

# Variables formulas can use, besides covariates, groups, and earlier latents.
occasion_variables <- c("time", "day", "hour", "sin_tod", "cos_tod", "weekend", "ema", "record_id")
