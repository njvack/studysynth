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
# (visits and experience sampling prompts). Later blocks can use earlier
# latents as predictors, which is how latents get correlated.

add_latents <- function(spec, occasions) {
  for (name in names(spec$latents)) {
    occasions[[name]] <- simulate_latent(spec$latents[[name]], occasions)
  }
  occasions
}

simulate_latent <- function(block, occasions) {
  formula <- stats::as.formula(block$formula)

  x <- fixed_design(formula, occasions)
  fixef <- unlist(block$fixef)
  value <- as.vector(x %*% fixef[colnames(x)])

  for (bar in lme4::findbars(formula)) {
    value <- value + random_part(bar, block$ranef, occasions)
  }

  value + residuals_for(block, occasions)
}

fixed_design <- function(formula, data) {
  fixed <- stats::delete.response(stats::terms(lme4::nobars(formula)))
  stats::model.matrix(fixed, data)
}

# One random-effect term, like (1 + time | record_id).
random_part <- function(bar, ranef, occasions) {
  group_name <- deparse(bar[[3]])
  z <- stats::model.matrix(stats::as.formula(paste("~", deparse(bar[[2]]))), occasions)
  groups <- do.call(paste, c(occasions[all.vars(bar[[3]])], sep = ":"))

  covariance <- ranef_covariance(ranef[[group_name]], colnames(z))
  unique_groups <- unique(groups)
  effects <- MASS::mvrnorm(length(unique_groups), rep(0, ncol(z)), covariance)
  effects <- matrix(effects, nrow = length(unique_groups))
  rowSums(z * effects[match(groups, unique_groups), , drop = FALSE])
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
