
test_data <- matrix(rnorm(20 * 1000), ncol = 20)
test_params <- rnorm(20, 0, 0.3)
test_outcome <- rnorm(1000, -0.3 + test_data %*% test_params, 3)

get_learner_list <- list(
  lrn_mean("Mean"), lrn_glm("GLM", gaussian())
)

lrn_setup <- learner_setup(get_learner_list, 4, 4, "gaussian", list(NNLogLik = "NNLogLik"))

test_fit <- lazy_cv(test_data, test_outcome, lrn_setup)
