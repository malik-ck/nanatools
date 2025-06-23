
### NOTE: There is a bug where, without outer CV, one gets two identical ensembles or something.
### NOTE: loss() and marginals() might need to be adapted to new structure.

quick_sim <- function() {

  # Schnell simulierte Daten...
  test_params <- c(0.5, rnorm(19, 0.1, 0.3))
  test_params_tx <- rnorm(19, -0.1, 0.25)
  treatment_and_confounders <- mvrnorm(1000, rep(0, 20), randcorr(20))
  treatment_and_confounders[,1] <- rbinom(nrow(treatment_and_confounders), 1, plogis(0.2 + (treatment_and_confounders[,2:20] %*% test_params_tx)))
  confounders <- treatment_and_confounders[,-1]
  treatment <- treatment_and_confounders[,1]
  outcome <- rnorm(1000, -0.3 + treatment_and_confounders %*% test_params, 1)
  colnames(treatment_and_confounders) <- LETTERS[1:20]

  # So kriegt man eine Liste an Learnern. Ich implementiere gleich noch einige (Custom ist auch sehr einfach).
  # Für outcome und treatment werden jeweils der empirische Mittelwert und GLMs benutzt, für das outcome
  # dabei ein GLM mit Gaussian likelihood genutzt, für treatment binomial.
  get_learner_list_y <- list(lrn_mean("Mean"), lrn_glm("GLM", gaussian()))
  get_learner_list_a <- list(lrn_mean("Mean"), lrn_glm("GLM", binomial()))

  # Spezifiziert nested 10x10 CV. Für deine kleinere Studie wäre vielleicht auch 20x20 angebracht, oder mehrere Wiederholungen
  # von 10x10 mit Pooling hinterher. list(Selector = "select") gibt den Metalearner an; da kann man auch z.B.
  # list(Selector = "select", Superlearner = "NNLogLik") angeben, dann kriegt man die Auswahl zwischen dem einzelnen besten Learner
  # und dem weighted ensemble. Ich würde tatsächlich das empfehlen, was unten steht; Das weighted ensemble braucht super lang.
  lrn_setup_y <- learner_setup(get_learner_list_y, outer_cv = 10, inner_cv = 10, "gaussian", list(Selector = "select"))
  lrn_setup_a <- learner_setup(get_learner_list_a, outer_cv = 10, inner_cv = 10, "gaussian", list(Selector = "select"))

  # Hier der call um zu fitten. Benutzt standardmäßig Funktionen aus future_apply, also falls du multithreading aufgesetzt hast
  # (z.B. über plan(multisession, workers = future::availableCores()), dann geht das schneller, geht aber auch auf Arbeitsspeicher.
  test_fit_a <- lazy_cv(confounders, treatment, lrn_setup_a)
  test_fit_y <- lazy_cv(treatment_and_confounders, outcome, lrn_setup_y)

  # Hier wird dann der TMLE geschätzt. trim_ipw = "gruber" implementiert unseren Ansatz zur data-adaptive PS-Trunkierung wie im Masterprotokoll.
  test_results <- fwb_tmle_bin(test_fit_a, test_fit_y, "A", trim_ipw = "gruber", n_bstrap = 1000)
  return(ifelse(test_results$results[3,6] < 0.5 & test_results$results[3,7] > 0.5, 1, 0))

}

test <- replicate(1000, quick_sim())


data(airquality)
airquality <- airquality[,-c(5,6)]
airquality$Temp <- ifelse(airquality$Temp > 80, 1, 0)
airquality_mice <- mice(airquality)

mi_fit_a <- lazy_cv_mi(airquality_mice, "Temp", c("Solar.R", "Wind"), lrn_setup_a)
mi_fit_y <- lazy_cv_mi(airquality_mice, "Ozone", NULL, lrn_setup_y)

get_test_mi_results <- fwb_tmle_bin_mi(mi_fit_wthem, mi_fit_y, "Temp", n_bstrap = 200)
