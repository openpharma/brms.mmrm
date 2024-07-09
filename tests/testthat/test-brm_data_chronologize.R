test_that("brm_data_chronologize() with class", {
  data <- brm_simulate_outline(n_time = 12, n_patient = 4)
  data$time <- gsub("_0", "_", data$time)
  attr(data, "brm_reference_time") <- "time_1"
  data$AVISITN <- as.integer(gsub("time_", "", data$time))
  data[, c("time", "AVISITN")]
  data1 <- brm_data_chronologize(data, time = "time", order = "AVISITN")
  levels <- paste0("time_", seq_len(12))
  expect_equal(as.character(sort(unique(data1$time))), levels)
  data2 <- brm_data_chronologize(data, time = "time", levels = levels)
  expect_equal(as.character(sort(unique(data2$time))), levels)
})

test_that("brm_data_chronologize() without class", {
  data <- brm_simulate_outline(n_time = 12, n_patient = 4)
  data <- tibble::as_tibble(data)
  data$AVISIT <- gsub("_0", "_", data$time)
  data$AVISITN <- as.integer(gsub("time_", "", data$time))
  data[, c("AVISIT", "AVISITN")]
  data1 <- brm_data_chronologize(data, time = "AVISIT", order = "AVISITN")
  levels <- paste0("time_", seq_len(12))
  expect_equal(as.character(sort(unique(data1$AVISIT))), levels)
  data2 <- brm_data_chronologize(data, time = "AVISIT", levels = levels)
  expect_equal(as.character(sort(unique(data2$AVISIT))), levels)
})
