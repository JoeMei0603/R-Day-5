source("R\\churnProbability.R")

test_that("Max is always lower than min", {
  library(data.table)
  personal_data <- fread("data_personal.csv")
  customer_data <- fread("data_customer.csv")
  merged_data <- merge(personal_data, customer_data, by="CustomerId", all = TRUE)

  max_probability <- churn_probability(merged_data, 15653251)
  min_probability <- churn_probability(merged_data, 15662641)
  expect_lt(min_probability, max_probability)
})
