# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

churn_probability <- function(dataset, customer_id) {

  # Check if customer exists
  if(!(customer_id %in% dataset$CustomerId)){
    stop("Customer ID does not exist in the dataset.")
  }

  # Convert
  dataset[, Exited := as.factor(Exited)]
  dataset[, Gender := as.factor(Gender)]

  # Predict
  prediction <- glm(Exited ~ CreditScore + Gender + Age + Tenure + Balance + NumOfProducts + HasCrCard + IsActiveMember + EstimatedSalary,
                    data = dataset, family = "binomial")

  # Create churn column
  dataset$Churn_Probability <- predict(prediction, type = "response", dataset)

  # Return
  selected_customer <- dataset[CustomerId == customer_id]
  return_probability <- selected_customer$Churn_Probability
  return(return_probability)
}
