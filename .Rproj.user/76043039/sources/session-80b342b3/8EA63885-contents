# Function for rounding
num_f <- function(x, deci = 2){format(round(x, deci), nsmall = deci)}


# Replacing the ranger function in `SuperLearner` with one that has variable importance
SL.ranger <- function (Y, X, newX, family, obsWeights, num.trees = 500, mtry = floor(sqrt(ncol(X))), 
                       write.forest = TRUE, probability = family$family == "binomial", 
                       min.node.size = ifelse(family$family == "gaussian", 
                                              5, 1), replace = TRUE, sample.fraction = ifelse(replace, 
                                                                                              1, 0.632), num.threads = 1, verbose = T, ...) 
{
  require("ranger")
  if (family$family == "binomial") {
    Y = as.factor(Y)
  }
  if (is.matrix(X)) {
    X = data.frame(X)
  }
  fit <- ranger::ranger(`_Y` ~ ., data = cbind(`_Y` = Y, 
                                               X), num.trees = num.trees, mtry = mtry, min.node.size = min.node.size, 
                        replace = replace, sample.fraction = sample.fraction, 
                        case.weights = obsWeights, write.forest = write.forest, 
                        probability = probability, num.threads = num.threads, 
                        verbose = verbose, importance = 'impurity') # (KG 12Oct2023: added importance option)
  pred <- predict(fit, data = newX)$predictions
  if (family$family == "binomial") {
    pred = pred[, "1"]
  }
  fit <- list(object = fit, verbose = verbose)
  class(fit) <- c("SL.ranger")
  out <- list(pred = pred, fit = fit)
  return(out)
}

