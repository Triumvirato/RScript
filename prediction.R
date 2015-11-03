library(kernlab)
training = read.csv("training.csv")
testing = read.csv("testing.csv")

training[[1]] = NULL
testing[[1]] = NULL

bug_classifier = ksvm(bug.fixing.time ~ ., data = training, kernel = "vanilladot",scaled=FALSE,cache=100)
bug_predictions = predict(bug_classifier, testing)
#table(bug_predictions,testing_bug.fixing.time)
agreement = bug_predictions == testing$bug.fixing.time
prop.table(table(agreement))