library(RTextTools)
library(topicmodels)

data <- read_data(system.file("data/NYTimes.csv.gz",package="RTextTools"), type="csv", sep = ";")

matrix <- create_matrix(cbind(data["Title"],data["Subject"]), language="english", removeNumbers=TRUE, stemWords=TRUE, weighting=weightTf)

k <- length(t(unique(data["Topic.Code"])))

lda <- LDA(matrix, k, method="VEM")

beta <- attr(lda, "beta")
alpha <- attr(lda, "alpha")
gamma <- attr(lda, "gamma")
terms <- attr(lda, "terms")

table(data["Topic.Code"])

lda.topic <- rep(0, nrow(gamma));
for (i in 1:nrow(gamma))
{
  lda.topic[i] <- which.max(gamma[i,]);
}

table(lda.topic)