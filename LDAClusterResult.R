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


inputDataSets <- data.frame(data["Topic.Code"]), lda.topic)
names(inputDataSets) <- c("Actual","Predicted")

#compute frequency of actual categories
actual <- as.data.frame(table(inputDataSets$Actual))
names(actual) <- c("Actual","ActualFreq")

#build confusion matrix
confusion <- as.data.frame(table(inputDataSets$Actual, inputDataSets$Predicted))
names(confusion) <- c("Actual","Predicted","Freq")

#calculate percentage of test cases based on actual frequency
confusion <- merge(confusion, actual, by=c("Actual"))
confusion$Percent <- confusion$Freq/confusion$ActualFreq*100

#render plot
# we use three different layers
# first we draw tiles and fill color based on percentage of test cases
tile <- ggplot() +
  geom_tile(aes(x=Actual, y=Predicted,fill=Percent),data=confusion, color="black",size=0.1) +
  labs(x="Actual",y="Predicted") 

# next we render text values. If you only want to indicate values greater than zero then use data=subset(confusion, Percent > 0)
tile <- tile +
  geom_text(aes(x=Actual,y=Predicted, label=sprintf("%.1f", Percent)),data=confusion, size=3, colour="black") +
  scale_fill_gradient(low="grey",high="red")

# lastly we draw diagonal tiles. We use alpha = 0 so as not to hide previous layers but use size=0.3 to highlight border
tile <- tile +
  geom_tile(aes(x=Actual,y=Predicted),data=subset(confusion, as.character(Actual)==as.character(Predicted)), color="black",size=0.3, fill="black", alpha=0) 

#render
tile