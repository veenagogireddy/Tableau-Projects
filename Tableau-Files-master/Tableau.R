library(Rserve)
Rserve()
library(rpart)
df<-read.csv("E:/ISMC6137/airbnb.csv")
df
df<-na.omit(df)
df
write.csv(df, file = "airbnb.csv")
library(rpart)
fit=rpart(df$occupancy_30~df$bedrooms+df$price+df$guests_included+df$minimum_nights+df$Number.of.amenities+df$reviews_per_month,method="anova")
predict(fit)
data.frame(predict(fit,type="vector"))
t(data.frame(predict(fit,type="vector")))[1,]
rpart.plot(fit, extra = 106)



library("neuralnet");
nn1 <- neuralnet(occupancy_30~bedrooms+Number.of.amenities+minimum_nights+price,data=trainingData,act.fct = "logistic",hidden=3, threshold=0.01);
                 #~bedrooms+amenities+Price+minnights+as.factor(property)+as.factor(host)+as.factor(loc)+expp, hidden=10, threshold=0.01);
predict(nn1)




accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)

install.packages("rpart")

#data.frame(occupancy=.arg1,bedrooms=.arg2,price=.arg3,guests=.arg4,minnights=.arg5,amenities=.arg6,reviews=.arg7));
fit<-prune(fit,fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]);
t(data.frame(predict(fit,type="vector")))

install.packages('neuralnet')
library("neuralnet")
traininginput <-  as.data.frame(runif(50, min=0, max=100))
trainingoutput <- sqrt(traininginput)

#result<- compute(,trainingdata)$net.result

#Column bind the data into one variable
trainingdata <- cbind(traininginput,trainingoutput)
colnames(trainingdata) <- c("Input","Output")

#Train the neural network
#Going to have 10 hidden layers
#Threshold is a numeric value specifying the threshold for the partial
#derivatives of the error function as stopping criteria.
net.sqrt <- neuralnet(Output~Input,trainingdata, hidden=10, threshold=0.01)
trainingData <- subset(df, select=c("occupancy_30", "bedrooms","Number.of.amenities","minimum_nights","price"))
trainingData
library(gbm)
gbm1 <- gbm(occupancy_30~bedrooms+Number.of.amenities+minimum_nights+price,data=trainingData, n.trees=5, distribution="gaussian",
            interaction.depth=3, bag.fraction=0.5, train.fraction=1.0, shrinkage=0.1,
            keep.data=TRUE)
pred <- predict(gbm1,n.trees=5)
pred
pred <- as.data.frame(pred)
pred


# subset
df[,c("A","B","E")]
trainingdata<-
print(net.sqrt)
new.sqrt$net.result
result<- compute(net.sqrt,trainingdata)$net.result
result
#predict(net.sqrt)
#Plot the neural network
plot(net.sqrt)

