library(glmnet)
library(caret)

#Problem 1

#Generate highly correlated variables
library(MASS)
 mu <- rep(0,697)
 Sigma <- matrix(.955, nrow=697, ncol=697) + diag(697)*.1

 rawvars <- mvrnorm(n=1000, mu=mu, Sigma=Sigma)

 cov(rawvars); cor(rawvars)
 eigen(cov(rawvars))
#
# #generate not so correlated variables
 munew <- rep(0,3)
 noncovmatrix <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), nrow =3)
 rawvars2 <- mvrnorm(n=1000, mu=munew, Sigma= noncovmatrix)
#
 #dataset for ex.2
 data.ex2 <- cbind(rawvars,rawvars2)

 #values for beta
 beta2 <- runif(696, 0,0)
 betanoncov <- c(500,600,700,800)
 beta <- as.matrix(append(beta2, betanoncov))
#
 #define the response variable
 y <- data.ex2%*%beta
 sdex2 <- abs(mean(y))
 error <- rnorm(1000,mean =0,sd=0.1*sdex2)
 y <- y+error






# 
# 
# beta2 <- runif(696, 0,0)
# betanoncov <- c(50,60,70,80)
# beta <- as.matrix(append(beta2, betanoncov))
# 
# Mymean<- sample(-20:20,700,replace = TRUE)
# Mystd<-sample(0.5:3.5,700,replace = TRUE)
# 
# data.ex1<-matrix(NA,700,1000)
# 
# # making predictors
# i=1
# 
# while (i < 701){
#   data.ex1[i,]<- rnorm(1000,Mymean[i],Mystd[i])
#   i<-i+1
# }
# 
# # creating response
# y<-t(data.ex1)%*%beta
# sigma1<-0.1*mean(y)
# noise1<-rnorm(1000,0,sigma1^2)
# y<-y+noise1
# 
# # Transposing data.ex1 and y
# data.ex2<-t(data.ex1)
# 


#############################################################################################################
#Creating training and test datasets
data <-as.data.frame(cbind(y,data.ex2))
names(data)[1]<-"y"

X=model.matrix(y~.,data=data)[,-1]
y=data$y
grid <- 10^seq(10,-2,length=100)

train=sample(1:nrow(X), nrow(X)*0.7)
test=(-train)
y.test=y[test]
#############################################################################################################

#Ridge Regression

ridge_model <- glmnet(X, y, alpha=0, lambda=grid)
plot(ridge_model, xvar = "lambda", main = "Ridge")
ridge_model_cv <- cv.glmnet(X,y, alpha=0)
plot(ridge_model_cv)



ridge_model_train <- glmnet(X[train,],y[train],alpha=0,
                            lambda=grid,thresh=1e-12)
ridge_pred <- predict(ridge_model_train, s=ridge_model_cv$lambda.min,
                      newx=X[test,])
ridgemse<-mean((ridge_pred-y.test)^2)


###################################################################################################
# LASSO Regression

lasso.mod=glmnet(X[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)

set.seed(1)
cv.out=cv.glmnet(X[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=X[test,])
lassomse<-mean((lasso.pred-y.test)^2)

out=glmnet(X,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)
lasso.coef<-lasso.coef [which(rowSums(lasso.coef) > 0), ]
lasso.coef<-as.data.frame(t(lasso.coef))
lasso.coef.names<-names(lasso.coef)
lassonames<-lasso.coef.names


###################################################################################################
# Principle Component

library(pls)
set.seed(1)
pcr.fit=pcr(y~.,data=data, scale=TRUE, validation="CV")

summary(pcr.fit)

validationplot(pcr.fit,val.type = "MSEP")

set.seed(1)
pcr.fit=pcr(y~., data=data, subset=train,scale=TRUE,validation="CV")
validationplot(pcr.fit,val.type="MSEP")

pcr.pred=predict(pcr.fit,X[test,],ncomp=629)
pcrmse<-mean((pcr.pred-y.test)^2)

####################################################################################################
# Partial least square

pls_train <- plsr(y[train]~X[train,], scale=TRUE, validation="CV")
summary(pls_train)
validationplot(pls_train,val.type="MSEP")

pls.pred <-predict(pls_train,X[test,],ncomp=73)
plsmse<-mean((pls.pred-y.test)^2)

###################################################################################################
# Least Square

linear_reg_train <- lm(y~.,data=data,subset = train)
ndata<-as.data.frame(X[test,])
linear_pred <- predict(linear_reg_train, newdata=ndata)
lsmse<-mean((linear_pred-y.test)^2)


print(paste0("Ridge MSE is ",ridgemse))
print(paste0("Lasso MSE is ",lassomse))
print(paste0("PCR MSE is ",pcrmse))
print(paste0("PLS MSE is ",plsmse))
print(paste0("Least Squares MSE is ",lsmse))

##Order of MSE least to greatest: PLS, Ridge, PCR, Lasso, Least squares##

#Problem 2

#Generate highly correlated variables
library(MASS)
mu <- rep(0,697)
Sigma <- matrix(.955, nrow=697, ncol=697) + diag(697)*.1

rawvars <- mvrnorm(n=1000, mu=mu, Sigma=Sigma)

cov(rawvars); cor(rawvars)
eigen(cov(rawvars))
#
# #generate not so correlated variables
munew <- rep(0,3)
noncovmatrix <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), nrow =3)
rawvars2 <- mvrnorm(n=1000, mu=munew, Sigma= noncovmatrix)
#
#dataset for ex.2
data.ex2 <- cbind(rawvars,rawvars2)

#values for beta
beta2 <- runif(696, 0,0)
betanoncov <- c(500,600,700,800)
beta <- as.matrix(append(beta2, betanoncov))
#
#define the response variable
y <- data.ex2%*%beta
sdex2 <- abs(mean(y))
error <- rnorm(1000,mean =0,sd=0.2*sdex2)
y <- y+error

#############################################################################################################
#Creating training and test datasets
data <-as.data.frame(cbind(y,data.ex2))
names(data)[1]<-"y"

X=model.matrix(y~.,data=data)[,-1]
y=data$y
grid <- 10^seq(10,-2,length=100)

train=sample(1:nrow(X), nrow(X)*0.7)
test=(-train)
y.test=y[test]
#############################################################################################################

#Ridge Regression

ridge_model <- glmnet(X, y, alpha=0, lambda=grid)
plot(ridge_model, xvar = "lambda", main = "Ridge")
ridge_model_cv <- cv.glmnet(X,y, alpha=0)
plot(ridge_model_cv)



ridge_model_train <- glmnet(X[train,],y[train],alpha=0,
                            lambda=grid,thresh=1e-12)
ridge_pred <- predict(ridge_model_train, s=ridge_model_cv$lambda.min,
                      newx=X[test,])
ridgemse<-mean((ridge_pred-y.test)^2)


###################################################################################################
# LASSO Regression

lasso.mod=glmnet(X[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)

set.seed(1)
cv.out=cv.glmnet(X[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=X[test,])
lassomse<-mean((lasso.pred-y.test)^2)

out=glmnet(X,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)
lasso.coef<-lasso.coef [which(rowSums(lasso.coef) > 0), ]
lasso.coef<-as.data.frame(t(lasso.coef))
lasso.coef.names<-names(lasso.coef)
lassonames<-lasso.coef.names


###################################################################################################
# Principle Component

library(pls)
set.seed(1)
pcr.fit=pcr(y~.,data=data, scale=TRUE, validation="CV")

summary(pcr.fit)

validationplot(pcr.fit,val.type = "MSEP")

set.seed(1)
pcr.fit=pcr(y~., data=data, subset=train,scale=TRUE,validation="CV")
validationplot(pcr.fit,val.type="MSEP")

pcr.pred=predict(pcr.fit,X[test,],ncomp=620)
pcrmse<-mean((pcr.pred-y.test)^2)

####################################################################################################
# Partial least square

pls_train <- plsr(y[train]~X[train,], scale=TRUE, validation="CV")
summary(pls_train)
validationplot(pls_train,val.type="MSEP")


pls.pred <-predict(pls_train,X[test,],ncomp=27)
plsmse<-mean((pls.pred-y.test)^2)

###################################################################################################
# Least Square

linear_reg_train <- lm(y~.,data=data,subset = train)
ndata<-as.data.frame(X[test,])
linear_pred <- predict(linear_reg_train, newdata=ndata)
lsmse<-mean((linear_pred-y.test)^2)


print(paste0("Ridge MSE is ",ridgemse))
print(paste0("Lasso MSE is ",lassomse))
print(paste0("PCR MSE is ",pcrmse))
print(paste0("PLS MSE is ",plsmse))
print(paste0("Least Squares MSE is ",lsmse))

##Order of MSE least to greatest: PLS, Ridge, PCR, Lasso, Lease Squares.##

#Problem 3

#Generate highly correlated variables
library(MASS)
mu <- rep(0,697)
Sigma <- matrix(.955, nrow=697, ncol=697) + diag(697)*.1

rawvars <- mvrnorm(n=1000, mu=mu, Sigma=Sigma)

cov(rawvars); cor(rawvars)
eigen(cov(rawvars))
#
# #generate not so correlated variables
munew <- rep(0,3)
noncovmatrix <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), nrow =3)
rawvars2 <- mvrnorm(n=1000, mu=munew, Sigma= noncovmatrix)
#
#dataset for ex.2
data.ex2 <- cbind(rawvars,rawvars2)

#values for beta
beta2 <- runif(696, 0,0)
betanoncov <- c(500,600,700,800)
beta <- as.matrix(append(beta2, betanoncov))
#
#define the response variable
y <- data.ex2%*%beta
sdex2 <- abs(mean(y))
error <- rnorm(1000,mean =0,sd=0.4*sdex2)
y <- y+error


#############################################################################################################
#Creating training and test datasets
data <-as.data.frame(cbind(y,data.ex2))
names(data)[1]<-"y"

X=model.matrix(y~.,data=data)[,-1]
y=data$y
grid <- 10^seq(10,-2,length=100)

train=sample(1:nrow(X), nrow(X)*0.7)
test=(-train)
y.test=y[test]
#############################################################################################################

#Ridge Regression

ridge_model <- glmnet(X, y, alpha=0, lambda=grid)
plot(ridge_model, xvar = "lambda", main = "Ridge")
ridge_model_cv <- cv.glmnet(X,y, alpha=0)
plot(ridge_model_cv)



ridge_model_train <- glmnet(X[train,],y[train],alpha=0,
                            lambda=grid,thresh=1e-12)
ridge_pred <- predict(ridge_model_train, s=ridge_model_cv$lambda.min,
                      newx=X[test,])
ridgemse<-mean((ridge_pred-y.test)^2)


###################################################################################################
# LASSO Regression

lasso.mod=glmnet(X[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)

set.seed(1)
cv.out=cv.glmnet(X[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=X[test,])
lassomse<-mean((lasso.pred-y.test)^2)

out=glmnet(X,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)
lasso.coef<-lasso.coef [which(rowSums(lasso.coef) > 0), ]
lasso.coef<-as.data.frame(t(lasso.coef))
lasso.coef.names<-names(lasso.coef)
lassonames<-lasso.coef.names


###################################################################################################
# Principle Component

library(pls)
set.seed(1)
pcr.fit=pcr(y~.,data=data, scale=TRUE, validation="CV")

summary(pcr.fit)

validationplot(pcr.fit,val.type = "MSEP")

set.seed(1)
pcr.fit=pcr(y~., data=data, subset=train,scale=TRUE,validation="CV")
validationplot(pcr.fit,val.type="MSEP")

pcr.pred=predict(pcr.fit,X[test,],ncomp=700)
pcrmse<-mean((pcr.pred-y.test)^2)

####################################################################################################
# Partial least square

pls_train <- plsr(y[train]~X[train,], scale=TRUE, validation="CV")
summary(pls_train)
validationplot(pls_train,val.type="MSEP")

pls.pred <-predict(pls_train,X[test,],ncomp=21)
plsmse<-mean((pls.pred-y.test)^2)

###################################################################################################
# Least Square

linear_reg_train <- lm(y~.,data=data,subset = train)
ndata<-as.data.frame(X[test,])
linear_pred <- predict(linear_reg_train, newdata=ndata)
lsmse<-mean((linear_pred-y.test)^2)


print(paste0("Ridge MSE is ",ridgemse))
print(paste0("Lasso MSE is ",lassomse))
print(paste0("PCR MSE is ",pcrmse))
print(paste0("PLS MSE is ",plsmse))
print(paste0("Least Squares MSE is ",lsmse))

##Order of MSE least to greatest: PLS, Ridge, PCR, Lasso, Lease Squares.##