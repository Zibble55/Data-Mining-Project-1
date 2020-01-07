library(glmnet)

# scene 1 updated
set.seed(1000)
beta<- sample(setdiff(-20:20,0),15,replace = TRUE)
beta<-as.matrix(beta)
beta<-t(beta)

# set.seed(1000)
# beta<- sample(setdiff(-5.00:5.00,0),15,replace = TRUE)
# beta<-as.matrix(beta)
# beta<-t(beta)


#Problem 1

#making 15 random mean and std 
Mymean<- sample(15,replace = FALSE)
Mystd<-sample(15,replace = FALSE)

data.ex1<-matrix(NA,15,1000)

# making predictors
 i=1
# 
# while (i < 16){
#   data.ex1[i,]<- rnorm(1000,Mymean[i],Mystd[i])
#   i<-i+1
# }

Mymean<-0
Mystd<-1

while (i < 16){
  data.ex1[i,]<- rnorm(1000,Mymean,Mystd)
  i<-i+1
}

# creating response
y<-beta%*%data.ex1
sigma1<-0.1*mean(y)
noise1<-rnorm(1000,0,sigma1^2)
y<-y+noise1

# Transposing data.ex1 and y
data.ex1<-t(data.ex1)
y<-t(y)

#############################################################################################################
#Creating training and test datasets

data <-as.data.frame(cbind(y,data.ex1))
names(data)[1]<-"y"

X<-model.matrix(y~.,data=data)[,-1]
y<-data$y
grid <- 10^seq(10,-2,length=100)

set.seed(2)
train<-sample(1:nrow(X), nrow(X)*0.7)
test<-(-train)

y.test<-y[test]


###################################################################################################
# Ridge

ridge_model <- glmnet(X, y, alpha=0, lambda=grid)
plot(ridge_model, xvar = "lambda", main = "Ridge")
ridge_model_cv <- cv.glmnet(X,y, alpha=0)
plot(ridge_model_cv)

ridge_model_train <- glmnet(X[train,],y[train],alpha=0,
                            lambda=grid,thresh=1e-12)
ridge_pred <- predict(ridge_model_train, s=ridge_model_cv$lambda.min,
                      newx=X[test,])
ridgemse_1<-mean((ridge_pred-y.test)^2)
###################################################################################################
# LASSO Regression

lasso.mod=glmnet(X[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)

cv.out=cv.glmnet(X[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=X[test,])
lassomse_1<-mean((lasso.pred-y.test)^2)

out=glmnet(X,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)
lasso.coef<-lasso.coef [which(rowSums(lasso.coef) > 0), ]
lasso.coef<-as.data.frame(t(lasso.coef))
lasso.coef.names<-names(lasso.coef)
lassonames<-lasso.coef.names
###################################################################################################
# Principle Component

library(pls)
pcr.fit=pcr(y~.,data=data, scale=TRUE, validation="CV")

summary(pcr.fit)

validationplot(pcr.fit,val.type = "MSEP")

pcr.fit=pcr(y~., data=data, subset=train,scale=TRUE,validation="CV")
validationplot(pcr.fit,val.type="MSEP")

pcr.pred=predict(pcr.fit,X[test,],ncomp=15)
pcrmse_1<-mean((pcr.pred-y.test)^2)

pcr.fit=pcr(y~X,scale=TRUE,ncomp=15)
summary(pcr.fit)

###################################################################################################
# partial

pls_train <- plsr(y[train]~ X[train,], scale=TRUE, validation="CV")
summary(pls_train)
pls.pred <-predict(pls_train, X[test,],ncomp=6)
plsmse_1<-mean((pls.pred-y.test)^2)

###################################################################################################
# least square

linear_reg_train <- lm(y~.,data=data,subset = train)
linear_pred <- predict(linear_reg_train, newdata=as.data.frame(X[test,]))
lsmse_1<-mean((linear_pred-y.test)^2)
###################################################################################################

print(paste0("Ridge MSE is ",ridgemse_1))
print(paste0("Lasso MSE is ",lassomse_1))
print(paste0("PCR MSE is ",pcrmse_1))
print(paste0("PLS MSE is ",plsmse_1))
print(paste0("Least Squares MSE is ",lsmse_1))

##Order of MSE least to greatest: Ridge, Lasso, PCR, PLS, Least Squares.##

#Problem 2

#making 15 random mean and std 
Mymean<- sample(15,replace = FALSE)
Mystd<-sample(15,replace = FALSE)

data.ex1<-matrix(NA,15,1000)

# making predictors
i=1
# 
# while (i < 16){
#   data.ex1[i,]<- rnorm(1000,Mymean[i],Mystd[i])
#   i<-i+1
# }

Mymean<-0
Mystd<-1

while (i < 16){
  data.ex1[i,]<- rnorm(1000,Mymean,Mystd)
  i<-i+1
}


# creating response
y<-beta%*%data.ex1
sigma2<-0.2*mean(y)
noise2<-rnorm(1000,0,sigma2^2)
y<-y+noise2

# Transposing data.ex1 and y
data.ex1<-t(data.ex1)
y<-t(y)

#############################################################################################################
#Creating training and test datasets

data <-as.data.frame(cbind(y,data.ex1))
names(data)[1]<-"y"

X<-model.matrix(y~.,data=data)[,-1]
y<-data$y
grid <- 10^seq(10,-2,length=100)

set.seed(2)
train<-sample(1:nrow(X), nrow(X)*0.7)
test<-(-train)

y.test<-y[test]


###################################################################################################
# Ridge

ridge_model <- glmnet(X, y, alpha=0, lambda=grid)
plot(ridge_model, xvar = "lambda", main = "Ridge")
ridge_model_cv <- cv.glmnet(X,y, alpha=0)
plot(ridge_model_cv)

ridge_model_train <- glmnet(X[train,],y[train],alpha=0,
                            lambda=grid,thresh=1e-12)
ridge_pred <- predict(ridge_model_train, s=ridge_model_cv$lambda.min,
                      newx=X[test,])
ridgemse_2<-mean((ridge_pred-y.test)^2)
###################################################################################################
# LASSO Regression

lasso.mod=glmnet(X[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)

cv.out=cv.glmnet(X[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=X[test,])
lassomse_2<-mean((lasso.pred-y.test)^2)

out=glmnet(X,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)
lasso.coef<-lasso.coef [which(rowSums(lasso.coef) > 0), ]
lasso.coef<-as.data.frame(t(lasso.coef))
lasso.coef.names<-names(lasso.coef)
lassonames<-lasso.coef.names
###################################################################################################
# Principle Component

library(pls)
pcr.fit=pcr(y~.,data=data, scale=TRUE, validation="CV")

summary(pcr.fit)

validationplot(pcr.fit,val.type = "MSEP")

pcr.fit=pcr(y~., data=data, subset=train,scale=TRUE,validation="CV")
validationplot(pcr.fit,val.type="MSEP")

pcr.pred=predict(pcr.fit,X[test,],ncomp=15)
pcrmse_2<-mean((pcr.pred-y.test)^2)

pcr.fit=pcr(y~X,scale=TRUE,ncomp=15)
summary(pcr.fit)

###################################################################################################
# partial

pls_train <- plsr(y[train]~ X[train,], scale=TRUE, validation="CV")
summary(pls_train)
pls.pred <-predict(pls_train, X[test,],ncomp=6)
plsmse_2<-mean((pls.pred-y.test)^2)

###################################################################################################
# least square

linear_reg_train <- lm(y~.,data=data,subset = train)
linear_pred <- predict(linear_reg_train, newdata=as.data.frame(X[test,]))
lsmse_2<-mean((linear_pred-y.test)^2)
###################################################################################################

print(paste0("Ridge MSE is ",ridgemse_2))
print(paste0("Lasso MSE is ",lassomse_2))
print(paste0("PCR MSE is ",pcrmse_2))
print(paste0("PLS MSE is ",plsmse_2))
print(paste0("Least Squares MSE is ",lsmse_2))

##Order of MSE least to greatest: Ridge, Lasso, PLS, Least Squares, PCR.##

#Problem 3

#making 15 random mean and std 
Mymean<- sample(15,replace = FALSE)
Mystd<-sample(15,replace = FALSE)

data.ex1<-matrix(NA,15,1000)

# making predictors
i=1
# 
# while (i < 16){
#   data.ex1[i,]<- rnorm(1000,Mymean[i],Mystd[i])
#   i<-i+1
# }

Mymean<-0
Mystd<-1

while (i < 16){
  data.ex1[i,]<- rnorm(1000,Mymean,Mystd)
  i<-i+1
}


# creating response
y<-beta%*%data.ex1
sigma3<-0.4*mean(y)
noise3<-rnorm(1000,0,sigma3^2)
y<-y+noise3

# Transposing data.ex1 and y
data.ex1<-t(data.ex1)
y<-t(y)

#############################################################################################################
#Creating training and test datasets

data <-as.data.frame(cbind(y,data.ex1))
names(data)[1]<-"y"

X<-model.matrix(y~.,data=data)[,-1]
y<-data$y
grid <- 10^seq(10,-2,length=100)

set.seed(2)
train<-sample(1:nrow(X), nrow(X)*0.7)
test<-(-train)

y.test<-y[test]


###################################################################################################
# Ridge

ridge_model <- glmnet(X, y, alpha=0, lambda=grid)
plot(ridge_model, xvar = "lambda", main = "Ridge")
ridge_model_cv <- cv.glmnet(X,y, alpha=0)
plot(ridge_model_cv)

ridge_model_train <- glmnet(X[train,],y[train],alpha=0,
                            lambda=grid,thresh=1e-12)
ridge_pred <- predict(ridge_model_train, s=ridge_model_cv$lambda.min,
                      newx=X[test,])
ridgemse_3<-mean((ridge_pred-y.test)^2)
###################################################################################################
# LASSO Regression

lasso.mod=glmnet(X[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)

cv.out=cv.glmnet(X[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=X[test,])
lassomse_3<-mean((lasso.pred-y.test)^2)

out=glmnet(X,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)
lasso.coef<-lasso.coef [which(rowSums(lasso.coef) > 0), ]
lasso.coef<-as.data.frame(t(lasso.coef))
lasso.coef.names<-names(lasso.coef)
lassonames<-lasso.coef.names
###################################################################################################
# Principle Component

library(pls)
pcr.fit=pcr(y~.,data=data, scale=TRUE, validation="CV")

summary(pcr.fit)

validationplot(pcr.fit,val.type = "MSEP")

pcr.fit=pcr(y~., data=data, subset=train,scale=TRUE,validation="CV")
validationplot(pcr.fit,val.type="MSEP")

pcr.pred=predict(pcr.fit,X[test,],ncomp=15)
pcrmse_3<-mean((pcr.pred-y.test)^2)

pcr.fit=pcr(y~X,scale=TRUE,ncomp=15)
summary(pcr.fit)

###################################################################################################
# partial

pls_train <- plsr(y[train]~ X[train,], scale=TRUE, validation="CV")
summary(pls_train)
pls.pred <-predict(pls_train, X[test,],ncomp=6)
plsmse_3<-mean((pls.pred-y.test)^2)

###################################################################################################
# least square

linear_reg_train <- lm(y~.,data=data,subset = train)
linear_pred <- predict(linear_reg_train, newdata=as.data.frame(X[test,]))
lsmse_3<-mean((linear_pred-y.test)^2)
###################################################################################################

print(paste0("Ridge MSE is ",ridgemse_3))
print(paste0("Lasso MSE is ",lassomse_3))
print(paste0("PCR MSE is ",pcrmse_3))
print(paste0("PLS MSE is ",plsmse_3))
print(paste0("Least Squares MSE is ",lsmse_3))

##Order of MSE least to greatest: Ridge, Lasso, PLS, Least Squares, PCR.##
