rm(list=ls())
cat("\014")

library(knitr)
library(wooldridge)
attach(bwght2)

datos = na.omit(bwght2)

train.size = round(dim(datos)[1] * 0.7)
train = sample(1:dim(datos)[1], train.size)
test = -train
datos.train = datos[train, ]
datos.test = datos[test, ]
test.size = dim(datos.test)[1]


set.seed(100)
lm.fit = lm(lbwght~., data=datos.train)
lm.pred = predict(lm.fit, newdata = datos.test)

error.mco <- mean((datos.test[, "lbwght"] - lm.pred)^2)
error.mco

results <- matrix(NA, nrow = 3, ncol = 3)
colnames(results) <- c("None","5 Cross Validation","10 Cross Validation")
rownames(results) <- c("MCO","MCO with Subset Selection","MCO with Forward step wise")
results <- as.table(results)

results["MCO", "None"] = error.mco

summary(lm.fit)
lm.fit$coefficients

datos[, "moth"] <- NULL
datos[, "foth"] <- NULL
datos.train[, "moth"] <- NULL
datos.train[, "foth"] <- NULL
datos.test[, "moth"] <- NULL
datos.test[, "foth"] <- NULL


library(leaps)

nvariables <- as.numeric(dim(datos)[2] - 1)
regfit.full = regsubsets(lbwght~., data=datos[train,], nvmax=nvariables)

predict.regsubsets=function(object, newdata, id, ...) {
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id) 
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

k = 10
set.seed(100)
folds=sample(1:k,nrow(datos.train),replace=TRUE)
table(folds)
cv.errors=matrix(NA,k,nvariables, dimnames =list(NULL , paste(1:nvariables)))
for(j in 1:k){
  best.fit=regsubsets(lbwght~.,data=datos.train[folds!=j,],
                      nvmax=nvariables)
  for(i in 1:nvariables){
    pred=predict.regsubsets(best.fit,datos.train[folds==j,],id=i)
    cv.errors[j,i]=mean( (datos.train$lbwght[folds==j]-pred)^2)
  }
}
rmse.cv=sqrt(apply(cv.errors,2,mean))
rmse.cv
which.min(rmse.cv)
plot(rmse.cv,pch=19,type="b")
reg.best=regsubsets (lbwght~.,data=datos.train , nvmax=nvariables)
coef(reg.best ,which.min(rmse.cv))

# Modelo final acorde a las la mejor selección de conjuntos
fit.final <- lm(lbwght ~ bwght + fmaps + lbw + vlbw, data = datos.train)
summary(fit.final)

regfit.full=regsubsets(lbwght~.,data= datos[train,],nvmax=nvariables)

lm.pred = predict.regsubsets(regfit.full, newdata = datos.test, id=which.min(rmse.cv))
error.mss <- mean((datos.test[, "lbwght"] - lm.pred)^2)
error.mss 

results["MCO with Subset Selection", "10 Cross Validation"] = error.mss

nvariables <- as.numeric(dim(datos)[2] - 1)
regfit.full = regsubsets(lbwght~., data=datos[train,], nvmax=nvariables, method = "forward")

k = 10
set.seed(100)
folds=sample(1:k,nrow(datos.train),replace=TRUE)

table(folds)
cv.errors=matrix(NA,k,nvariables, dimnames =list(NULL , paste(1:nvariables)))
for(j in 1:k){
  best.fit=regsubsets(lbwght~.,data=datos.train[folds!=j,],
                      nvmax=nvariables, method = "forward")
  for(i in 1:nvariables){
    pred=predict.regsubsets(best.fit,datos.train[folds==j,],id=i)
    cv.errors[j,i]=mean( (datos.train$lbwght[folds==j]-pred)^2)
  }
}
rmse.cv=sqrt(apply(cv.errors,2,mean))
rmse.cv
which.min(rmse.cv)
plot(rmse.cv,pch=19,type="b")
reg.best=regsubsets (lbwght~.,data=datos.train , nvmax=nvariables, method = "forward")
coef(reg.best ,which.min(rmse.cv))

# Modelo final acorde a las la mejor selección de conjuntos
fit.final <- lm(lbwght ~ bwght + fmaps + lbw + vlbw, data = datos.train)
summary(fit.final)

regfit.full=regsubsets(lbwght~.,data= datos[train,],nvmax=nvariables, method = "forward")

lm.pred = predict.regsubsets(regfit.full, newdata = datos.test, id=which.min(rmse.cv))
error.mss <- mean((datos.test[, "lbwght"] - lm.pred)^2)
error.mss 

results["MCO with Forward step wise", "10 Cross Validation"] = error.mss

nvariables <- as.numeric(dim(datos)[2] - 1)
regfit.full = regsubsets(lbwght~., data=datos[train,], nvmax=nvariables)

predict.regsubsets=function(object, newdata, id, ...) {
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id) 
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

k = 5
set.seed(100)
folds=sample(1:k,nrow(datos.train),replace=TRUE)

table(folds)
cv.errors=matrix(NA,k,nvariables, dimnames =list(NULL , paste(1:nvariables)))
for(j in 1:k){
  best.fit=regsubsets(lbwght~.,data=datos.train[folds!=j,],
                      nvmax=nvariables)
  for(i in 1:nvariables){
    pred=predict.regsubsets(best.fit,datos.train[folds==j,],id=i)
    cv.errors[j,i]=mean( (datos.train$lbwght[folds==j]-pred)^2)
  }
}
rmse.cv=sqrt(apply(cv.errors,2,mean))
rmse.cv
which.min(rmse.cv)
plot(rmse.cv,pch=19,type="b")
reg.best=regsubsets (lbwght~.,data=datos.train , nvmax=nvariables)
coef(reg.best ,which.min(rmse.cv))

# Modelo final acorde a las la mejor selección de conjuntos
fit.final <- lm(lbwght ~ bwght + fmaps + lbw + vlbw, data = datos.train)
summary(fit.final)

regfit.full=regsubsets(lbwght~.,data= datos[train,],nvmax=nvariables)

lm.pred = predict.regsubsets(regfit.full, newdata = datos.test, id=which.min(rmse.cv))
error.mss <- mean((datos.test[, "lbwght"] - lm.pred)^2)
error.mss 

results["MCO with Subset Selection", "5 Cross Validation"] = error.mss

nvariables <- as.numeric(dim(datos)[2] - 1)
regfit.full = regsubsets(lbwght~., data=datos[train,], nvmax=nvariables, method = "forward")

k = 5
set.seed(100)
folds=sample(1:k,nrow(datos.train),replace=TRUE)

table(folds)
cv.errors=matrix(NA,k,nvariables, dimnames =list(NULL , paste(1:nvariables)))
for(j in 1:k){
  best.fit=regsubsets(lbwght~.,data=datos.train[folds!=j,],
                      nvmax=nvariables, method = "forward")
  for(i in 1:nvariables){
    pred=predict.regsubsets(best.fit,datos.train[folds==j,],id=i)
    cv.errors[j,i]=mean( (datos.train$lbwght[folds==j]-pred)^2)
  }
}
rmse.cv=sqrt(apply(cv.errors,2,mean))
rmse.cv
which.min(rmse.cv)
plot(rmse.cv,pch=19,type="b")
reg.best=regsubsets (lbwght~.,data=datos.train , nvmax=nvariables, method = "forward")
coef(reg.best ,which.min(rmse.cv))

# Modelo final acorde a las la mejor selección de conjuntos
fit.final <- lm(lbwght ~ bwght + fmaps + lbw + vlbw, data = datos.train)
summary(fit.final)

regfit.full=regsubsets(lbwght~.,data= datos[train,],nvmax=nvariables, method = "forward")

lm.pred = predict.regsubsets(regfit.full, newdata = datos.test, id=which.min(rmse.cv))
error.mss <- mean((datos.test[, "lbwght"] - lm.pred)^2)
error.mss 


kable(results)

results2 <- matrix(NA, nrow = 5, ncol = 2)
colnames(results2) <- c("5 Cross Validation","10 Cross Validation")
rownames(results2) <- c("RIDGE","LASSO","PCA", "PLS", "LASSO with Elastic Net")
results2 <- as.table(results2)

set.seed(100)
library(glmnet)
x=model.matrix(lbwght~.,datos)[,-1]
y=datos$lbwght
y.test=y[test]
grid=10^seq(10,-2, length =100)
cv.ridge=cv.glmnet(x[train ,],y[train],alpha=0,lambda=grid)
plot(cv.ridge)
mejorlambda=cv.ridge$lambda.min
mejorlambda

ridge.mod=glmnet(x[train ,],y[train],alpha=0,lambda=grid)
ridge.pred=predict(ridge.mod,s=mejorlambda ,newx=x[test ,])

error.ridge <- mean((ridge.pred-datos.test[, "lbwght"] )^2)
error.ridge

results2["RIDGE", "10 Cross Validation"] = error.ridge

# Regla del "codo" de una DT del error de VC:
lambda.codo <- cv.ridge$lambda.1se
lambda.codo

ridge.pred.2=predict(ridge.mod,s=lambda.codo,newx=x[test ,])
error.ridge.2 <- mean((ridge.pred.2-datos.test[, "lbwght"] )^2)
error.ridge.2

set.seed(100)
cv.lasso=cv.glmnet(x[train ,],y[train],alpha=1, lambda = grid)
plot(cv.lasso)
bestlam=cv.lasso$lambda.min
bestlam

lasso.mod=glmnet(x[train ,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test ,])

error.lasso <- mean((lasso.pred-datos.test[, "lbwght"] )^2)
error.lasso
results2["LASSO", "10 Cross Validation"] = error.lasso

# Regla del "codo" de una DT del error de VC:
lambda.codo.l <- cv.lasso$lambda.1se
lambda.codo.l

lasso.pred.2=predict(lasso.mod,s=lambda.codo.l,newx=x[test ,])
error.lasso.2 <- mean((lasso.pred.2-datos.test[, "lbwght"] )^2)
error.lasso.2

c = coef(lasso.mod, s = bestlam)
c

set.seed(100)
library(glmnet)
x=model.matrix(lbwght~.,datos)[,-1]
y=datos$lbwght
y.test=y[test]
grid=10^seq(10,-2, length =100)
cv.ridge=cv.glmnet(x[train ,],y[train],alpha=0,lambda=grid, nfolds = 5)
plot(cv.ridge)
mejorlambda=cv.ridge$lambda.min
mejorlambda

ridge.mod=glmnet(x[train ,],y[train],alpha=0,lambda=grid)
ridge.pred=predict(ridge.mod,s=mejorlambda ,newx=x[test ,])

error.ridge <- mean((ridge.pred-datos.test[, "lbwght"] )^2)
error.ridge

results2["RIDGE", "5 Cross Validation"] = error.ridge

set.seed(100)
cv.lasso=cv.glmnet(x[train ,],y[train],alpha=1, lambda = grid, nfolds = 5)
plot(cv.lasso)
bestlam=cv.lasso$lambda.min
bestlam

lasso.mod=glmnet(x[train ,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test ,])

error.lasso <- mean((lasso.pred-datos.test[, "lbwght"] )^2)
error.lasso

results2["LASSO", "5 Cross Validation"] = error.lasso

c = coef(lasso.mod, s = bestlam)
c

library(pls)
set.seed(100)
pcr.fit=pcr(lbwght~., data=datos,subset=train,scale=TRUE, validation="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP", xlab = "Número de Componentes Principales")
pcr.cv <- crossval(pcr.fit, segments = 10)
plot(RMSEP(pcr.cv), legendpos="topright")
summary(pcr.cv, what = "validation")

## Selecciona el número de componentes principales
## Regla del codo: 1 d.t.
ncomp.1.d.t. <- selectNcomp(pcr.fit, method = "onesigma", plot = TRUE, validation = "CV",
                            segments = 10)
ncomp.1.d.t.
pcr.pred=predict(pcr.fit,newdata=x[test,],ncomp=ncomp.1.d.t.)
error.pcr <- mean((pcr.pred - datos.test[, "lbwght"])^2)
error.pcr

results2["PCA", "10 Cross Validation"] = error.pcr

set.seed(100)
pcr.fit=pcr(lbwght~., data=datos,subset=train,scale=TRUE, validation="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP", xlab = "Número de Componentes Principales")
pcr.cv <- crossval(pcr.fit, segments = 5)
plot(RMSEP(pcr.cv), legendpos="topright")
summary(pcr.cv, what = "validation")

## Selecciona el número de componentes principales
## Regla del codo: 1 d.t.
ncomp.1.d.t. <- selectNcomp(pcr.fit, method = "onesigma", plot = TRUE, validation = "CV",
                            segments = 5)
ncomp.1.d.t.
pcr.pred=predict(pcr.fit,newdata=x[test,],ncomp=ncomp.1.d.t.)
error.pcr <- mean((pcr.pred - datos.test[, "lbwght"])^2)
error.pcr

results2["PCA", "5 Cross Validation"] = error.pcr

set.seed(100)
pls.fit=plsr(lbwght~., data=datos,subset=train,scale=TRUE, validation="CV")
summary(pls.fit)
validationplot(pls.fit,val.type="MSEP", xlab = "Número de Componentes Principales")
pls.cv <- crossval(pls.fit, segments = 10)
plot(RMSEP(pls.cv), legendpos="topright")
summary(pls.cv, what = "validation")

## Utilizamos 4 componentes por el Mínimo Error de VC
pls.pred=predict(pls.fit,newdata=x[test,],ncomp=4)
error.pls <- mean((pls.pred - datos.test[, "lbwght"])^2)
error.pls

## Selecciona el número de componentes principales
## Regla del codo: 1 d.t.
ncomp.1.d.t. <- selectNcomp(pls.fit, method = "onesigma", plot = TRUE, validation = "CV",
                            segments = 10)
ncomp.1.d.t.
pls.pred.2=predict(pls.fit,newdata=x[test,],ncomp=ncomp.1.d.t.)
error.pls.codo <- mean((pls.pred.2 - datos.test[, "lbwght"])^2)
error.pls.codo

results2["PLS", "10 Cross Validation"] = error.pls.codo

## Regla de la permutación: se selecciona el ncomp que nos da el min Error de VC
ncomp.perm <- selectNcomp(pls.fit, method = "randomization", plot = TRUE)
ncomp.perm
pls.pred.3=predict(pls.fit,newdata=x[test,],ncomp=ncomp.perm)
error.pls.perm <- mean((pls.pred.3 - datos.test[, "lbwght"])^2)
error.pls.perm



set.seed(100)
pls.fit=plsr(lbwght~., data=datos,subset=train,scale=TRUE, validation="CV")
summary(pls.fit)
validationplot(pls.fit,val.type="MSEP", xlab = "Número de Componentes Principales")
pls.cv <- crossval(pls.fit, segments = 5)
plot(RMSEP(pls.cv), legendpos="topright")
summary(pls.cv, what = "validation")


## Utilizamos 4 componentes por el Mínimo Error de VC
pls.pred=predict(pls.fit,newdata=x[test,],ncomp=4)
error.pls <- mean((pls.pred - datos.test[, "lbwght"])^2)
error.pls

## Selecciona el número de componentes principales
## Regla del codo: 1 d.t.
ncomp.1.d.t. <- selectNcomp(pls.fit, method = "onesigma", plot = TRUE, validation = "CV",
                            segments = 5)
ncomp.1.d.t.
pls.pred.2=predict(pls.fit,newdata=x[test,],ncomp=ncomp.1.d.t.)
error.pls.codo <- mean((pls.pred.2 - datos.test[, "lbwght"])^2)
error.pls.codo

results2["PLS", "5 Cross Validation"] = error.pls.codo

## Regla de la permutación: se selecciona el ncomp que nos da el min Error de VC
ncomp.perm <- selectNcomp(pls.fit, method = "randomization", plot = TRUE)
ncomp.perm
pls.pred.3=predict(pls.fit,newdata=x[test,],ncomp=ncomp.perm)
error.pls.perm <- mean((pls.pred.3 - datos.test[, "lbwght"])^2)
error.pls.perm



library(glmnet)
library(caret)
set.seed(100)
lambda.grid <- 10^seq(2,-2, length = 100)
alpha.grid <- seq(0,1, by = 0.05)

Control <- trainControl(method = "cv", number = 10)
busca.grid <- expand.grid(alpha = alpha.grid, lambda = lambda.grid)

set.seed(100)
mi.entrenamiento <- train(lbwght~., data = datos.train, method = "glmnet", 
                          tuneGrid = busca.grid, trControl = Control,
                          tuneLength = 10,
                          standardize = TRUE, maxit = 1000000)

plot(mi.entrenamiento)
attributes(mi.entrenamiento)
mi.entrenamiento$bestTune

mi.modelo.glmnet <- mi.entrenamiento$finalModel
coef(mi.modelo.glmnet, s = mi.entrenamiento$bestTune$lambda)
mej.modelo <- glmnet(x[train ,],y[train], alpha=mi.entrenamiento$bestTune$alpha,
                     lambda = mi.entrenamiento$bestTune$lambda)
c = coef(mej.modelo, s = mi.entrenamiento$bestTune$lambda)
cbind(coef(mej.modelo, s = mi.entrenamiento$bestTune$lambda), coef(mi.modelo.glmnet, s = mi.entrenamiento$bestTune$lambda))

lre.pred <- predict(mej.modelo,s=mi.entrenamiento$bestTune$lambda,newx=x[test ,])

error.lassoelastic <- mean((lre.pred - datos.test[, "lbwght"])^2)
error.lassoelastic

results2["LASSO with Elastic Net", "10 Cross Validation"] = error.lassoelastic


set.seed(100)
lambda.grid <- 10^seq(2,-2, length = 100)
alpha.grid <- seq(0,1, by = 0.05)
Control <- trainControl(method = "cv", number = 5)
busca.grid <- expand.grid(alpha = alpha.grid, lambda = lambda.grid)
set.seed(100)
mi.entrenamiento <- train(lbwght~., data = datos.train, method = "glmnet", 
                          tuneGrid = busca.grid, trControl = Control,
                          tuneLength = 5,
                          standardize = TRUE, maxit = 1000000)

plot(mi.entrenamiento)
attributes(mi.entrenamiento)
mi.entrenamiento$bestTune

mi.modelo.glmnet <- mi.entrenamiento$finalModel
coef(mi.modelo.glmnet, s = mi.entrenamiento$bestTune$lambda)
mej.modelo <- glmnet(x[train ,],y[train], alpha=mi.entrenamiento$bestTune$alpha,
                     lambda = mi.entrenamiento$bestTune$lambda)
c = coef(mej.modelo, s = mi.entrenamiento$bestTune$lambda)
cbind(coef(mej.modelo, s = mi.entrenamiento$bestTune$lambda), coef(mi.modelo.glmnet, s = mi.entrenamiento$bestTune$lambda))

lre.pred <- predict(mej.modelo,s=mi.entrenamiento$bestTune$lambda,newx=x[test ,])

error.lassoelastic <- mean((lre.pred - datos.test[, "lbwght"])^2)
error.lassoelastic
results2["LASSO with Elastic Net", "5 Cross Validation"] = error.lassoelastic


kable(results2)
