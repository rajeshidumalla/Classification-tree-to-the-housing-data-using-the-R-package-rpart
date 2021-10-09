# Classification tree to the housing data using the R package rpart

The aim of this project is to fit a classification tree to the housing data using the R package rpart. So, I begin by loading the data, and indicating which variables are categorical (note that some of them are ordinal variables, and we should treat them as such!)


```{r}
library(rpart)
info=c("Y","sex","marital_status","age","education","occupation","annual_income","length",
"dual_incomes","size_household","nb_minors","householder_status","ethnic","language")
X=read.table("Housetype_Data.csv",sep=",",header=F)
Y=X[,1]
X=data.frame(X)
colnames(X)=info
X$Y<-as.factor(X$Y)
X$sex<-as.factor(X$sex)
X$marital_status<-as.factor(X$marital_status)
X$occupation<-as.factor(X$occupation)
X$dual_incomes<-as.factor(X$dual_incomes)
X$ethnic<-as.factor(X$ethnic)
X$householder_status<-as.factor(X$householder_status)
X$language<-as.factor(X$language)
#### The goal is too infer Y from X by fitting an optimal classification tree
```

Let's begin by analyzing the data: nb of missing values, spread, etc (just to get the feel of the data)

```{r}
##### We begin by analyzing the data: nb of missing values, spread, etc
K= 5 ## number of possible classes for the housing
summary(X)
```

<img width="619" alt="Screen Shot 2021-10-10 at 12 47 05 am" src="https://user-images.githubusercontent.com/56792400/136660491-b51fad61-af20-40a9-b07b-1666de1a1c4d.png">


```{r}
# grow tree
fit <- rpart(Y~ .,method="class", data=X,cp=0) ### Y vs X (the rest of the variables)
### This method does exactly what we want: it gives back a classification tree according to the formula given
### The complexity paramter
plotcp(fit) # visualize cross-validation results
```
<img width="613" alt="Screen Shot 2021-10-10 at 12 48 29 am" src="https://user-images.githubusercontent.com/56792400/136660527-2fca251a-0a07-44c4-bb87-633696238296.png">

```{r}
### Prune the tree: select the CP that is 1MSE above the cp-value that minimizes the misclassification error
# get index of CP with lowest xerror
opt_cp <- which.min(fit$cptable[,'xerror'])
candidate_cp<- which(fit$cptable[,'xerror']>(fit$cptable[opt_cp,'xerror']+fit$cptable[opt_cp,'xstd']))
cp_mse_ind<- min(which(fit$cptable[,'xerror']<(fit$cptable[opt_cp,'xerror']+fit$cptable[opt_cp,'xstd'])))
cp_mse=fit$cptable[cp_mse_ind,'CP']
#get its value
#prune tree
pruned_model <- prune(fit,cp_mse)
### as input
printcp(pruned_model) # display the results in terms of complexity paramter
```


```{r}
#plot tree
library(rpart.plot)
```
Warning: package  rpart.plot  was built under R version 3.3.2

```{r}
rpart.plot(pruned_model, uniform=TRUE,main="Classification Tree for Household_Data")
```
<img width="602" alt="Screen Shot 2021-10-10 at 12 49 49 am" src="https://user-images.githubusercontent.com/56792400/136660565-f95d302d-1739-436e-81ad-47baff73bf5a.png">


We note that:

• class 2 (Condominium) and 4 (mobile home) are not predicted by our classification tree. This is due to
the fact that these are relatively rare classes, which account for only respectively 7.5 and 1.8 % of the
observations.

• The most important variable in our case if the householder status (own, rent, live with parent)

• The misclassification cross-validated error rate is roughly 0.409 x 0.636 = 0.2601





























