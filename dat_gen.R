rm(list=ls())
setwd("/Users/ch513/Box/00_Duke/Stroke-EHR-TransferLearning/to_Jimmy/")
source("library_v2.R")

auc.est.fun=function(n, p, 
                     normal.mean.b=NULL, normal.sd.b=NULL, 
                     beta.alpha.b=NULL, beta.ratio.b=NULL,
                     unif.min.b=NULL, unif.mean.b=NULL,
                     dist.b="normal", 
                     mean.X, sd.X){
if(dist.b=="normal"){myb=rnorm(p, normal.mean.b, normal.sd.b)}
if(dist.b=="beta"){myb=rbeta(p, shape1=beta.alpha.b, shape2=beta.alpha.b/beta.ratio.b-beta.alpha.b)}
if(dist.b=="uniform"){myb=runif(p, min=unif.min.b, max=2*unif.mean.b-unif.min.b)}
X=matrix(rnorm(n*p, 0, 1),nrow=n)
Y=rbinom(n, 1,p=g.logit(X%*%myb))
list(b=myb,auc=ROC.Est.FUN(Y,X%*%myb,yy0=0.5)[1])
}

set.seed(1234)
# uniform
# increasing unif.mean.b increases 
auc.est.fun(n=10000, p=10, unif.min.b=0.2, unif.mean.b=0.2, dist.b="uniform", mean.X=1, sd.X=0.5)
auc.est.fun(n=10000, p=10, unif.min.b=0.2, unif.mean.b=0.4, dist.b="uniform", mean.X=1, sd.X=0.5)
auc.est.fun(n=10000, p=10, unif.min.b=0.2, unif.mean.b=0.6, dist.b="uniform", mean.X=1, sd.X=0.5)
auc.est.fun(n=10000, p=10, unif.min.b=0.2, unif.mean.b=0.8, dist.b="uniform", mean.X=1, sd.X=0.5)
auc.est.fun(n=10000, p=10, unif.min.b=0.2, unif.mean.b=0.9, dist.b="uniform", mean.X=1, sd.X=0.5)

  
# normal
# increasing normal.mean.b increases AUC
auc.est.fun(n=10000, p=10, normal.mean.b=0.1, normal.sd.b=0.4, dist.b="normal", mean.X=1, sd.X=0.5)
auc.est.fun(n=10000, p=10, normal.mean.b=0.5, normal.sd.b=0.4, dist.b="normal", mean.X=1, sd.X=0.5)
auc.est.fun(n=10000, p=10, normal.mean.b=1, normal.sd.b=0.4, dist.b="normal", mean.X=1, sd.X=0.5)
auc.est.fun(n=10000, p=10, normal.mean.b=1.9, normal.sd.b=0.4, dist.b="normal", mean.X=1, sd.X=0.5)


# beta
# increasing beta.ratio.b increases 
auc.est.fun(n=10000, p=10, beta.alpha.b=0.5, beta.ratio.b=0.01, dist.b="beta", mean.X=1, sd.X=0.5)
auc.est.fun(n=10000, p=10, beta.alpha.b=0.5, beta.ratio.b=0.1, dist.b="beta", mean.X=1, sd.X=0.5)
auc.est.fun(n=10000, p=10, beta.alpha.b=0.5, beta.ratio.b=0.5, dist.b="beta", mean.X=1, sd.X=0.5)
auc.est.fun(n=10000, p=10, beta.alpha.b=0.5, beta.ratio.b=0.9, dist.b="beta", mean.X=1, sd.X=0.5)
auc.est.fun(n=10000, p=10, beta.alpha.b=0.5, beta.ratio.b=0.999, dist.b="beta", mean.X=1, sd.X=0.5)

