\name{logisticSPCA-package}
\alias{logisticSPCA-package}
\alias{logisticSPCA}
\docType{package}
\title{
Logistic Supervised PCA
}
\description{
This package is an extension of the supervised PCA algorithm developed by Bair, Hastie et al. (2006)
so that supervised PCA can be used with other distributions as well. The primary purpose was to be used
for logistic regression for the thesis of Stylianos Kampakis (2015).
}
\details{
\tabular{ll}{
Package: \tab logisticSPCA\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-07-20\cr
License: \tab GPL-3\cr
}
}
\author{
Stylianos Kampakis

Maintainer: Who to complain to stylianos.kampakis@gmail.com
}

\references{

Kampakis, Stylianos, "Predictive Modelling of Football Injuries", PhD Thesis, University College London (2015)

Bair, Eric, et al. "Prediction by supervised principal components." Journal of the American Statistical Association 101.473 (2006)

}

\keyword{ package }
\seealso{
Package 'superpc' \code{\link{https://cran.r-project.org/web/packages/superpc/superpc.pdf}} ~~
}
\examples{

#model a dataset

x<-data.frame(matrix(rnorm(1000*20),ncol=20))
y<-rowSums(x)
y[y>0]=1
y[y<=0]=0

m=spca.fit(x,y,alpha=0.1)
summary(m)

#get the PCA object from the analysis

m$pca_matrix
}
