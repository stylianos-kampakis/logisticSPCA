\name{spca.predict}
\alias{spca.predict}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
spca.predict
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
spca.predict(model, input, num_components)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
%%     ~~Describe \code{model} here~~
}
  \item{input}{
%%     ~~Describe \code{input} here~~
}
  \item{num_components}{
%%     ~~Describe \code{num_components} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (model, input, num_components) 
{
    if (is.null(pca_matrix)) {
        pca_matrix <- spca.computePCA(input, target, alpha, family)
    }
    input <- data.frame(predict(pca_matrix, data))
    if (num_components < ncol(inputs)) {
        inputs = inputs[, 1:num_components]
    }
    res <- predict(model, input, type = "response")
    return(res)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
