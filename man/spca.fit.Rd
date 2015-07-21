\name{spca.fit}
\alias{spca.fit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
spca.fit
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
spca.fit(input, target, alpha, family = gaussian())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
%%     ~~Describe \code{input} here~~
}
  \item{target}{
%%     ~~Describe \code{target} here~~
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
}
  \item{family}{
%%     ~~Describe \code{family} here~~
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
function (input, target, alpha, family = gaussian()) 
{
    keep = spca.selectInputs(input, target, alpha, family)
    input = input[, keep]
    input = apply(input, 2, function(x) as.numeric(x))
    pca_matrix <- spca.computePCA(input)
    input = predict(pca_matrix, input)
    m1 <- glm.fit(input, target, family = family)
    class(m1) = append(class(m1), "spca")
    m1$pca_matrix = pca_matrix
    m1$keep_columns_index = keep
    return(m1)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
