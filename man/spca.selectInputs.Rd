\name{spca.selectInputs}
\alias{spca.selectInputs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
spca.selectInputs
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
spca.selectInputs(input, target, alpha, family = gaussian())
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
    num_col <- ncol(input)
    keep <- logical(num_col)
    for (i in 1:num_col) {
        m1 <- glm.fit(matrix(c(rep(1, length(input[[i]])), as.numeric(as.vector(input[[i]]))), 
            ncol = 2), as.vector(target), family = family)
        if (abs(coef(m1)[2]) > alpha) {
            keep[i] = TRUE
        }
        else {
            keep[i] = FALSE
        }
    }
    return(keep)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
