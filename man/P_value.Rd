\name{P_value}
\alias{P_value}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
P_value
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
P_value(cdf, x, df = numeric(0), side = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cdf}{
%%     ~~Describe \code{cdf} here~~
}
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{df}{
%%     ~~Describe \code{df} here~~
}
  \item{side}{
%%     ~~Describe \code{side} here~~
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
function (cdf, x, df = numeric(0), side = 0)
{
    k = length(df)
    P = switch(k + 1, cdf(x), cdf(x, df), cdf(x, df[1], df[2]),
        cdf(x, df[1], df[2], df[3]))
    if (side < 0)
        P
    else if (side > 0)
        1 - P
    else if (P < 1/2)
        2 * P
    else 2 * (1 - P)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
