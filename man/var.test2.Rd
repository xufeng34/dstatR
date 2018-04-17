\name{var.test2}
\alias{var.test2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
var.test2
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
var.test2(x, y, mu = c(Inf, Inf), side = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{mu}{
%%     ~~Describe \code{mu} here~~
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
function (x, y, mu = c(Inf, Inf), side = 0)
{
    n1 = length(x)
    n2 = length(y)
    if (all(mu < Inf)) {
        Sx2 = sum((x - mu[1])^2)/n1
        Sy2 = sum((y - mu[2])^2)/n2
        df1 = n1
        df2 = n2
    }
    else {
        Sx2 = var(x)
        Sy2 = var(y)
        df1 = n1 - 1
        df2 = n2 - 1
    }
    F = Sx2/Sy2
    P = P_value(pf, F, df = c(df1, df2), side = side)
    list(S1 = Sx2, S2 = Sy2, df1 = df1, df2 = df2, F = F, p_value = P)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
