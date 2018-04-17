\name{t.test2}
\alias{t.test2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
t.test2
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
t.test2(x, y, sigma = c(-1, -1), var.equal = FALSE, side = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{sigma}{
%%     ~~Describe \code{sigma} here~~
}
  \item{var.equal}{
%%     ~~Describe \code{var.equal} here~~
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
function (x, y, sigma = c(-1, -1), var.equal = FALSE, side = 0)
{
    n1 = length(x)
    n2 = length(y)
    xb = mean(x)
    yb = mean(y)
    if (all(sigma >= 0)) {
        z = (xb - yb)/sqrt(sigma[1]^2/n1 + sigma[2]^2/n2)
        list(mu = xb - yb, df = n1 + n2, Z = z, p_value = P_value(pnorm,
            z, side = side))
    }
    else {
        if (var.equal == TRUE) {
            Sw = sqrt(((n1 - 1) * var(x) + (n2 - 1) * var(y))/(n1 +
                n2 - 2))
            t = (xb - yb)/(Sw * sqrt(1/n1 + 1/n2))
            df = n1 + n2 - 2
        }
        else {
            S1 = var(x)
            S2 = var(y)
            df = (S1/n1 + S2/n2)^2/(S1^2/n1^2/(n1 - 1) + S2^2/n2^2/(n2 -
                1))
            t = (xb - yb)/sqrt(S1/n1 + S2/n2)
        }
        list(mu = xb - yb, df = df, t = t, p_value = P_value(pt,
            t, df, side))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
