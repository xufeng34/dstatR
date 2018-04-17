\name{z.conf.plot}
\alias{z.conf.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
z.conf.plot
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
z.conf.plot(conf.level = 0.95)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{conf.level}{
%%     ~~Describe \code{conf.level} here~~
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
function (conf.level = 0.95)
{
    x = seq(-3, 3, 0.1)
    curve(dnorm(x), -3, 3)
    legend(-1.3, 0.3, paste("1-a=", conf.level), bty = "n")
    a = 1 - conf.level
    za = qnorm(c(a/2, 1 - a/2))
    abline(v = c(za), lty = 3)
    za
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
