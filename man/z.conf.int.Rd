\name{z.conf.int}
\alias{z.conf.int}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
z.conf.int
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
z.conf.int(x, sigma, conf.level = 0.95)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{sigma}{
%%     ~~Describe \code{sigma} here~~
}
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
function (x, sigma, conf.level = 0.95)
{
    n = length(x)
    a = 1 - conf.level
    Za = qnorm(1 - a/2)
    se = sigma/sqrt(n)
    c(mean(x) - se * Za, mean(x) + se * Za)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
