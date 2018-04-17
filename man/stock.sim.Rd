\name{stock.sim}
\alias{stock.sim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
stock.sim
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
stock.sim(N = 100, File = "", Seed = 123)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{N}{
%%     ~~Describe \code{N} here~~
}
  \item{File}{
%%     ~~Describe \code{File} here~~
}
  \item{Seed}{
%%     ~~Describe \code{Seed} here~~
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
function (N = 100, File = "", Seed = 123)
{
    set.seed(Seed)
    R1 = rbinom(N, 1, 0.3) + 1
    R2 = round(rnorm(N, 40, 10), 0)
    R2[R2 < 18] <- 0
    R2[R2 > 70] <- 0
    R3 = rbinom(N, 2, 0.75) + 1
    R4 = rbinom(N, 3, 0.5) + 1
    R5 = rbinom(N, 1, 0.6) + 1
    R6 = rbinom(N, 1, 0.85) + 1
    R7 = round(runif(N, 1, 1100), 1)
    R7[sample(1:N, 1)] <- 0
    R8 = rbinom(N, 7, 0.5) + 1
    R9 = rbinom(N, 7, 0.7) + 1
    RX = data.frame(sex = R1, age = R2, result = R3, method = R4,
        risk = R5, post = R6, fund = R7, job = R8, edu = R9)
    if (File == "")
        RX
    else write.csv(RX, file = File, row.names = FALSE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
