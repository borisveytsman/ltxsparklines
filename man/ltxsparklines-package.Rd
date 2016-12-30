\name{ltxsparklines-package}
\alias{ltxsparklines-package}
\alias{ltxsparklines}
\docType{package}
\title{
\packageTitle{ltxsparklines}
}
\description{
\packageDescription{ltxsparklines}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{ltxsparklines}
\packageIndices{ltxsparklines}

There are several packages that produce sparklines in R: SparkTable by
Alexander Kowarik, Bernhard Meindl, Matthias Templ
(https://CRAN.R-project.org/package=sparkTable),  sparklines by Bart
(https://github.com/Bart6114/sparklines).  However, they either provide
HTML-only output, or require Java libraries.  This is a lightweight and
easy to use package for LaTeX users.

The package provides one function, \code{\link{sparkline}}, which
outputs LaTeX code compatible with LaTeX package \code{sparklines}
(Andreas Loeffer and Dan Luecking, http://www.ctan.org/pkg/sparklines). 
}

\section{Package options}{
  Most options set the defaults for \code{sparkline} command.  They can
  be overriden by the corresponding argument of \code{sparkline}, for
  example, \code{sparkline(...,width=20)}.
  \describe{
    \item{ltxsparklines.width}{The width of the sparkline
  (\code{10}).} 
    \item{ltxsparklines.na.rm}{Whether to delete \code{NA} lines from
      the sparkline data (\code{TRUE}).}
    \item{ltxsparklines.bottomline}{Whether to add the bottom line
      (FALSE).} 
    \item{ltxsparklines.startdotcolor}{The color of the dot at the start
      of the sparkline.  If \code{NA} (the default), do not draw the dot.}
    \item{ltxsparklines.enddotcolor}{The color of the dot at the end
      of the sparkline.  If \code{NA} (the default), do not draw the dot.}
    \item{ltxsparklines.dotcolor}{The color of dots in the sparkline
      (\code{blue}).} 
    \item{ltxsparklines.output}{The output format (\code{knitr}).}
    }
  }

\author{
Boris Veytsman

Maintainer: <borisv@lk.net> 
}
\references{
  Andreas Loeffer and Dan Luecking, Sparklines, 2014,
  http://www.ctan.org/pkg/sparklines
}
\keyword{ package }

\examples{
\dontrun{
sparkline(x=c(1,3,6),
          y=c(5,8,9),
          startdotcolor='blue',
          enddotcolor='red')
}
\dontrun{
sparkline(Nile)
}
}
