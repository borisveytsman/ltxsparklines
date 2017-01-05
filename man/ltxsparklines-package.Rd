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

\section{Package options and setup}{

  Do not forget to add \code{\\usepackage\{sparklines\}} to the preamble
  of your TeX document.
  
  There are two sets of options for
  sparklines in LaTeX: those which are set from TeX, and those which are
  set from R.  The TeX options should be set with \code{\\renewcommand},
  \code{\\definecolor} or \code{\\setlength}, for example,
  \preformatted{
    \% The height of the sparklines in ex units
    \renewcommand\sparklineheight{1.75} 
    \% The line width
    \setlength\sparklinethickness{0.4pt}
    \% The color of the sparkline
    \definecolor{sparklinecolor}{named}{blue}
    \% The color of the sparkine rectangle when present
    \definecolor{sparkrectanglecolor}{gray}{0.8}
    \% The dot width
    \setlength\sparkdotwidth{2pt}
    \% The color of the spikes
    \definecolor{sparkspikecolor}{named}{red}
    \% The color of the bottom line when present
    \definecolor{bottomlinecolor}{gray}{0.2}
    \% The thickness of the bottom line
    \setlength\sparkbottomlinethickness{.2pt}
    \% The clipping separation (need sparklines v1.7 or later)
    \setlength\sparklineclipsep{2pt}    
    }
    See the documentation for the LaTeX \code{sparklines} package.

    
  Most options set from R define the defaults for \code{sparkline}
  command.  They can 
  be overriden by the corresponding argument of \code{sparkline}, for
  example, \code{sparkline(...,width=20)}.
  \describe{
    \item{ltxsparklines.width}{The width of the sparkline
  (\code{10}).} 
    \item{ltxsparklines.clip}{Whether to clip the sparklines in the
  drawing region (\code{FALSE}).  You need LaTeX \code{sparklines}
  package at least v1.7 for \code{clip=TRUE}.} 
    \item{ltxsparklines.na.rm}{Whether to delete \code{NA} lines from
      the sparkline data (\code{TRUE}).}
    \item{ltxsparklines.bottomline}{Whether to add the bottom line
      (FALSE). You need LaTeX
      \code{sparklines} package at least v1.6 to draw bottom
      line.} 
    \item{ltxsparklines.bottomlinex}{The dimensions of the extended
      bottom line (by default \code{c(NA, NA)}, which means  no extended
      bottom line). You need LaTeX
      \code{sparklines} package at least v1.7 to draw extended bottom
      line.} 
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
  Andreas Loeffer and Dan Luecking, Sparklines, 2016,
  http://www.ctan.org/pkg/sparklines
}
\keyword{ package }


\seealso{
  \code{\link{sparkline}}
  \code{vignette(ltxsparklines)}
}
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
