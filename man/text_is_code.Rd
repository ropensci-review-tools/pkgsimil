% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{text_is_code}
\alias{text_is_code}
\title{Estimate whether input text string is code or English prose text.}
\usage{
text_is_code(txt)
}
\arguments{
\item{txt}{Single input text string}
}
\value{
Logical value indicating whether or not \code{txt} was identified as
code.
}
\description{
This is only approximate, and there are even software packages which can
give false negatives and be identified as prose (like rOpenSci's "geonames"
package), and prose which may be wrongly identified as code.
}
\examples{
txt <- "Some text without any code"
text_is_code (txt)
txt <- "this_is_code <- function (x) { x }"
text_is_code (txt)
}
\seealso{
Other utils: 
\code{\link{head.pkgmatch}()},
\code{\link{pkgmatch_load_data}()},
\code{\link{print.pkgmatch}()}
}
\concept{utils}
