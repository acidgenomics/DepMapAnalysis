% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/plotGeneEffect-methods.R
\name{plotGeneEffect}
\alias{plotGeneEffect}
\alias{plotGeneEffect,Achilles-method}
\alias{plotGeneEffect,DEMETER2-method}
\title{Plot gene effect}
\usage{
plotGeneEffect(object, ...)

\S4method{plotGeneEffect}{Achilles}(object, genes, geom = c("boxplot", "density"))

\S4method{plotGeneEffect}{DEMETER2}(object, genes, geom = c("boxplot", "density"))
}
\arguments{
\item{object}{Object.}

\item{...}{Additional arguments.}

\item{genes}{\code{character}.
Gene identifiers.}

\item{geom}{\code{character(1)}.
Plot type. Uses \code{\link[base:match.arg]{match.arg()}} internally and defaults to
the first argument in the \code{character} vector.}
}
\value{
\code{ggplot}.
}
\description{
Plot gene effect
}
\note{
Updated 2020-10-07.
}
\examples{
data(ach, dem)

## Achilles ====
object <- ach
genes <- rownames(object)[seq_len(5L)]
plotGeneEffect(object, genes = genes)

## DEMETER2 ====
object <- dem
genes <- rownames(object)[seq_len(5L)]
plotGeneEffect(object, genes = genes)
}
