% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/async.R
\name{async_ops}
\alias{async_ops}
\title{Async operations}
\usage{
async_ops(package, then, as_promise)
}
\arguments{
\item{package}{The package name of the framework as a
string. \code{async()} and \code{async_generator()} check that the package
is installed at runtime.}

\item{then}{A function of two arguments. The first argument is a
promise object (as created by \code{as_promise}). The second argument
is a callback function that must be called once the promise
object is resolved.}

\item{as_promise}{A function of one argument. It should be a no-op
when passed a promise object, and otherwise wrap the value in a
resolved promise.}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

Customisation point for the \emph{async} package or any concurrency
framework that defines a "then" operation. Assign the result of
\code{async_ops()} to the \code{.__coro_async_ops__.} symbol in your
namespace.
}
\keyword{internal}
