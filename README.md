
# flowery

[![Travis-CI Build Status](https://travis-ci.org/lionel-/flowery.svg?branch=master)](https://travis-ci.org/lionel-/flowery)
![](https://img.shields.io/badge/lifecycle-experimental-red.svg)


## Overview

Flowery is an experimental package implementing generators in R. It
also provides transducers for transforming iterators in a pipeline.
See the [iteration vignette](http://rpubs.com/lionel-/iteration-draft)
for more information.

In the future the engine used to create generators will also be used
for async/await functions and CSP coroutines.


## Installation

Install the development version from github with:

```r
# install.packages("devtools")
devtools::install_github("lionel-/flowery", build_vignettes = TRUE)
```
