library(testthat)
library(rEDM)

if ("sample.kind" %in% names(formals(RNGkind)))
{
    suppressWarnings(RNGkind(sample.kind = "Rounding"))
}

test_check("rEDM")
