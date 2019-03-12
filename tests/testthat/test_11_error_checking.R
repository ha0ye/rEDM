context("Check various warning messages and input checking")

data("two_species_model")
ts <- two_species_model$x[1:200]

test_that("end of range checking works", {
    expect_warning(simplex_out <- simplex(ts, lib = c(1, 201), E = 1), 
                   "end_of_range = 201, but num_vectors = 200")
    expect_warning(simplex_out <- simplex(ts, lib = c(1, 201), E = 1), 
                   "end of time_range was greater than the number of vectors; corrected")
    expect_warning(simplex_out <- simplex(ts, lib = c(1, 201), E = 1), 
                   "Found overlap between lib and pred. Enabling cross-validation with exclusion radius = 0.")
    expect_known_hash(simplex_out, "0fcd00b55d")
})

test_that("beginning of range checking works", {
    expect_warning(simplex_out <- simplex(ts, lib = c(201, 300), pred = c(101, 102), E = 1), 
                   "start_of_range = 201, but num_vectors = 200")
    expect_warning(simplex_out <- simplex(ts, lib = c(201, 300), pred = c(101, 102), E = 1), 
                   "start of time_range was greater than the number of vectors; skipping")
    expect_warning(simplex_out <- simplex(ts, lib = c(201, 300), pred = c(101, 102), E = 1), 
                   "no nearest neighbors found; using NA for forecast")
    expect_known_hash(simplex_out, "9e5a38a7bc")
})

test_that("L1 norm works", {
    expect_error(simplex_out <- simplex(ts, lib = c(1, 100), 
                                        pred = c(101, 200), 
                                        norm = 1), 
                 NA)
    expect_known_hash(simplex_out, "0b8aae1d08")
})

test_that("P-norm with P = 0.5 works", {
    expect_error(simplex_out <- simplex(ts, lib = c(1, 100), 
                                        pred = c(101, 200), 
                                        norm = 0.5), 
                 NA)
    expect_known_hash(round(simplex_out, 4), "0ed9636fd8")
})
