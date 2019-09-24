context("Check various warning messages and input checking")

data("two_species_model")
ts <- two_species_model$x[1:200]

test_that("end of range checking works", {
    expect_error(w <- capture_warnings(simplex_out <- simplex(ts, lib = c(1, 201), E = 1)), NA)
    expect_match(w, "end_of_range = 201, but num_vectors = 200", all = FALSE)
    expect_match(w, "end of time_range was greater than the number of vectors; corrected", all = FALSE)
    expect_match(w, "Found overlap between lib and pred. Enabling cross-validation with exclusion radius = 0.", all = FALSE)
    expect_known_hash(round(simplex_out, 4), "982df3363e")
})

test_that("beginning of range checking works", {
    expect_error(w <- capture_warnings(simplex_out <- simplex(ts, lib = c(201, 300), pred = c(101, 102), E = 1)), NA)
    expect_match(w, "start_of_range = 201, but num_vectors = 200", all = FALSE)
    expect_match(w, "start of time_range was greater than the number of vectors; skipping", all = FALSE)
    expect_match(w, "no nearest neighbors found; using NA for forecast", all = FALSE)
    simplex_out$rho <- NaN
    simplex_out$p_val <- NaN
    simplex_out$const_pred_rho <- NaN
    simplex_out$const_p_val <- NaN
    expect_known_hash(round(simplex_out, 4), "e5b3bb5459")
})

test_that("L1 norm works", {
    expect_error(simplex_out <- simplex(ts, lib = c(1, 100), 
                                        pred = c(101, 200), 
                                        norm = 1), 
                 NA)
    expect_known_hash(round(simplex_out, 4), "306097b845")
})

test_that("P-norm with P = 0.5 works", {
    expect_error(simplex_out <- simplex(ts, lib = c(1, 100), 
                                        pred = c(101, 200), 
                                        norm = 0.5), 
                 NA)
    expect_known_hash(round(simplex_out, 4), "0ed9636fd8")
})
