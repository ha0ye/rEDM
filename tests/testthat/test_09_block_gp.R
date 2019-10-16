context("Check block_gp function")

data("two_species_model")
block <- two_species_model[1:200, ]

test_that("block_gp works", {
    expect_error(output <- block_gp(block, columns = c("x", "y"), 
                                    phi = 0.5, 
                                    v_e = seq(from = -300, to = 300, by = 50),
                                    eta = 7,
                                    fit_params = FALSE,
                                    first_column_time = TRUE, silent = TRUE),
                 NA)
    expect_s3_class(output, "data.frame")
    expect_true("embedding" %in% names(output))
    expect_true("tp" %in% names(output))
    expect_true("phi" %in% names(output))
    expect_true("v_e" %in% names(output))
    expect_true("eta" %in% names(output))
    expect_true("fit_params" %in% names(output))
    expect_true("num_pred" %in% names(output))
    expect_true("rho" %in% names(output))
    expect_true("mae" %in% names(output))
    expect_true("rmse" %in% names(output))
    expect_equal(NROW(output), 13)
    expect_known_hash(round(output$rho, 4), "00056d3065")
})

test_that("block_gp handels 1 prediction", {
    expect_error(output <- block_gp(block, 
                                    lib = c(1, 50), 
                                    pred = c(50, 51), 
                                    columns = "x", 
                                    phi = -12.812, 
                                    v_e = 1.975, 
                                    eta = -0.576, 
                                    fit_params = FALSE, 
                                    stats_only = FALSE), 
                 NA)
    expect_s3_class(output, "data.frame")
    expect_true("model_output" %in% names(output))
    expect_true(is.list(output$model_output))
    expect_error(model_output <- output$model_output[[1]], NA)
    expect_s3_class(model_output, "data.frame")
    expect_true("time" %in% names(model_output))
    expect_true("obs" %in% names(model_output))
    expect_true("pred" %in% names(model_output))
    expect_true("pred_var" %in% names(model_output))
    expect_equal(dim(model_output), c(1, 4))
    expect_known_hash(round(model_output, 4), "a824dc28bd")
})

test_that("block_gp model_output works", {
    expect_error(output <- block_gp(block, columns = c("x", "y"), 
                                    fit_params = TRUE,
                                    phi = 0.5, v_e = -170, eta = 7, 
                                    first_column_time = TRUE, 
                                    stats_only = FALSE, 
                                    silent = TRUE),
                 NA)
    expect_s3_class(output, "data.frame")
    expect_true("model_output" %in% names(output))
    expect_true(is.list(output$model_output))
    expect_error(model_output <- output$model_output[[1]], NA)
    expect_s3_class(model_output, "data.frame")
    expect_true("time" %in% names(model_output))
    expect_true("obs" %in% names(model_output))
    expect_true("pred" %in% names(model_output))
    expect_true("pred_var" %in% names(model_output))
    expect_equal(dim(model_output), c(199, 4))
    expect_known_hash(round(model_output, 4), "412915324b")
})

test_that("block_gp covariance matrix works", {
    expect_error(output <- block_gp(block, columns = c("x", "y"), 
                                    phi = 0.5, v_e = -170, eta = 7, 
                                    first_column_time = TRUE, 
                                    save_covariance_matrix = TRUE, 
                                    silent = TRUE),
                 NA)
    expect_s3_class(output, "data.frame")
    expect_true("model_output" %in% names(output))
    expect_true(is.list(output$model_output))
    expect_true("covariance_matrix" %in% names(output))
    expect_true(is.list(output$covariance_matrix))
    expect_error(covariance_matrix <- output$covariance_matrix[[1]], NA)
    expect_is(covariance_matrix, "matrix")
    expect_equal(dim(covariance_matrix), c(199, 199))
    expect_known_hash(round(covariance_matrix, 4), "df5a7a72ab")
})

test_that("block_gp works on multivariate time series", {
    expect_error(output <- block_gp(EuStockMarkets[1:300, ], columns = c("DAX", "SMI"),
                                    target_column = "CAC", 
                                    phi = 0.5, v_e = 0.005, eta = 100, 
                                    stats_only = FALSE, 
                                    silent = TRUE),
                 NA)
    model_output <- round(output$model_output[[1]], 4)
    expect_known_hash(model_output, "5b6e433b50")
    
    output <- output[, !(names(output) %in% "model_output")]
    output <- data.frame(lapply(output, function(y) 
        if (is.numeric(y)) round(y, 4) else y))
    attributes(output) <- attributes(output)[sort(names(attributes(output)))]
    expect_known_hash(output, "e950f04518")
})
