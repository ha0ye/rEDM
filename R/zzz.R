Rcpp::loadModule("lnlp_module", TRUE)
Rcpp::loadModule("block_lnlp_module", TRUE)
Rcpp::loadModule("xmap_module", TRUE)

.onAttach <- function(...) {
    if (!interactive()) return()
    
    intro_message <- paste("If you're new to the rEDM package, please check out the tutorial:",
                           "> vignette(\"rEDM-tutorial\")", sep = "\n")
    packageStartupMessage(intro_message)
}
