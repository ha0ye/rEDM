loadModule("lnlp_module", TRUE)
loadModule("block_lnlp_module", TRUE)
loadModule("xmap_module", TRUE)

.onAttach <- function(...) {
    packageStartupMessage("*** WARNING *** This is the `dev` branch of the rEDM package. *** WARNING ***")
    
    if (!interactive()) return()
    
    intro_message <- paste("If you're new to the rEDM package, please check out the tutorial:",
                           "> vignette(\"rEDM_tutorial\")", sep = "\n")
    packageStartupMessage(intro_message)
}