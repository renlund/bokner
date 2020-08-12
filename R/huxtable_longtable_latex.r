##' huxtable longtable fixer
##'
##' \code{huxtable} allows the setting of the (latex) tabular environment to be
##'     specified. Providing 'longtable' doesn't work since this will be wrapped
##'     in a 'table' environment. However, since the tabular environment created
##'     is fine, it can be extracted. As this has no effect on html and docx
##'     output, this wrapper will do nothing if the format is missing, html, or
##'     docx, or if the attribute \code{tabular_environment} is anything other
##'     than 'longtable'. (In these cases the input will simple be returned
##'     as-is.) If output format is latex AND the tabular environment is
##'     'longtable', then the tabular will be extracted and used as-is (thus
##'     removing the ambient table environment).
##' @param h a huxtable
##' @export
bn_if_long_ht <- function(h){
    output <- bn_rmp2()
    te <- attr(h, "tabular_environment")
    if(!("huxtable" %in% class(h)) || is.null(te) ||
       is.na(te) || is.null(output)) return(h)
    if(te == "longtable" && output == "latex"){
        cap <- attr(h, "caption")
        ## is it better to extract or construct the label?
        extract_label <- TRUE
        if(extract_label){
            lab <- bn_extract_label_from_caption(cap)
            if(is.null(lab)) lab <- "tab:NoLaBel"
        } else {
            cl <- bn_clab() ## knitr::opts_current$get("label")
            lab <- if(!is.null(cl)) paste0("tab:", cl) else "tab:NoLaBel"
        }
        x <- to_latex(h, tabular_only = TRUE)
        ## manipulpulate x to insert caption and label
        y <- unlist(strsplit(x, split = "\n"))
        i <- which(grepl("\\begin{longtable}", y, fixed = TRUE))[1]
        ymod <- c(y[1:i],
                  paste0("\\caption{\\label{", lab, "} ",
                         cap, "}\\tabularnewline"),
                  y[(i+1):length(y)])
        cat(paste0(ymod, collapse = "\n"))
    } else {
        h
    }
}
