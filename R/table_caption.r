##' get chunk label
##'
##' for convience, an alias for \code{knitr::opts_current$get("label")}
##' @export
bn_clab <- function() knitr::opts_current$get("label")

##' table caption
##'
##' get bookdown enumerable table caption with label from the chunk label as
##'     default. The caption provided will be inserted after "Table:
##'     (\\#tab:<chunk label>)"
##' @param caption character; the caption
##' @param label character; the label. By default, this will be the chunk label,
##'     most of the time you do not want to change this
##' @export
bn_tab_cap <- function(caption, label = NULL){
    get_lab <- if(is.null(label)) bn_clab() else label
    lab <- if(!is.null(get_lab)) get_lab else "noLabel"
    paste0("Table: (\\#tab:", lab, ") ", caption)
}
##' @describeIn bn_tab_cap alias for \code{bn_tab_cap}
##' @export
bn_tc <- bn_tab_cap

## help function to extract label
bn_extract_label_from_caption <- function(caption){
    if(is.null(caption) || is.na(caption)){
        NULL
    } else {
        gsub(pattern = "(Table: )(\\(\\\\#)(tab:.*)(\\))(.*)",
             replacement = "\\3",
             x = caption)
    }
}

##' table caption
##'
##' get bookdown enumerable table caption with label from the chunk label as
##'     default. The caption provided will be inserted after
##'     "(\\#tab:<chunk label>)"
##' @param caption character; the caption
##' @param label character; the label. By default, this will be the chunk label,
##'     most of the time you do not want to change this
##' @export
bn_cap <- function(caption, label = NULL){
    get_lab <- if(is.null(label)) bn_clab() else label
    lab <- if(!is.null(get_lab)) get_lab else "noLabel"
    paste0("(\\#tab:", lab, ") ", caption)
}
##' @describeIn bn_cap alias for \code{bn_cap}
##' @export
bn_c <- bn_cap

if(FALSE){

    (tmp <- bn_tc("A beautiful text", "foo"))
    bn_extract_label_from_caption(tmp)
    bn_extract_label_from_caption(NULL)
    bn_extract_label_from_caption(NA)


}
