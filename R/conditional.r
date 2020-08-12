##' get output format
##'
##' for convience, an alias for \code{knitr::opts_knit$get("rmarkdown.pandoc.to")}
##' @export
bn_rmp2 <- function() knitr::opts_knit$get("rmarkdown.pandoc.to")

##' test for output format
##'
##' check if knitr option 'rmarkdown.pandoc.to' is a given value
##' @param X format to test for
##' @export
bn_is_X <- function(X = NULL){
    test <- bn_rmp2() ## knitr::opts_knit$get("rmarkdown.pandoc.to")
    if(is.null(X)){
        message("## no testing format")
        FALSE
    } else if(is.null(test)){
        message("## knitr option 'rmarkdown.pandoc.to' ",
                "only set during compilation")
        FALSE
    } else if(test == X){
        TRUE
    } else FALSE
}
##' @describeIn bn_is_X test for docx
##' @export
bn_is_docx <- function() bn_is_X("docx")
##' @describeIn bn_is_X test for latex
##' @export
bn_is_latex <- function() bn_is_X("latex")
##' @describeIn bn_is_X test for html
##' @export
bn_is_html <- function() bn_is_X("html")

##' conditional code
##'
##' if format is X, insert conditional code specified by 'cc'
##' @param X format
##' @param cc character; code conditional on format being X
##' @export
bn_if_X <- function(X, cc) if(bn_is_X(X)) cc else ""
##' @describeIn bn_if_X code conditional on format being docx
##' @export
bn_if_docx <- function(cc) bn_if_X("docx", cc)
##' @describeIn bn_if_X code conditional on format being latex
##' @export
bn_if_latex <- function(cc) bn_if_X("latex", cc)
##' @describeIn bn_if_X code conditional on format being html
##' @export
bn_if_html <- function(cc) bn_if_X("html", cc)


##' set new page
##'
##' insert relevant code for 'newpage' in all formats specified by argument
##'     'format' (support for latex and docx)
##' @param format character vector of formats
##' @export
bn_newpage <- function(format = c("docx", "latex")){
    r <- NULL
    if("docx" %in% format) r <- bn_newpage_docx()
    if("latex" %in% format) r <- c(r, "\n", bn_newpage_latex())
    r
}
##' @describeIn bn_newpage newpage code for docx
##' @export
bn_newpage_docx <- function(){
    bn_if_X(X = "docx",
            cc = paste0(c("```{=openxml}",
                          "<w:p>",
                          "  <w:r>",
                          "    <w:br w:type='page'/>",
                          "  </w:r>",
                          "</w:p>",
                          "```"), collapse = "\n"))
}
##' @describeIn bn_newpage newpage code for latex
##' @export
bn_newpage_latex <- function(){
    bn_if_X(X = "latex",
            cc = "\n\\newpage\n")
}

if(FALSE){

    bn_is_X("foo")
    bn_newpage_latex()

}
