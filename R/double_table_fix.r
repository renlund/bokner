## helper function for bn_double_table_fix functions
bn_double_table_arg_tester <- function(input_given, input_pattern, prefix_given){
    ## make sure input is in working directory
    if(grepl(.Platform$file.sep, input_given, fixed = TRUE)){
        s <- paste0("input needs to be in the working directory")
        stop(s)
    }
    ## check if file has docx file ending
    if(!grepl(input_pattern, input_given)){
        s <- paste0("light warning: input does not match ", input_pattern)
        message(s)
    }
    ## prefix should not be empty
    if(is.null(prefix_given) | prefix_given == ""){
        s <- paste0("input cannot be empty")
        stop(s)
    }
}

##' fix compiled file
##'
##' Some table code generators (I'm thinking specifically of huxtable) have to
##'     have caption set to "Table: (\#<label>)" in order for automatic
##'     enumeration with bookdown to work for "all" formats (html, pdf, docx),
##'     with the side effect that tables will have captions starting with
##'     "Table: Table". This function simply substitutes "Table: Table" with
##'     "Table:". Note: this function is for docx and html, for pdf you need a
##'     'hook' - run \code{bn_double_table_fix(".pdf")} for more info
##' @param input character; name of docx file in the working directory
##' @param keep_input logical; should input be kept?
##' @param prefix character; if input is kept, the prefix to use
##' @return NULL; one or two files are written/manipulated as a side effect
##' @export
bn_double_table_fix <- function(input, keep_input = TRUE, prefix = "raw-"){
    if(grepl("\\.docx$", input)){
        bn_double_table_fix_docx(input = input, keep_input = keep_input)
    } else if(grepl("\\.html$", input)){
        bn_double_table_fix_html(input = input, keep_input = keep_input)
    } else if(grepl("\\.pdf$", input)){
        s = paste0("pdf:s are NOT handled by this function, insted use ",
                   "the hook supplied by 'bn_double_table_latex_hook'. ")
        message(s)
        bn_double_table_latex_hook(NULL)
    } else {
        s = paste0("I do not know what to do with this file\n",
                   "(I want a docx- or html file)")
        message(s)
    }
    invisible(NULL)
}

##' @describeIn bn_double_table_fix fix double table instance in docx file
##' @export
bn_double_table_fix_docx <- function(input, keep_input = TRUE, prefix = "raw-"){
    ## check arguments
    bn_double_table_arg_tester(input_given = input,
                               input_pattern = "\\.docx$",
                               prefix_given = prefix)
    ## determine new names and save input
    output_name = input
    input_new_name = paste0(prefix, input)
    file.copy(from = input, to = input_new_name)
    ## create name of container to unzip input to
    slump <- paste0(sample(LETTERS, 10), collapse = "")
    tmp.zip <- paste0("temporary-file-name-for-unzipped-", slump)
    if(file.exists(tmp.zip)){
        s <- paste0("temp file '", tmp.zip, "' already exists.")
        stop(s)
    }
    unzip(zipfile = input, exdir = tmp.zip)
    ## read the relevant part of the unzipped file and make substitution
    text <- file.path(tmp.zip, "word", "document.xml")
    rl <- readLines(text, warn = FALSE)
    n <- length(rl)
    for(i in 1:n){
        rl[i] <- gsub(pattern = "Table: Table",
                      replacement = "Table",
                      x = rl[i],
                      fixed = TRUE)
    }
    cat(rl, sep = "\n", file = text)
    ## remove original input and rezip the substituted file into a file with the
    ##   same name
    file.remove(input)
    curr_wd <- setwd(tmp.zip)
    zip(zipfile = file.path("../", output_name),
        files = list.files(all.files = TRUE, recursive=TRUE),
        flags = "-q")
    setwd(curr_wd)
    ## remove unzipped files
    unlink(tmp.zip, recursive = TRUE)
    ## display info and remove input if keep_input is FALSE
    cat("## updated file '", output_name, "'\n", sep = "")
    if(!keep_input){
        file.remove(input_new_name)
    } else {
        cat("## stored source as '", input_new_name, "'\n", sep = "")
    }
    invisible(NULL)
}

##' @describeIn bn_double_table_fix fix double table instance in html file
##' @export
bn_double_table_fix_html <- function(input, keep_input = TRUE, prefix = "raw-"){
    ## check arguments
    bn_double_table_arg_tester(input_given = input,
                               input_pattern = "\\.html$",
                               prefix_given = prefix)
    ## determine new names and save input
    output_name = input
    input_new_name = paste0(prefix, input)
    file.copy(from = input, to = input_new_name)
    ## read input and make substitution
    rl <- readLines(input)
    rl2 <- gsub(pattern = "(.*)(Table: \\(\\\\#tab:.*\\))(.*)",
                replacement = "\\1\\3", x = rl)
    cat(rl2, file = output_name)
    ## display info and remove input if keep_input is FALSE
    cat("## wrote file", output_name, "\n")
    if(!keep_input){
        file.remove(input_new_name)
    } else {
        cat("## stored source as '", input_new_name, "'\n", sep = "")
    }
    invisible(NULL)
}

##' fix pre-compiled file
##'
##' bookdown with pdf output are in some instances subject to the same problems
##'     described in \code{\link{bn_double_table_fix}}. This 'hook' tries to
##'     remove that problem. Insert this into a relevant setup chunk:
##'     \code{knit_hooks$set(document = bn_double_table_latex_hook)}
##' @param x argument used by internal knitr process; set to \code{NULL}
##'     (default) to get a helpful message
##' @export
bn_double_table_latex_hook <- function(x = NULL){
    if(is.null(x)){
        s <- paste0("This function must operate on the tex-file before ",
                    "compilation into a pdf and must thus be set in the ",
                    "rnw file. Insert this into a relevant setup chunk:\n",
                    "\n   knit_hooks$set(document = ",
                    "bn_double_table_latex_hook)\n")
        message(s)
    } else {
        r <- x
        for(i in seq_along(x)){
            y <- unlist(strsplit(x[i], split = "\n"))
            z <- bn_double_table_latex_hook_helper(y)
            r[i] <- paste0(z, collapse = "\n")
        }
        r
    }
}

## helper function
bn_double_table_latex_hook_helper <- function(x){
    caption_start = "(\\\\caption\\{)"    ## \\1
    poss_label = "(.*)"                   ## \\2
    markdown_label = "(Table: \\(.*\\))"  ## \\3
    caption_text = "(.*)"                 ## \\4
    caption_end = "(\\})"                 ## \\5
    Pattern = paste0(caption_start,
                     poss_label,
                     markdown_label,
                     caption_text,
                     caption_end)
    sub(pattern = Pattern,
        replacement = '\\1\\2\\4\\5',
        x  = x)
}

if(FALSE){

    bn_double_table_arg_tester("foo.docx", "\\.docx$", "yeah")
    bn_double_table_arg_tester("foo.docx", "\\.docx$", "")
    bn_double_table_arg_tester("foo.word", "\\.docx$", "yeah")
    bn_double_table_arg_tester("bar/foo.docx", "\\.docx$", "yeah")
    bn_double_table_latex_hook()
    bn_double_table_fix(".pdf")

}
