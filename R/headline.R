# ======================================== #
# Print Headline in Console
# ======================================== #
headline <- function(text, n_signs = 70, style = '=') {
    sep <- paste(rep(style, n_signs),
                 collapse = "")
    sep <- paste0(sep, '\n')
    text <- paste0(text, '\n')

    # Print
    '\n' %>% cat
    sep %>% cat
    text %>% cat
    sep %>% cat
    '\n' %>% cat
}