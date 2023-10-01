
# ======================================== #
# Print Headline in Console
# ======================================== #

#' Print Headline
#'
#' Diese Funktion erzeugt eine Überschrift in der Konsole.
#' Das ist z.B. in Schleifen zwischen der Ausgabe von Eregbnissen gelegentlich praktisch.
#' @param text Text der Überschrift.
#' @param n_signs Breite der Überschrift.
#' @param style Zeichen, aus dem die Überschrift bestehen soll.
# @return Die Summe von \code{x} und \code{y}.
#' @examples
#' headline('Results of calculations', n_signs = 65, style = '-')
#' @export

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
