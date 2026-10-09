#' Lancer l'application Shiny comprox
#'
#' @param ... Arguments supplémentaires passés à \code{\link[shiny]{shinyApp}}
#' @export
#' @examples
#' \textignore{
#'   comprox::run_app()
#' }
run_app <- function(...) {
  
  shiny::shinyApp(ui = app_ui, server = app_server, ...)
}
