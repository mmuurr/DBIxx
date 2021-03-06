##----------------------------------------
## Imports: sqlparse, DBI, purrr, readr
##----------------------------------------

## This splits `statement` into individual SQL statements.
## All statements prior to the final statement are executed via `DBI::dbExecute()`.
## The final SQL statement is then returned to the caller.
.execute_head <- function(conn, statement, ...) {
    stmts <- statement %>%
        sqlparse::format(strip_comments = TRUE) %>%
        sqlparse::split() %>%
        purrr::map_chr(stringr::str_trim) %>%
        purrr::keep(function(x) stringr::str_length(x) > 0)
    purrr::walk(head(stmts, -1), function(stmt) DBI::dbExecute(conn, stmt, ...))
    return(tail(stmts, 1))
}


## Tries to cast a data.frame to a package-specific table.
.cast_table <- function(df, as_f = NULL) {
    if(is.null(as_f)) return(df)
    tryCatch({
        return(as_f(df))
    }, error = function(e) {
        warning(conditionMessage(e), call. = FALSE)
        return(df)
    })
}


#' @inherit DBI::dbFetch
dbFetch <- function(res, n = -1, ..., .as_f = getOption("DBIxx::.as_f")) {
    .cast_table(DBI::dbFetch(res, n, ...), .as_f)
}


dbExecute <- function(conn, statement, ...) {
    final_stmt <- .execute_head(conn, statement, ...)
    DBI::dbExecute(conn, final_stmt, ...)
}
dbExecuteFile <- function(conn, file, ..., .locale = readr::default_locale()) {
    dbExecute(conn, readr::read_file(file, .locale), ...)
}


dbSendStatement <- function(conn, statement, ...) {
    final_stmt <- .execute_head(conn, statement, ...)
    DBI::dbSendStatement(conn, final_stmt, ...)
}
dbSendStatementFile <- function(conn, file, ..., .locale = readr::default_locale()) {
    dbSendStatement(conn, readr::read_file(file, .locale), ...)
}


dbSendQuery <- function(conn, statement, ...) {
    final_stmt <- .execute_head(conn, statement, ...)
    DBI::dbSendQuery(conn, final_stmt, ...)
}
dbSendQueryFile <- function(conn, file, ..., .locale = readr::default_locale()) {
    dbSendQuery(conn, readr::read_file(file, .locale), ...)
}


dbGetQuery <- function(conn, statement, ..., .as_f = getOption("DBIxx::.as_f")) {
    final_stmt <- .execute_head(conn, statement, ...)
    .cast_table(DBI::dbGetQuery(conn, final_stmt, ...), .as_f)
}
dbGetQueryFile <- function(conn, file, ..., .locale = readr::default_locale(), .as_f = getOption("DBIxx::.as_f")) {
    dbGetQuery(conn, readr::read_file(file, .locale), ..., .as_f = .as_f)
}
