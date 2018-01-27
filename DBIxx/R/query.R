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
        map_chr(stringr::str_trim) %>%
        keep(function(x) stringr::str_length(x) > 0)
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


dbFetch <- function(res, n = -1, ..., .as_f = getOption("DBIxx::.as_f")) {
    .cast_table(DBI::dbFetch(res, n, ...), .as_f)
}


dbExecute <- function(conn, statement, ...) {
    stmts <- .execute_head(conn, statement, ...)
    DBI::dbExecute(conn, stmts, ...)
}
dbExecuteFile <- function(conn, file, ..., .locale = default_locale()) {
    dbExecute(conn, readr::read_file(file, .locale), ...)
}


dbSendStatement <- function(conn, statement, ...) {
    stmts <- .execute_head(conn, statement, ...)
    DBI::dbSendStatement(conn, stmts, ...)
}
dbSendStatementFile <- function(conn, file, ..., .locale = default_locale()) {
    dbSendStatement(conn, readr::read_file(file, .locale), ...)
}


dbSendQuery <- function(conn, statement, ...) {
    stmts <- .execute_head(conn, statement, ...)
    DBI::dbSendQuery(conn, stmts, ...)
}
dbSendQueryFile <- function(conn, file, ..., .locale = default_locale()) {
    dbSendQuery(conn, readr::read_file(file, .locale), ...)
}


dbGetQuery <- function(conn, statement, ..., .as_f = getOption("DBIxx::.as_f")) {
    stmts <- .execute_head(conn, statement, ...)
    .cast_table(DBI::dbGetQuery(conn, stmts, ...), .as_f)
}
dbGetQueryFile <- function(conn, file, ..., .as_f = getOption("DBIxx::.as_f")) {
    dbGetQuery(conn, readr::read_file(file, .locale), ..., .as_f = .as_f)
}
