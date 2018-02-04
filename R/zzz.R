##----------------------------------------
## Imports:
## Suggests: tibble
##----------------------------------------

.onLoad <- function(libname, pkgname) {
    ## default to tibbles just because.
    default_as_f <- if(requireNamespace("tibble", quietly = TRUE)) {
                        tibble::as_tibble
                    } else {
                        NULL
                    }
    options("DBIxx::.as_f" = default_as_f)
}
