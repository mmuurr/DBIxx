##----------------------------------------
## Imports: DBI
## Depends: methods
##----------------------------------------

setMethod("str", signature(object = "DBIConnection"), function(object) {
    str(DBI::dbGetInfo(object))
})

setMethod("as.character", signature(x = "DBIConnection"), function(x) {
    capture.output(show(x))
})
