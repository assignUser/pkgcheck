
#' Check whether all man files have been generated by 'roxygen2'
#'
#' @param checks A 'pkgcheck' object with full \pkg{pkgstats} summary and
#' \pkg{goodpractice} results.
#' @return 'TRUE' if all files generated by 'roxygen2', otherwise 'FALSE'
#' @noRd
pkgchk_uses_roxygen2 <- function (checks) {

    rd <- list.files (
        file.path (checks$pkg, "man"),
        pattern = "\\.Rd$",
        full.names = TRUE
    )

    chk <- vapply (rd, function (i) {
        l1 <- readLines (i, n = 1L, encoding = "UTF-8")
        grepl ("Generated by roxygen2", l1, ignore.case = TRUE)
    },
    logical (1),
    USE.NAMES = FALSE
    )

    return (all (chk))
}

output_pkgchk_uses_roxygen2 <- function (checks) {

    out <- list (
        check_pass = checks$checks$uses_roxygen2,
        summary = "",
        print = ""
    ) # no print method

    out$summary <- ifelse (out$check_pass,
        "uses 'roxygen2'.",
        "does not use 'roxygen2'."
    )

    return (out)
}
