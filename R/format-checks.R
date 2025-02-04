
#' Convert checks to markdown-formatted report
#'
#' @param checks Result of main \link{pkgcheck} function
#' @param render If `TRUE`, render output as `html` document and open in
#' browser.
#' @return Markdown-formatted version of check report
#' @family extra
#' @export
checks_to_markdown <- function (checks, render = FALSE) {

    md_chks <- summarise_all_checks (checks)

    md_out <- c (
        paste0 (
            "## Checks for [", checks$pkg$name,
            " (v", checks$pkg$version, ")](",
            checks$pkg$url, ")"
        ),
        "",
        paste0 (
            "git hash: [",
            substring (checks$info$git$HEAD, 1, 8),
            "](",
            checks$pkg$url,
            "/tree/",
            checks$info$git$HEAD,
            ")"
        ),
        "",
        md_chks,
        "",
        paste0 ("Package License: ", checks$pkg$license),
        "",
        "---",
        ""
    )

    md_out <- c (
        md_out,
        srr_checks_to_md (checks)
    )

    # sec_nun is (1, 2) for (srr, non-srr) packages
    sec_num <- as.integer (!is.null (checks$info$srr)) + 1
    stats_rep <- pkgstats_format (checks, sec_num)

    md_out <- c (
        md_out,
        stats_rep,
        "",
        pkg_network (checks, sec_num),
        "",
        "---",
        "",
        paste0 (
            "### ",
            sec_num + 1,
            ". `goodpractice` and other checks"
        ),
        ""
    )

    has_gp <- "goodpractice" %in% names (checks)
    if (has_gp) {
        md_out <- c (
            md_out,
            "<details>",
            paste0 (
                "<summary>Details of goodpractice and other ",
                "checks (click to open)</summary>"
            ),
            "<p>",
            "",
            print_check (checks, "ci"),
            "",
            "---",
            "",
            gp_checks_to_md (checks),
            "",
            "</p>",
            "</details>"
        )
    } else {
        md_out <- c (
            md_out,
            "('goodpractice' not included with these checks)"
        )
    }

    extra <- extra_check_prints_from_env (checks)
    has_extra <- length (extra$env) > 0L |
        length (checks$checks$has_scrap) > 0L |
        length (checks$checks$obsolete_pkg_deps) > 0L
    if (has_extra) {
        e <- env2namespace ("pkgcheck")
        md_out <- c (
            md_out,
            "",
            "---",
            "",
            paste0 ("### ", sec_num + 2, ". Other Checks"),
            ""
        )
        extras <- c ("has_scrap", "obsolete_pkg_deps")
        lens <- vapply (
            extras, function (i) {
                length (checks$checks [[i]])
            },
            integer (1)
        )
        extras <- extras [which (lens > 0L)]
        for (ex in extras) {
            md_out <- c (
                md_out,
                print_check_md (
                    checks,
                    ex,
                    env2namespace ("pkgcheck")
                )
            )
        }

        for (e in extra$env) {
            for (p in extra$prints) {
                md_out <- c (
                    md_out,
                    print_check_md (checks, p, e)
                )
            }
        }
    }

    v <- data.frame (
        package = names (checks$meta),
        version = checks$meta,
        row.names = NULL
    )
    md_out <- c (
        md_out,
        "",
        "---",
        "",
        "<details>",
        "<summary>Package Versions</summary>",
        "<p>",
        "",
        knitr::kable (v),
        "",
        "</p>",
        "</details>"
    )

    if (render) {
        render_markdown (md_out, open = TRUE)
    }

    attr (md_out, "checks_okay") <- attr (md_chks, "checks_okay")
    attr (md_out, "is_noteworthy") <- attr (stats_rep, "is_noteworthy")
    attr (md_out, "network_file") <- checks$info$network_file
    attr (md_out, "srr_report_file") <- checks$info$srr$report_file

    return (md_out)
}

#' Format \pkg{pkgstats} data
#' @param checks Result of main \link{pkgcheck} function
#' @param sec_num Section numbering to use (1 for non-srr packages; otherwise
#' 2).
#' @return Report as formatted string
#' @noRd
pkgstats_format <- function (checks, sec_num) {

    is_noteworthy <- any (checks$info$pkgstats$noteworthy == "TRUE")

    note <- ifelse (is_noteworthy,
        paste0 (
            "This package features some noteworthy ",
            "statistical properties which may need to be ",
            "clarified by a handling editor prior to ",
            "progressing."
        ),
        paste0 (
            "The statistical properties of this package are ",
            "all within expected ranges."
        )
    )

    stats_rep <- c (
        "",
        paste0 ("### ", sec_num, ". Statistical Properties"),
        "",
        note,
        "",
        "<details>",
        paste0 (
            "<summary>Details of statistical properties ",
            "(click to open)</summary>"
        ),
        "<p>",
        "",
        "The package has:",
        "",
        pkg_stat_desc (checks),
        "",
        "---",
        "",
        paste0 (
            "Statistical properties of package structure as ",
            "distributional percentiles in relation to all ",
            "current CRAN packages"
        ),
        "The following terminology is used:",
        "- `loc` = \"Lines of Code\"",
        "- `fn` = \"function\"",
        "- `exp`/`not_exp` = exported / not exported",
        "",
        paste0 (
            "The final measure (`fn_call_network_size`) is ",
            "the total number of calls between functions (in ",
            "R), or more abstract relationships between code ",
            "objects in other languages. Values are flagged ",
            "as \"noteworthy\" when they lie in the upper or ",
            "lower 5th percentile."
        ),
        "",
        knitr::kable (checks$info$pkgstats,
            row.names = FALSE,
            digits = c (NA, 0, 1, NA)
        ),
        "",
        "---",
        "",
        "</p></details>"
    )

    attr (stats_rep, "is_noteworthy") <- is_noteworthy

    return (stats_rep)
}

#' Initial description of structural properties of package
#' @param checks Result of main \link{pkgcheck} function
#' @noRd
pkg_stat_desc <- function (checks) {

    stats <- checks$info$pkgstats
    loc <- attr (stats, "language")
    files <- attr (stats, "files")

    loc_pc <- gsub (".*\\:\\s?", "", loc)
    langs <- gsub ("\\:.*$", "", loc)
    files <- gsub (".*\\:\\s?", "", files)

    langs <- paste0 (langs, " (", loc_pc, " in ", files, " files)")

    code <- paste0 ("- code in ", langs [1])
    langs <- langs [-1]
    langs_first <- ""
    langs_last <- langs [length (langs)]
    if (length (langs) > 1) {
        langs_first <- paste0 (
            ", ",
            paste0 (langs [-length (langs)],
                collapse = ", "
            )
        )
    }
    out <- paste0 (code, langs_first, " and ", langs_last)

    s <- checks$pkg$summary
    summarise_one <- function (s, what, pre_text, type) {
        ifelse (s [[what]] == 0L,
            paste0 ("- no ", pre_text, " ", type),
            paste0 (
                "- ", s [[what]], " ", pre_text, " ",
                ifelse (s [[what]] == 1L,
                    type,
                    paste0 (type, "s")
                )
            )
        )
    }

    out <- c (
        out,
        paste0 ("- ", s$num_authors, " authors"),
        summarise_one (s, "num_vignettes", "", "vignette"),
        summarise_one (s, "num_data", "internal", "data file"),
        summarise_one (s, "imported_pkgs", "imported", "package"),
        summarise_one (s, "num_exported_fns", "exported", "function")
    )

    if (length (s$loc_exported_fns) > 0L) {
        out [length (out)] <- paste0 (
            out [length (out)],
            " (median ",
            s$loc_exported_fns,
            " lines of code)"
        )
    }

    out <- c (
        out,
        summarise_one (
            s, "num_non_exported_fns",
            "non-exported",
            "function"
        )
    )
    out [length (out)] <- paste0 (out [length (out)], " in R")

    if (length (s$num_non_exported_fns) > 0L) {
        out [length (out)] <- paste0 (
            out [length (out)],
            " (median ",
            s$loc_non_exported_fns,
            " lines of code)"
        )
    }

    if (s$num_src_fns > 0L) {
        lang_names <- gsub ("\\s.*$", "", langs)
        out <- c (
            out,
            paste0 (
                summarise_one (
                    s,
                    "num_src_fns",
                    lang_names,
                    "function"
                ),
                " (median ",
                s$loc_src_fns,
                " lines of code)"
            )
        )
    }

    return (out)
}

#' Output text and URL link to function call network as 'vis.js' file.
#' @param checks Result of main \link{pkgcheck} function
#' @param sec_num Section numbering to use (1 for non-srr packages; otherwise
#' 2).
#' @noRd
pkg_network <- function (checks, sec_num) {

    out <- c (
        "",
        paste0 ("### ", sec_num, "a. Network visualisation"),
        ""
    )

    if (!"network_file" %in% names (checks$info)) {
        return (c (
            out,
            paste0 (
                "This package contains no internal function calls, ",
                "and therefore no function call network"
            )
        ))
    }

    cache_dir <- Sys.getenv ("PKGCHECK_CACHE_DIR")
    visjs_dir <- file.path (cache_dir, "static") # in api.R
    if (!dir.exists (visjs_dir)) {
        dir.create (visjs_dir, recursive = TRUE)
    }

    flist <- list.files (
        visjs_dir,
        pattern = paste0 (checks$pkg$name, "_pkgstats"),
        full.names = TRUE
    )

    if (!checks$info$network_file %in% flist) {

        unlink (flist, recursive = TRUE)

        if (!dir.exists (visjs_dir)) {
            dir.create (visjs_dir, recursive = TRUE)
        }

        visjs_ptn <- basename (checks$info$network_file)
        visjs_ptn <- tools::file_path_sans_ext (visjs_ptn)
        flist <- list.files (dirname (checks$info$network_file),
            pattern = visjs_ptn,
            full.names = TRUE,
            recursive = TRUE
        )

        file.copy (flist, visjs_dir, recursive = TRUE)
    }

    return (c (
        out,
        paste0 (
            "Click to see the [interactive network visualisation of calls ",
            "between objects in package](",
            network_file (checks),
            ")"
        )
    ))
}

network_file <- function (checks) {

    Sys.getenv ("PKGCHECK_TEST_NETWORK_FILE", checks$info$network_file)
}

#' render markdown-formatted input into 'html'
#'
#' @param md Result of \link{checks_to_markdown} function.
#' @param open If `TRUE`, open `hmtl`-rendered version in web browser.
#' @return (invisible) Location of `.html`-formatted version of input.
#' @family extra
#' @export
render_markdown <- function (md, open = TRUE) {

    md <- gsub ("\\:heavy\\_check\\_mark\\:", "&#9989;", md)
    md <- gsub ("\\:heavy\\_multiplication\\_x\\:", "&#10060;", md)

    md <- add_stats_tooltips (md)

    fmd <- tempfile (pattern = "pkgcheck", fileext = ".Rmd")
    writeLines (md, con = fmd)
    f <- tempfile (pattern = "pkgcheck", fileext = ".html")
    rmarkdown::render (fmd, output_file = f)

    if (open) {
        utils::browseURL (f)
    }

    invisible (f)
}

add_stats_tooltips <- function (md) {

    md <- c (
        "<script>",
        "$(document).ready(function(){", # nolint
        "    $('[tooltip=\"tooltip\"]').tooltip();", # nolint
        "});",
        "</script>",
        "",
        md,
        ""
    )

    stats_start <- grep ("\\|measure", md) + 1
    stats_end <- grep ("^\\-\\-\\-$", md)
    stats_end <- stats_end [which (stats_end > stats_start)] [1]

    index <- grep ("^\\|[[:alpha:]]", md [stats_start:stats_end])
    index <- stats_start + index - 1

    tips <- tooltip_dictionary ()

    for (i in index) {
        ptn <- regmatches (md [i], regexpr ("\\w+", md [i]))
        if (!ptn %in% tips$what) {
            next
        }

        tip <- tips$tooltip [tips$what == ptn]

        md [i] <- gsub (
            ptn,
            paste0 (
                "<a tooltip='tooltip' ",
                "title='", tip, "'>",
                ptn, "</a>"
            ),
            md [i]
        )
    }

    return (md)
}

tooltip_dictionary <- function () {

    out <- rbind (
        c (
            "files_R",
            "Number of files in R directory"
        ),
        c (
            "files_src",
            "Number of files in src directory"
        ),
        c (
            "files_inst",
            "Number of files in inst directory"
        ),
        c (
            "files_vignettes",
            "Number of files in vignettes directory"
        ),
        c (
            "files_tests",
            "Number of files in tests directory"
        ),
        c (
            "loc_R",
            paste0 (
                "Lines of code (excluding documentation and empty lines) ",
                "in R directory"
            )
        ),
        c (
            "loc_src",
            paste0 (
                "Lines of code (excluding documentation and empty lines) ",
                "in src directory"
            )
        ),
        c (
            "loc_inst",
            paste0 (
                "Lines of code (excluding documentation and empty lines) ",
                "in inst directory"
            )
        ),
        c (
            "loc_vignettes",
            paste0 (
                "Lines of code (excluding documentation and empty lines) ",
                "in vignettes directory"
            )
        ),
        c (
            "loc_tests",
            paste0 (
                "Lines of code (excluding documentation and empty lines) ",
                "in tests directory"
            )
        ),
        c (
            "num_vignettes",
            "Number of vignettes"
        ),
        c (
            "n_fns_r",
            "Total number of exported and non-exported R functions"
        ),
        c (
            "n_fns_r_exported",
            "Total number of exported R functions"
        ),
        c (
            "n_fns_r_not_exported",
            "Total number of non-exported R functions"
        ),
        c (
            "n_fns_src",
            "Total number of exported and non-exported src functions"
        ),
        c (
            "n_fns_per_file_r",
            "Median number of functions per R source file"
        ),
        c (
            "n_fns_per_file_src",
            "Median number of functions per src source file"
        ),
        c (
            "num_params_per_fn",
            "Median number of parameters per exported R function"
        ),
        c (
            "loc_per_fn_r",
            "Median number of lines of code for each R function"
        ),
        c (
            "loc_per_fn_src",
            "Median number of lines of code for each src function"
        ),
        c (
            "loc_per_fn_r_exp",
            "Median number of lines of code for each exported R function"
        ),
        c (
            "loc_per_fn_r_not_exp",
            "Median number of lines of code for each non-exported R function"
        ),
        c (
            "rel_whitespace_R",
            "Relative proportion of white characters within each R code line"
        ),
        c (
            "rel_whitespace_src",
            "Relative proportion of white characters within each src code line"
        ),
        c (
            "rel_whitespace_inst",
            "Relative proportion of white characters within each inst code line"
        ),
        c (
            "rel_whitespace_vignettes",
            "Relative proportion of white characters within each vignette line"
        ),
        c (
            "rel_whitespace_tests",
            "Relative proportion of white characters within each test line"
        ),
        c (
            "doclines_per_fn_exp",
            paste0 (
                "Median number of lines of documentation for ",
                "each exported R function"
            )
        ),
        c (
            "doclines_per_fn_not_exp",
            paste0 (
                "Median number of lines of documentation for ",
                "each non-exported R function"
            )
        ),
        c (
            "fn_call_network_size",
            "Total number of calls from one package function to another"
        )
    )

    data.frame (
        what = out [, 1],
        tooltip = out [, 2]
    )
}
