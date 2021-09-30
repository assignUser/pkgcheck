lead <- function(x, n = 1L, default = NA) {
  xlen <- length(x)
  n <- pmin(n, xlen)
  c(x[-seq_len(n)], rep(default, n))
}

moar_parentheses <- function (pd_flat)
{
  paren_after <- pd_flat$token %in% c("'('", "'['", "LBB")
  if (!any(paren_after)) {
    return(pd_flat)
  }
  paren_before <- lead(paren_after, default = FALSE)
  pd_flat$spaces[paren_before] <- 1L
  pd_flat
}

mark_style <- function() {
  styler::create_style_guide(
    space = list(moar_parentheses),
    style_guide_name = "moar_parentheses",
    style_guide_version = 1
  )
}

code <- "lala(1)"
styler::style_text(code, style = mark_style)
