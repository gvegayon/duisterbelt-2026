# Draws fig/parts-of-ergm.svg: the ERGM equation in the notation of the
# conditional-ERGM paper (notation-paper.tex), with each part labelled.
#
# The equation is typeset by MathJax (SVG output, glyphs as paths, so the
# file needs no fonts) in a headless Chrome driven by chromote; the labels
# and leader lines are added here, positioned from the bounding boxes of the
# \cssId{...} groups. Needs internet access for the MathJax script.
#
# Run from the deck's root: Rscript scripts/parts-of-ergm.R

library(chromote)

col <- c(
  rest   = "#8a8f98",
  obs    = "#111111",
  par    = "#d97a00",
  stat   = "#0a9f00",
  norm   = "#5b2be0",
  supp   = "#b30000"
)

tex <- paste0(
  "\\textcolor{", col[["rest"]], "}{",
  "\\mathbb{P}_{\\textcolor{", col[["supp"]], "}{\\mathcal{Y}}}",
  "\\left\\{\\mathbf{Y} = ",
  "\\cssId{obs}{\\textcolor{", col[["obs"]], "}{\\mathbf{y}}};\\,",
  "\\boldsymbol{\\theta}\\right\\} = ",
  "\\frac{\\text{exp}\\left\\{",
  "\\cssId{par}{\\textcolor{", col[["par"]], "}{\\boldsymbol{\\theta}^{\\mathbf{t}}}}",
  "\\cssId{stat}{\\textcolor{", col[["stat"]], "}{s\\left(\\mathbf{y}\\right)}}",
  "\\right\\}}{",
  "\\cssId{norm}{\\textcolor{", col[["norm"]], "}{",
  "\\sum_{\\mathbf{y}'\\in\\mathcal{Y}}\\text{exp}\\left\\{",
  "\\boldsymbol{\\theta}^{\\mathbf{t}}s\\left(\\mathbf{y}'\\right)\\right\\}}}",
  "},\\quad\\forall\\,\\mathbf{y}\\in",
  "\\cssId{supp}{\\textcolor{", col[["supp"]], "}{\\mathcal{Y}}}",
  "}"
)

html <- paste0(
  "<!doctype html><html><head><meta charset='utf-8'>",
  "<script>window.MathJax = {loader: {load: ['[tex]/html', '[tex]/color']},",
  " tex: {packages: {'[+]': ['html', 'color']}},",
  " svg: {fontCache: 'none'}};</script>",
  "<script src='https://cdn.jsdelivr.net/npm/mathjax@3.2.2/es5/tex-svg.js'>",
  "</script></head><body style='margin:0'>",
  "<div id='eq' style='font-size:40px; display:inline-block'>\\[",
  tex,
  "\\]</div></body></html>"
)

page <- tempfile(fileext = ".html")
writeLines(html, page)

b <- ChromoteSession$new(width = 1400, height = 600)
b$Page$navigate(paste0("file://", page))
Sys.sleep(6)
ev <- function(x) b$Runtime$evaluate(x, returnByValue = TRUE)$result$value

# The equation's <svg>, and the boxes of the labelled parts relative to it.
info <- jsonlite::fromJSON(ev("
  (function() {
    var svg = document.querySelector('#eq svg');
    var r0 = svg.getBoundingClientRect();
    var out = {w: r0.width, h: r0.height, svg: svg.outerHTML, box: {}};
    ['obs', 'par', 'stat', 'norm', 'supp'].forEach(function(id) {
      var r = document.getElementById(id).getBoundingClientRect();
      out.box[id] = [r.left - r0.left, r.top - r0.top, r.width, r.height];
    });
    return JSON.stringify(out);
  })()
"))
b$close()

# Layout: the equation sits at (x0, y0) inside a larger canvas.
x0 <- 150
y0 <- 110
W  <- info$w + x0 + 70
H  <- info$h + y0 + 100

box <- lapply(info$box, function(v) {
  list(
    cx  = x0 + v[1] + v[3] / 2,
    top = y0 + v[2],
    bot = y0 + v[2] + v[4],
    l   = x0 + v[1],
    r   = x0 + v[1] + v[3]
  )
})

# The nested <svg> gets explicit pixel sizes instead of MathJax's ex units.
eq <- info$svg
eq <- sub("width=\"[^\"]*\"", sprintf("width=\"%.2f\"", info$w), eq)
eq <- sub("height=\"[^\"]*\"", sprintf("height=\"%.2f\"", info$h), eq)
eq <- sub("style=\"[^\"]*\"", "", eq)
eq <- sub("<svg ", sprintf("<svg x=\"%.2f\" y=\"%.2f\" ", x0, y0), eq)

label <- function(x, y, lines, colour, anchor = "middle") {
  spans <- paste0(
    sprintf(
      "<tspan x=\"%.1f\" dy=\"%s\">%s</tspan>",
      x,
      c("0", rep("1.2em", length(lines) - 1L)),
      lines
    ),
    collapse = ""
  )
  sprintf(
    "<text x=\"%.1f\" y=\"%.1f\" text-anchor=\"%s\" fill=\"%s\">%s</text>",
    x, y, anchor, colour, spans
  )
}

leader <- function(x1, y1, x2, y2, colour) {
  sprintf(
    paste0(
      "<line x1=\"%.1f\" y1=\"%.1f\" x2=\"%.1f\" y2=\"%.1f\" ",
      "stroke=\"%s\" stroke-width=\"2\" stroke-linecap=\"round\"/>"
    ),
    x1, y1, x2, y2, colour
  )
}

parts <- c(
  # Above the numerator: parameters to the left, statistics to the right.
  label(
    box$par$cx - 130, 34,
    c("A vector of", "model parameters"),
    col[["par"]]
  ),
  leader(box$par$cx - 90, 72, box$par$cx - 4, box$par$top - 6, col[["par"]]),
  label(
    box$stat$cx + 150, 34,
    c("A vector of", "sufficient statistics"),
    col[["stat"]]
  ),
  leader(box$stat$cx + 100, 72, box$stat$cx + 8, box$stat$top - 6, col[["stat"]]),
  # Below the denominator.
  label(
    box$norm$cx, box$norm$bot + 52,
    "The normalizing constant",
    col[["norm"]]
  ),
  # The observed network, bottom left.
  label(
    box$obs$cx - 60, box$obs$bot + 96,
    "The observed network",
    col[["obs"]]
  ),
  leader(
    box$obs$cx - 40, box$obs$bot + 68, box$obs$cx, box$obs$bot + 8,
    col[["obs"]]
  ),
  # The support, bottom right.
  label(
    box$supp$cx - 20, box$supp$bot + 60,
    c("All possible", "networks"),
    col[["supp"]]
  )
)

svg <- c(
  sprintf(
    paste0(
      "<svg xmlns=\"http://www.w3.org/2000/svg\" ",
      "xmlns:xlink=\"http://www.w3.org/1999/xlink\" ",
      "viewBox=\"0 0 %.0f %.0f\" width=\"%.0f\" height=\"%.0f\" ",
      "font-family=\"Helvetica, Arial, sans-serif\" font-size=\"26\">"
    ),
    W, H, W, H
  ),
  eq,
  parts,
  "</svg>"
)

writeLines(svg, "fig/parts-of-ergm.svg")
cat("fig/parts-of-ergm.svg:", round(W), "x", round(H), "\n")
