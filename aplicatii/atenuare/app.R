# Atenuarea: ce face eroarea de măsurare din x cu panta regresiei y ~ x
# Aplicație Shinylive pentru ggluca.ro/aplicatii/atenuare (sursa: Measurement lessons/apps/atenuare).
# Terminologia după Sava (Analiza datelor în cercetarea psihologică): fidelitate, eroare de măsurare,
# mărimea eșantionului, panta/dreapta de regresie, atenuare.
# Stilul site-ului (Hokusai2, Atkinson Hyperlegible, fundal „hârtie”, urmează modul luminos/întunecat al paginii)
# și construcția aplicației prior-predictiv: shiny + bslib + grafică de bază, grafice trimise ca PNG cu
# renderImage(), pentru că renderPlot() blochează Shinylive în Safari (r-shinylive#204).

library(shiny)
library(bslib)

# ---- culori ----------------------------------------------------------------------------
# Hokusai2 ca pe site (hokusai-light.scss / hokusai-dark.scss), plus o culoare caldă pentru
# „ce găsește regresia”, ca să se distingă de dreapta reală.
palettes <- list(
  light = list(bg = "#fdfbf6", text = "#2b2f33", muted = "#5c6670", grid = "#e6e2d8",
               truth = "#0a3351", found = "#c2602a", fixed = "#2f70a1", points = "#72aeb6"),
  dark  = list(bg = "#161d24", text = "#e3ebf1", muted = "#a7b4bf", grid = "#2a3540",
               truth = "#cfe6ef", found = "#e8955f", fixed = "#8cc3e6", points = "#72aeb6")
)

plot_style <- function(pal) {
  par(mar = c(4.2, 4.4, 0.8, 0.8), bg = pal$bg, las = 1, bty = "n",
      col.axis = pal$muted, col.lab = pal$text, fg = pal$muted,
      cex.axis = 0.95, cex.lab = 1.05, mgp = c(2.8, 0.6, 0))
}
grid_lines <- function(pal) {
  abline(h = axTicks(2), v = axTicks(1), col = pal$grid, lwd = 1)
}

# Diagrama de dispersie: dreapta reală, dreapta estimată din eșantion și, la cerere, dreapta corectată
# (folosită și de _featured.R pentru imaginea din listă)
grafic_dispersie <- function(s, beta, p = palettes$light, rel_corectare = NULL) {
  plot_style(p)
  plot(NA, xlim = range(s$xobs), ylim = range(s$y), xlab = "x", ylab = "y")
  grid_lines(p)
  points(s$xobs, s$y, pch = 16, cex = 0.7, col = adjustcolor(p$points, 0.65))
  abline(0, beta, col = p$truth, lwd = 3)
  abline(s$a, s$b, col = p$found, lwd = 3, lty = 2)
  leg <- c("dreapta reală", "dreapta estimată din eșantion"); cols <- c(p$truth, p$found); lt <- c(1, 2)
  if (!is.null(rel_corectare)) {
    abline(s$a, s$b / rel_corectare, col = p$fixed, lwd = 3, lty = 4)
    leg <- c(leg, "dreapta corectată"); cols <- c(cols, p$fixed); lt <- c(lt, 4)
  }
  legend(if (beta >= 0) "topleft" else "topright", legend = leg, col = cols, lty = lt, lwd = 3, bty = "n",
         cex = 0.9, text.col = p$text, bg = p$bg)
}

# graficul de bază desenat într-un PNG de mărimea zonei de afișare, trimis cu renderImage()
png_render <- function(id, session, draw, alt) {
  renderImage({
    cd <- session$clientData
    w <- cd[[paste0("output_", id, "_width")]]
    h <- cd[[paste0("output_", id, "_height")]]
    if (is.null(w) || w <= 0) w <- 600
    if (is.null(h) || h <= 0) h <- 420
    pr <- cd$pixelratio %||% 1
    f <- tempfile(fileext = ".png")
    png(f, width = w * pr, height = h * pr, res = 96 * pr)
    op <- options(OutDec = ",")   # virgulă zecimală și pe axe
    draw()
    options(op)
    dev.off()
    list(src = f, contentType = "image/png", width = w, height = h, alt = alt)
  }, deleteFile = TRUE)
}

# numere în format românesc: virgulă zecimală, minus tipografic
fmt <- function(x, d = 2) sub("^-", "\u2212", formatC(x, format = "f", digits = d, decimal.mark = ","))

# ---- temă (ca în aplicația prior-predictiv) -------------------------------------------
theme <- bs_theme(
  version = 5, bg = palettes$light$bg, fg = palettes$light$text, primary = "#2f70a1",
  base_font = font_collection(
    font_link("Atkinson Hyperlegible",
              href = "https://fonts.googleapis.com/css2?family=Atkinson+Hyperlegible:wght@400;700&display=swap"),
    "sans-serif"
  )
)
theme <- bs_add_rules(theme, "
.summary { font-size: .95rem; line-height: 1.45; padding-bottom: .55rem; margin-bottom: .25rem;
  border-bottom: 1px solid rgba(10, 51, 81, .2); font-variant-numeric: tabular-nums; }
[data-bs-theme=dark] {
  --bs-body-bg: #161d24; --bs-body-bg-rgb: 22, 29, 36;
  --bs-body-color: #e3ebf1; --bs-body-color-rgb: 227, 235, 241;
  --bs-emphasis-color: #ffffff; --bs-emphasis-color-rgb: 255, 255, 255;
  --bs-secondary-color: rgba(227, 235, 241, .72); --bs-secondary-color-rgb: 227, 235, 241;
  --bs-secondary-bg: #222c36; --bs-tertiary-bg: #1c252e;
  --bs-border-color: #2e3a45;
  --bs-link-color: #8cc3e6; --bs-link-color-rgb: 140, 195, 230;
  --bs-link-hover-color: #72aeb6; --bs-link-hover-color-rgb: 114, 174, 182;
  --bs-primary: #8cc3e6; --bs-primary-rgb: 140, 195, 230;
  color-scheme: dark;
}
[data-bs-theme=dark] .summary { border-bottom-color: rgba(140, 195, 230, .25); }
[data-bs-theme=dark] .form-switch .form-check-input:not(:checked) {
  --bs-form-switch-bg: url(\"data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='-4 -4 8 8'%3e%3ccircle r='3' fill='rgba%28227,235,241,0.4%29'/%3e%3c/svg%3e\");
}
[data-bs-theme=dark] .btn-outline-primary {
  --bs-btn-color: #8cc3e6; --bs-btn-border-color: #8cc3e6;
  --bs-btn-hover-color: #161d24; --bs-btn-hover-bg: #8cc3e6; --bs-btn-hover-border-color: #8cc3e6;
  --bs-btn-active-color: #161d24; --bs-btn-active-bg: #72aeb6; --bs-btn-active-border-color: #72aeb6;
}
[data-bs-theme=dark] .irs.irs--shiny .irs-line { background: #2e3a45; border-color: #2e3a45; }
[data-bs-theme=dark] .irs.irs--shiny .irs-min, [data-bs-theme=dark] .irs.irs--shiny .irs-max {
  background: #222c36; color: #a7b4bf; }
[data-bs-theme=dark] .irs.irs--shiny .irs-grid-text { color: #a7b4bf; }
")

# Modul luminos/întunecat urmează pagina Quarto din jur (quarto-dark / quarto-light pe <body>),
# ca în aplicația prior-predictiv. Rulată separat, rămâne luminoasă.
script_mode <- tags$script(HTML("
(function() {
  function pageBody() {
    var w = window;
    try {
      while (w.parent && w.parent !== w) {
        w = w.parent;
        var b = w.document.body;
        if (b && (b.classList.contains('quarto-dark') || b.classList.contains('quarto-light'))) return b;
      }
    } catch (e) {}
    return null;
  }
  var body = pageBody();
  function dark() { return !!body && body.classList.contains('quarto-dark'); }
  function apply() {
    var d = dark();
    document.documentElement.setAttribute('data-bs-theme', d ? 'dark' : 'light');
    try { if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue('dark_mode', d); } catch (e) {}
  }
  document.documentElement.setAttribute('data-bs-theme', dark() ? 'dark' : 'light');
  $(document).on('shiny:connected', apply);
  if (body) new MutationObserver(apply).observe(body, { attributes: true, attributeFilter: ['class'] });
})();
"))

# Virgulă zecimală pe glisoare: ionRangeSlider afișează numerele prin „prettify”
script_virgula <- tags$script(HTML("
Shiny.addCustomMessageHandler('rezumat', function(h) { document.getElementById('rezumat').innerHTML = h; });
$(document).on('shiny:connected', function() {
  setTimeout(function() {
    $('.js-range-slider').each(function() {
      var s = $(this).data('ionRangeSlider');
      var alfa = ['rel', 'rel_assumed'].indexOf(this.id) >= 0;   // fidelitatea: fără zero în față, două zecimale
      if (s) s.update({ prettify: function(n) {
        if (alfa) return n < 1 ? n.toLocaleString('ro-RO', { minimumFractionDigits: 2 }).replace(/^0/, '') : '1';
        return n.toLocaleString('ro-RO', { maximumFractionDigits: 2 });
      } });
    });
  }, 0);
});
"))

# ---- UI ---------------------------------------------------------------------------------
main <- layout_sidebar(
  sidebar = sidebar(
    width = 290,
    sliderInput("beta", "Panta reală", min = -2, max = 2, value = 1, step = 0.1),
    sliderInput("rel", "Fidelitatea lui x (α)", min = 0.1, max = 1, value = 0.7, step = 0.05),
    sliderInput("n", "Mărimea eșantionului", min = 20, max = 1000, value = 200, step = 10),
    sliderInput("sdy", "Abaterea standard a erorii din y", min = 0, max = 2, value = 0, step = 0.1),
    input_switch("fix", "Corectează pentru atenuare", value = FALSE),
    conditionalPanel(
      "input.fix",
      sliderInput("rel_assumed", "Fidelitatea folosită la corectare (α)", min = 0.1, max = 1,
                  value = 0.7, step = 0.05)
    ),
    actionButton("again", "Eșantion nou", class = "btn-outline-primary")
  ),
  div(class = "summary", id = "rezumat"),
  card(full_screen = TRUE, card_header("Un eșantion"),
       imageOutput("scatter", height = "480px"))
)

ui <- page_fillable(
  theme = theme,
  lang = "ro",
  main,
  script_mode,
  script_virgula
)

# ---- server -----------------------------------------------------------------------------
server <- function(input, output, session) {

  pal <- reactive(if (isTRUE(input$dark_mode)) palettes$dark else palettes$light)

  # corectarea pornește de la fidelitatea reală; mutată apoi, arată o corectare greșită
  observeEvent(input$rel, updateSliderInput(session, "rel_assumed", value = input$rel))

  sample1 <- reactive({
    input$again
    n <- input$n; rel <- input$rel
    xtrue <- rnorm(n)
    xobs  <- xtrue + rnorm(n, 0, sqrt((1 - rel) / rel))   # var(xtrue) = 1
    y     <- input$beta * xtrue + rnorm(n, 0, input$sdy)
    fit   <- lm(y ~ xobs)
    ci    <- suppressWarnings(confint(fit))["xobs", ]   # potrivire perfectă la fidelitate 1 și fără eroare în y
    list(xtrue = xtrue, xobs = xobs, y = y, b = unname(coef(fit)[2]), lo = ci[[1]], hi = ci[[2]],
         a = unname(coef(fit)[1]))
  })

  output$scatter <- png_render("scatter", session, function() {
    grafic_dispersie(sample1(), input$beta, pal(), if (input$fix) input$rel_assumed)
  }, alt = "Diagramă de dispersie a lui y în funcție de valorile măsurate ale lui x, cu dreapta de regresie reală și cea estimată")

  # Rezumatul în notație APA (b și 95% CI [ ; ]), cu b cursiv. HTML-ul static din pagină e
  # actualizat printr-un mesaj, nu cu renderUI(): renderUI() cu HTML a blocat Safari în Shinylive.
  rezumat_html <- reactive({
    s <- sample1(); beta <- input$beta; rel <- input$rel
    fmt_a <- function(x) sub("^0,", ",", fmt(x))   # APA: fără zero în față pentru valori care nu depășesc 1
    ic <- function(lo, hi) paste0("95% CI [", fmt(lo), "; ", fmt(hi), "]")
    txt <- paste0("Pentru o pantă reală de ", fmt(beta), " și la o fidelitate de ", fmt_a(rel),
                  " a predictorului x, modelul estimează în acest eșantion o pantă de <em>b</em> = ", fmt(s$b), ", ",
                  ic(s$lo, s$hi), ".")
    if (input$fix) {
      ra <- input$rel_assumed
      txt <- paste0(txt, " Corectată pentru atenuare (α = ", fmt_a(ra), "), panta devine <em>b</em> = ",
                    fmt(s$b / ra), ", ", ic(s$lo / ra, s$hi / ra), ".")
    }
    txt
  })
  observe(session$sendCustomMessage("rezumat", rezumat_html()))

}

shinyApp(ui, server)
