library(shiny)
library(bslib)

# Exemplul din McElreath (2020), Statistical Rethinking, cap. 4: înălțimea
# adulților !Kung (datele lui Nancy Howell) în funcție de greutate.
greutate_medie <- 45     # kg
greutate_interval <- c(31, 63)
limita_jos <- 0          # cm
limita_sus <- 272        # cm, Robert Wadlow
axa_y <- c(-100, 400)
greutate_sd <- 6.5       # kg, abaterea standard a greutății la adulții !Kung
n_extrageri <- 4000      # extrageri din distribuția a priori comună

# Hokusai2 (MetBrewer), ca pe ggluca.ro
culoare_linii <- "#2f70a1"
culoare_text <- "#2b2f33"
culoare_text_secundar <- "#5c6670"
culoare_grila <- "#e6e2d8"
culoare_imposibil <- "#ece8df"
fundal <- "#fdfbf6"

# Paletele graficelor: luminoasă (culorile de mai sus) și întunecată, după
# hokusai-dark.scss de pe site. Contrast față de fundal: liniile 9,0, textul
# 14,1, textul secundar 8,0 (minimul pentru text e 4,5).
palete <- list(
  luminos = list(linii = culoare_linii, text = culoare_text,
                 text_secundar = culoare_text_secundar, grila = culoare_grila,
                 imposibil = culoare_imposibil, fundal = fundal),
  intunecat = list(linii = "#8cc3e6", text = "#e3ebf1", text_secundar = "#a7b4bf",
                   grila = "#2a3540", imposibil = "#212b34", fundal = "#161d24")
)

familii <- c(
  "Normală" = "normal",
  "Student-t" = "student",
  "Log-normală" = "lognormal",
  "Exponențială" = "exponential",
  "Half-normal" = "halfnormal",
  "Half-Cauchy" = "halfcauchy",
  "Uniformă (0, max)" = "uniform"
)

parametri <- list(
  normal = c(m = "Medie", s = "Abatere standard"),
  student = c(df = "Grade de libertate", m = "Locație", s = "Scală"),
  lognormal = c(m = "μ (pe scala log)", s = "σ (pe scala log)"),
  exponential = c(rate = "Rată"),
  halfnormal = c(s = "Abatere standard"),
  halfcauchy = c(s = "Scală"),
  uniform = c(max = "Maxim")
)

# Parametrii care trebuie să fie strict pozitivi
pozitivi <- c("s", "df", "rate", "max")

extrage <- function(n, prior) {
  p <- prior$p
  switch(prior$familie,
    normal = rnorm(n, p$m, p$s),
    student = p$m + p$s * rt(n, p$df),
    lognormal = rlnorm(n, p$m, p$s),
    exponential = rexp(n, p$rate),
    halfnormal = abs(rnorm(n, 0, p$s)),
    halfcauchy = abs(rcauchy(n, 0, p$s)),
    uniform = runif(n, 0, p$max)
  )
}

prior_valid <- function(prior) {
  valori <- unlist(prior$p)
  if (length(valori) == 0 || anyNA(valori)) return(FALSE)
  all(valori[names(valori) %in% pozitivi] > 0)
}

format_ro <- function(x, cifre = 0) {
  formatC(x, format = "f", digits = cifre, decimal.mark = ",", big.mark = ".")
}

# Procent rotunjit; o valoare mică, dar nenulă, nu apare ca 0%.
procent <- function(x) {
  p <- 100 * mean(x)
  if (p > 0 && p < 0.5) return("<1%")
  if (p < 100 && p > 99.5) return(">99%")
  paste0(format_ro(p), "%")
}


# Modul: alegerea unui prior --------------------------------------------------

prior_ui <- function(id, permise, implicit) {
  ns <- NS(id)
  alegeri <- familii[familii %in% permise]
  campuri <- lapply(permise, function(f) {
    conditionalPanel(
      condition = sprintf("input.familie === '%s'", f),
      ns = ns,
      lapply(names(parametri[[f]]), function(k) {
        numericInput(
          ns(paste0(f, "_", k)),
          label = parametri[[f]][[k]],
          value = implicit[[f]][[k]],
          step = if (k == "df") 1 else 0.1
        )
      })
    )
  })
  tagList(
    selectInput(ns("familie"), "Distribuție", choices = alegeri,
                selected = names(implicit)[1]),
    campuri
  )
}

prior_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      f <- input$familie
      req(f)
      chei <- names(parametri[[f]])
      p <- lapply(chei, function(k) input[[paste0(f, "_", k)]])
      list(familie = f, p = setNames(p, chei))
    })
  })
}


# Grafic -----------------------------------------------------------------------

grafic_linii <- function(a, b, centrat, pal = palete$luminos) {
  centru <- if (centrat) greutate_medie else 0
  x <- greutate_interval
  y1 <- a + b * (x[1] - centru)
  y2 <- a + b * (x[2] - centru)

  stil_grafic(pal)
  plot(NA, xlim = x, ylim = axa_y, xaxs = "i", yaxs = "i", axes = FALSE,
       xlab = "Greutate (kg)", ylab = "Înălțime (cm)")
  u <- par("usr")
  rect(u[1], u[3], u[2], limita_jos, col = pal$imposibil, border = NA)
  rect(u[1], limita_sus, u[2], u[4], col = pal$imposibil, border = NA)
  abline(h = seq(-100, 400, by = 100), col = pal$grila, lwd = 1)
  abline(v = seq(35, 60, by = 5), col = pal$grila, lwd = 1)
  axis(1, at = seq(35, 60, by = 5), lwd = 0, lwd.ticks = 0)
  axis(2, at = seq(-100, 400, by = 100), lwd = 0, lwd.ticks = 0)

  segments(x[1], y1, x[2], y2, col = adjustcolor(pal$linii, alpha.f = 0.35),
           lwd = 1.5)

  abline(h = c(limita_jos, limita_sus), lty = 2, lwd = 1.2,
         col = pal$text_secundar)
  dx <- 0.012 * diff(u[1:2])
  dy <- 0.025 * diff(u[3:4])
  eticheta(u[1] + dx, limita_sus + dy, "272 cm: cel mai înalt om înregistrat (Robert Wadlow)",
           adj = c(0, 0), pal = pal)
  eticheta(u[1] + dx, limita_jos - dy, "0 cm", adj = c(0, 1), pal = pal)
  eticheta(u[2] - dx, mean(c(limita_sus, u[4])), "imposibil", adj = c(1, 0.5), pal = pal,
           culoare = pal$text_secundar)
  eticheta(u[2] - dx, mean(c(limita_jos, u[3])), "imposibil", adj = c(1, 0.5), pal = pal,
           culoare = pal$text_secundar)
}

# Etichetă cu fundal, ca liniile să nu acopere textul
eticheta <- function(x, y, text_eticheta, adj, pal, culoare = pal$text, cex = 0.9) {
  w <- strwidth(text_eticheta, cex = cex)
  h <- strheight(text_eticheta, cex = cex)
  x0 <- x - adj[1] * w
  y0 <- y - adj[2] * h
  px <- 0.35 * strwidth("m", cex = cex)
  py <- 0.45 * h
  rect(x0 - px, y0 - py, x0 + w + px, y0 + h + py,
       col = adjustcolor(pal$imposibil, alpha.f = 0.9), border = NA)
  text(x, y, text_eticheta, adj = adj, cex = cex, col = culoare)
}

stil_grafic <- function(pal) {
  par(mar = c(4.2, 4.6, 0.8, 0.8), bg = pal$fundal, las = 1,
      col.axis = pal$text_secundar, col.lab = pal$text,
      cex.axis = 0.95, cex.lab = 1.05, mgp = c(3, 0.6, 0))
}

# Histograma înălțimilor simulate. Axa cuprinde mereu intervalul -100–400 cm,
# extinsă până la cuantilele de 0,5% și 99,5%; valorile mai extreme nu apar.
grafic_inaltimi <- function(y, pal = palete$luminos) {
  lim <- range(c(axa_y, quantile(y, c(0.005, 0.995))))
  rupturi <- pretty(lim, n = 60)
  lim <- range(rupturi)
  h <- hist(y[y >= lim[1] & y <= lim[2]], breaks = rupturi, plot = FALSE)
  y_max <- max(h$counts) * 1.15

  stil_grafic(pal)
  plot(NA, xlim = lim, ylim = c(0, y_max), xaxs = "i", yaxs = "i", axes = FALSE,
       xlab = "Înălțime simulată (cm)", ylab = "Număr de înălțimi")
  u <- par("usr")
  rect(u[1], u[3], limita_jos, u[4], col = pal$imposibil, border = NA)
  rect(limita_sus, u[3], u[2], u[4], col = pal$imposibil, border = NA)
  repere_y <- pretty(c(0, y_max))
  repere_x <- pretty(lim)
  abline(h = repere_y, col = pal$grila, lwd = 1)
  axis(1, at = repere_x, labels = format_ro(repere_x), lwd = 0, lwd.ticks = 0)
  axis(2, at = repere_y, labels = format_ro(repere_y), lwd = 0, lwd.ticks = 0)

  n_bare <- length(h$counts)
  rect(h$breaks[-(n_bare + 1)], 0, h$breaks[-1], h$counts,
       col = pal$linii, border = pal$fundal, lwd = 1)

  abline(v = c(limita_jos, limita_sus), lty = 2, lwd = 1.2,
         col = pal$text_secundar)
  dx <- 0.01 * diff(u[1:2])
  dy <- 0.04 * diff(u[3:4])
  eticheta(limita_jos - dx, u[4] - dy, "0 cm", adj = c(1, 1), pal = pal)
  eticheta(limita_sus + dx, u[4] - dy, "272 cm", adj = c(0, 1), pal = pal)
  if (limita_jos - u[1] > 0.12 * diff(u[1:2])) {
    eticheta(mean(c(u[1], limita_jos)), u[4] / 2, "imposibil", adj = c(0.5, 0.5), pal = pal,
             culoare = pal$text_secundar)
  }
  if (u[2] - limita_sus > 0.12 * diff(u[1:2])) {
    eticheta(mean(c(limita_sus, u[2])), u[4] / 2, "imposibil", adj = c(0.5, 0.5), pal = pal,
             culoare = pal$text_secundar)
  }
}


# Cod brms ------------------------------------------------------------------------

numar_cod <- function(x) format(x, scientific = FALSE, trim = TRUE, drop0trailing = TRUE)

distributie_stan <- function(prior) {
  p <- lapply(prior$p, numar_cod)
  switch(prior$familie,
    normal = sprintf("normal(%s, %s)", p$m, p$s),
    student = sprintf("student_t(%s, %s, %s)", p$df, p$m, p$s),
    lognormal = sprintf("lognormal(%s, %s)", p$m, p$s),
    exponential = sprintf("exponential(%s)", p$rate),
    halfnormal = sprintf("normal(0, %s)", p$s),
    halfcauchy = sprintf("cauchy(0, %s)", p$s),
    uniform = sprintf("uniform(0, %s)", p$max)
  )
}

# Verificat cu brms 2.23 (make_stancode): centrat, brms centrează singur
# predictorul, iar class = Intercept este α din aplicație; necentrat,
# interceptul devine coeficient (0 + Intercept). brms nu acceptă limite pe un
# singur coeficient, deci lb = 0 pentru β log-normal se aplică întregii clase b.
cod_brms <- function(pa, pb, ps, centrat) {
  alfa <- distributie_stan(pa)
  beta <- distributie_stan(pb)
  sigma <- distributie_stan(ps)
  sigma_linie <- if (ps$familie == "uniform") {
    sprintf("  prior(%s, class = sigma, ub = %s)", sigma, numar_cod(ps$p$max))
  } else {
    sprintf("  prior(%s, class = sigma)", sigma)
  }
  lognormal <- pb$familie == "lognormal"

  if (centrat) {
    linii_prior <- c(
      sprintf("  prior(%s, class = Intercept),", alfa),
      sprintf("  prior(%s, class = b%s),", beta, if (lognormal) ", lb = 0" else ""),
      sigma_linie
    )
    nota <- "# brms centrează intern greutatea pe medie (44,99 kg), deci Intercept = α din aplicație"
    formula <- "height ~ weight"
  } else {
    linii_prior <- if (lognormal) {
      c("  # brms nu permite limite pe un singur coeficient: lb = 0 se aplică și interceptului",
        sprintf("  prior(%s, class = b, lb = 0),", beta),
        sprintf("  prior(%s, class = b, coef = Intercept),", alfa))
    } else {
      c(sprintf("  prior(%s, class = b, coef = Intercept),", alfa),
        sprintf("  prior(%s, class = b, coef = weight),", beta))
    }
    linii_prior <- c(linii_prior, sigma_linie)
    nota <- "# Greutate necentrată: interceptul (înălțimea la 0 kg) devine un coeficient obișnuit"
    formula <- "bf(height ~ 0 + Intercept + weight)"
  }

  paste(c(
    "library(brms)",
    "",
    "# Datele lui Nancy Howell (adulții !Kung), de pe GitHub-ul lui McElreath",
    'd <- read.csv("https://raw.githubusercontent.com/rmcelreath/rethinking/master/data/Howell1.csv", sep = ";")',
    "d <- d[d$age >= 18, ]",
    "",
    nota,
    "priori <- c(",
    linii_prior,
    ")",
    "",
    '# sample_prior = "only": înălțimile observate nu sunt folosite, doar greutățile',
    sprintf('fit <- brm(%s, data = d, prior = priori, sample_prior = "only")', formula),
    "",
    "pp_check(fit, ndraws = 100)                                    # înălțimi simulate",
    "plot(conditional_effects(fit, spaghetti = TRUE, ndraws = 100))  # drepte"
  ), collapse = "\n")
}


# Evidențierea liniilor schimbate din codul brms. Rulează doar în browser:
# compară codul nou cu cel anterior (diferență pe linii, ca în git) și
# colorează liniile noi sau modificate. R nu face nimic în plus.
script_evidentiere <- tags$script(HTML("
(function() {
  var vechi = null;
  function escape(t) {
    return t.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
  }
  function schimbate(a, b) {
    var n = a.length, m = b.length, L = [], i, j;
    for (i = 0; i <= n; i++) { L.push(new Array(m + 1).fill(0)); }
    for (i = n - 1; i >= 0; i--) {
      for (j = m - 1; j >= 0; j--) {
        L[i][j] = a[i] === b[j] ? L[i + 1][j + 1] + 1 : Math.max(L[i + 1][j], L[i][j + 1]);
      }
    }
    var rez = new Array(m).fill(true);
    i = 0; j = 0;
    while (i < n && j < m) {
      if (a[i] === b[j]) { rez[j] = false; i++; j++; }
      else if (L[i + 1][j] >= L[i][j + 1]) { i++; }
      else { j++; }
    }
    return rez;
  }
  $(document).on('shiny:value', function(ev) {
    if (ev.name !== 'cod_brms') return;
    var nou = String(ev.value), anterior = vechi;
    vechi = nou;
    if (anterior === null || anterior === nou) return;
    var linii = nou.split('\\n'), marcate = schimbate(anterior.split('\\n'), linii);
    setTimeout(function() {
      var el = document.getElementById('cod_brms');
      if (!el || el.textContent !== nou) return;
      el.innerHTML = linii.map(function(l, k) {
        return '<span class=\"cod-linie' + (marcate[k] ? ' cod-schimbat' : '') + '\">' +
          (escape(l) || ' ') + '</span>';
      }).join('');
    }, 0);
  });
})();
"))


# Modul luminos/întunecat urmează pagina din jur: Quarto pune quarto-dark sau
# quarto-light pe <body>. Aplicația rulează într-un iframe de pe același site,
# deci scriptul urcă la pagina-părinte, îi citește clasa și o urmărește.
script_mod <- tags$script(HTML("
(function() {
  function corpPagina() {
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
  var corp = corpPagina();
  function intunecat() { return !!corp && corp.classList.contains('quarto-dark'); }
  function aplica() {
    var d = intunecat();
    document.documentElement.setAttribute('data-bs-theme', d ? 'dark' : 'light');
    try { if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue('mod_intunecat', d); } catch (e) {}
  }
  document.documentElement.setAttribute('data-bs-theme', intunecat() ? 'dark' : 'light');
  $(document).on('shiny:connected', aplica);
  if (corp) new MutationObserver(aplica).observe(corp, { attributes: true, attributeFilter: ['class'] });
})();
"))


# Grafic ca imagine PNG trimisă cu renderImage(). renderPlot() depășește
# stiva de apeluri a webR în Safari (Shinylive 0.10.12, R 4.6), iar aplicația
# se blochează: https://github.com/posit-dev/r-shinylive/issues/204
grafic_png <- function(id, session, deseneaza, alt) {
  renderImage({
    cd <- session$clientData
    w <- cd[[paste0("output_", id, "_width")]]
    h <- cd[[paste0("output_", id, "_height")]]
    req(w > 0, h > 0)
    pr <- cd$pixelratio %||% 1
    f <- tempfile(fileext = ".png")
    png(f, width = w * pr, height = h * pr, res = 96 * pr)
    deseneaza()
    dev.off()
    list(src = f, contentType = "image/png", width = w, height = h, alt = alt())
  }, deleteFile = TRUE)
}


# Interfață --------------------------------------------------------------------

tema <- bs_theme(
  version = 5,
  bg = fundal,
  fg = culoare_text,
  primary = culoare_linii,
  base_font = font_collection(
    font_link(
      "Atkinson Hyperlegible",
      href = "https://fonts.googleapis.com/css2?family=Atkinson+Hyperlegible:wght@400;700&display=swap"
    ),
    "sans-serif"
  )
)

# Rezumatul de deasupra fiecărui grafic: text liniștit, cifra în bold
tema <- bs_add_rules(tema, "
.rezumat-tab {
  font-size: .95rem; line-height: 1.45;
  padding-bottom: .55rem; margin-bottom: .25rem;
  border-bottom: 1px solid rgba(10, 51, 81, .2);
}
.rezumat-tab p { margin: 0; }
.rezumat-tab strong { font-variant-numeric: tabular-nums; color: #0a3351; }
#linii, #inaltimi { min-height: 300px; }
")

# Liniile schimbate din codul brms: fundal albastru care dispare treptat
tema <- bs_add_rules(tema, "
#cod_brms .cod-linie { display: block; margin: 0 -.5rem; padding: 0 .5rem; }
#cod_brms .cod-schimbat { animation: cod-schimbat 2.4s ease-out forwards; }
@keyframes cod-schimbat {
  0%, 35% { background-color: rgba(47, 112, 161, .22); }
  100% { background-color: rgba(47, 112, 161, 0); }
}
@media (prefers-reduced-motion: reduce) {
  #cod_brms .cod-schimbat { animation: none; background-color: rgba(47, 112, 161, .14); }
}
")

# Modul întunecat (data-bs-theme="dark", pus de script_mod): culorile din
# hokusai-dark.scss de pe site
tema <- bs_add_rules(tema, "
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
}
[data-bs-theme=dark] { color-scheme: dark; }
[data-bs-theme=dark] .accordion {
  --bs-accordion-active-color: #8cc3e6;
  --bs-accordion-active-bg: #1c252e;
  --bs-accordion-btn-icon: url(\"data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 16 16' fill='none' stroke='%23e3ebf1' stroke-linecap='round' stroke-linejoin='round'%3e%3cpath d='m2 5 6 6 6-6'/%3e%3c/svg%3e\");
  --bs-accordion-btn-active-icon: url(\"data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 16 16' fill='none' stroke='%238cc3e6' stroke-linecap='round' stroke-linejoin='round'%3e%3cpath d='m2 5 6 6 6-6'/%3e%3c/svg%3e\");
}
[data-bs-theme=dark] .form-switch .form-check-input:not(:checked) {
  --bs-form-switch-bg: url(\"data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='-4 -4 8 8'%3e%3ccircle r='3' fill='rgba%28227,235,241,0.4%29'/%3e%3c/svg%3e\");
}
[data-bs-theme=dark] .rezumat-tab { border-bottom-color: rgba(140, 195, 230, .25); }
[data-bs-theme=dark] .rezumat-tab strong { color: #8cc3e6; }
[data-bs-theme=dark] .btn-outline-primary {
  --bs-btn-color: #8cc3e6; --bs-btn-border-color: #8cc3e6;
  --bs-btn-hover-color: #161d24; --bs-btn-hover-bg: #8cc3e6; --bs-btn-hover-border-color: #8cc3e6;
  --bs-btn-active-color: #161d24; --bs-btn-active-bg: #72aeb6; --bs-btn-active-border-color: #72aeb6;
}
[data-bs-theme=dark] .selectize-input, [data-bs-theme=dark] .selectize-input.full,
[data-bs-theme=dark] .selectize-dropdown {
  background: #1c252e; color: #e3ebf1; border-color: #2e3a45;
}
[data-bs-theme=dark] .selectize-dropdown .option.active { background: #2f70a1; color: #ffffff; }
[data-bs-theme=dark] .irs.irs--shiny .irs-line { background: #2e3a45; border-color: #2e3a45; }
[data-bs-theme=dark] .irs.irs--shiny .irs-min, [data-bs-theme=dark] .irs.irs--shiny .irs-max {
  background: #222c36; color: #a7b4bf;
}
[data-bs-theme=dark] .irs.irs--shiny .irs-grid-text { color: #a7b4bf; }
[data-bs-theme=dark] #cod_brms { background: #1c252e; color: #e3ebf1; border-color: #2e3a45; }
[data-bs-theme=dark] #cod_brms .cod-schimbat { animation-name: cod-schimbat-intunecat; }
@keyframes cod-schimbat-intunecat {
  0%, 35% { background-color: rgba(140, 195, 230, .22); }
  100% { background-color: rgba(140, 195, 230, 0); }
}
@media (prefers-reduced-motion: reduce) {
  [data-bs-theme=dark] #cod_brms .cod-schimbat { animation: none; background-color: rgba(140, 195, 230, .14); }
}
")

ui <- page_sidebar(
  theme = tema,
  lang = "ro",
  sidebar = sidebar(
    width = 300,
    accordion(
      open = c("alfa", "beta"),
      accordion_panel(
        "α · interceptul", value = "alfa",
        uiOutput("sens_alfa", class = "small text-muted mb-2"),
        prior_ui("alfa", c("normal", "student"),
                 list(normal = list(m = 178, s = 20),
                      student = list(df = 3, m = 178, s = 20)))
      ),
      accordion_panel(
        "β · panta", value = "beta",
        p(class = "small text-muted mb-2",
          "Cu câți centimetri crește înălțimea medie la fiecare kilogram în plus."),
        prior_ui("beta", c("normal", "student", "lognormal"),
                 list(normal = list(m = 0, s = 10),
                      student = list(df = 3, m = 0, s = 10),
                      lognormal = list(m = 0, s = 1)))
      ),
      accordion_panel(
        "σ · abaterea standard", value = "sigma",
        p(class = "small text-muted mb-2",
          "Cât de împrăștiate sunt înălțimile în jurul dreptei. Nu schimbă dreptele."),
        prior_ui("sigma", c("uniform", "exponential", "halfnormal", "halfcauchy"),
                 list(uniform = list(max = 50),
                      exponential = list(rate = 0.1),
                      halfnormal = list(s = 10),
                      halfcauchy = list(s = 2.5)))
      )
    ),
    input_switch("centrat", "Greutate centrată (greutate − 45 kg)", value = TRUE),
    sliderInput("n", "Număr de drepte", min = 20, max = 300, value = 100, step = 10),
    actionButton("din_nou", "Extrage din nou", class = "btn-outline-primary")
  ),
  navset_card_underline(
    full_screen = TRUE,
    title = uiOutput("model"),
    nav_panel(
      "Drepte",
      div(class = "rezumat-tab",
        p(tags$strong(textOutput("pct_negative", inline = TRUE)),
          " dintre drepte spun că oamenii mai grei sunt mai scunzi."),
        p(tags$strong(textOutput("pct_afara", inline = TRUE)),
          " dintre drepte trec sub 0\u00a0cm sau peste 272\u00a0cm, deci sunt imposibile.")
      ),
      imageOutput("linii", height = "100%", fill = TRUE),
      p(class = "small text-muted mb-0",
        "Fiecare dreaptă corespunde unei extrageri din distribuția a priori ",
        "comună a parametrilor, înainte de orice observare.")
    ),
    nav_panel(
      "Înălțimi simulate",
      div(class = "rezumat-tab",
        p(tags$strong(textOutput("pct_inaltimi", inline = TRUE)),
          " dintre înălțimile simulate sunt sub 0\u00a0cm sau peste 272\u00a0cm, ",
          "deci sunt imposibile.")
      ),
      imageOutput("inaltimi", height = "100%", fill = TRUE),
      p(class = "small text-muted mb-0",
        "Distribuția predictivă a priori: pentru fiecare dintre cele 4.000 de ",
        "extrageri ale lui α, β și σ se alege o greutate (media 45 kg, abaterea ",
        "standard 6,5 kg) și apoi o înălțime din Normal(μ, σ). Cele mai extreme ",
        "valori (cel mult 1%) nu apar în grafic.")
    ),
    nav_panel(
      "Cod brms",
      verbatimTextOutput("cod_brms"),
      script_evidentiere,
      p(class = "small text-muted mb-0",
        "Aceleași distribuții a priori, ca model brms. Cu sample_prior = \"only\", ",
        "Stan extrage doar din distribuția a priori, iar pp_check() și ",
        "conditional_effects() reproduc cele două grafice ale aplicației.")
    ),
    footer = card_footer(
      class = "small text-muted",
      "Exemplu după McElreath (2020), ",
      tags$em("Statistical Rethinking"), ", cap. 4; greutățile (31–63 kg, ",
      "media 45 kg) sunt cele ale adulților !Kung din datele lui Nancy Howell."
    )
  ),
  script_mod
)


# Server -----------------------------------------------------------------------

server <- function(input, output, session) {
  prior_alfa <- prior_server("alfa")
  prior_beta <- prior_server("beta")
  prior_sigma <- prior_server("sigma")

  samanta <- reactiveVal(2026)
  observeEvent(input$din_nou, samanta(samanta() + 1))

  extrageri <- reactive({
    pa <- prior_alfa()
    pb <- prior_beta()
    ps <- prior_sigma()
    validate(
      need(prior_valid(pa), "Parametrii lui α trebuie completați (scala și gradele de libertate pozitive)."),
      need(prior_valid(pb), "Parametrii lui β trebuie completați (scala și gradele de libertate pozitive)."),
      need(prior_valid(ps), "Parametrii lui σ trebuie completați și pozitivi.")
    )
    # Câte o sămânță pe componentă: schimbarea unui prior nu le schimbă pe celelalte.
    s <- 10 * samanta()
    n <- n_extrageri
    list(
      a = cu_samanta(s + 1, extrage(n, pa)),
      b = cu_samanta(s + 2, extrage(n, pb)),
      sigma = cu_samanta(s + 3, extrage(n, ps)),
      greutate = cu_samanta(s + 4, rnorm(n, greutate_medie, greutate_sd)),
      z = cu_samanta(s + 5, rnorm(n))
    )
  })

  centru <- reactive(if (isTRUE(input$centrat)) greutate_medie else 0)

  inaltimi <- reactive({
    e <- extrageri()
    e$a + e$b * (e$greutate - centru()) + e$sigma * e$z
  })

  output$sens_alfa <- renderUI({
    if (isTRUE(input$centrat)) {
      "Înălțimea medie a unei persoane cu greutatea medie (45 kg)."
    } else {
      "Înălțimea medie a unei persoane cu greutatea 0 kg."
    }
  })

  output$model <- renderUI({
    x <- if (isTRUE(input$centrat)) "(greutate − 45)" else "greutate"
    tags$span(
      class = "font-monospace small",
      "înălțime ~ Normal(μ, σ),  μ = α + β · ", x
    )
  })

  linii_y <- reactive({
    e <- extrageri()
    cbind(e$a + e$b * (greutate_interval[1] - centru()),
          e$a + e$b * (greutate_interval[2] - centru()))
  })

  imposibil <- function(y) y < limita_jos | y > limita_sus


  output$pct_negative <- renderText(procent(extrageri()$b < 0))

  output$pct_afara <- renderText({
    y <- linii_y()
    procent(pmin(y[, 1], y[, 2]) < limita_jos | pmax(y[, 1], y[, 2]) > limita_sus)
  })

  output$pct_inaltimi <- renderText(procent(imposibil(inaltimi())))

  output$cod_brms <- renderText({
    pa <- prior_alfa(); pb <- prior_beta(); ps <- prior_sigma()
    validate(need(prior_valid(pa) && prior_valid(pb) && prior_valid(ps),
                  "Parametrii distribuțiilor a priori trebuie completați."))
    cod_brms(pa, pb, ps, isTRUE(input$centrat))
  })

  # Modul paginii (luminos/întunecat), trimis de script_mod din browser
  paleta <- reactive(if (isTRUE(input$mod_intunecat)) palete$intunecat else palete$luminos)

  output$linii <- grafic_png(
    "linii", session,
    deseneaza = function() {
      e <- extrageri()
      arata <- seq_len(input$n)
      grafic_linii(e$a[arata], e$b[arata], isTRUE(input$centrat), paleta())
    },
    alt = function() {
      y <- linii_y()
      afara <- pmin(y[, 1], y[, 2]) < limita_jos | pmax(y[, 1], y[, 2]) > limita_sus
      sprintf(
        "%d drepte de regresie extrase din distribuția a priori comună a parametrilor, pentru greutăți între 31 și 63 kg. Din toate cele 4.000 de extrageri, %s au pantă negativă și %s ies din intervalul posibil de 0–272 cm.",
        input$n, procent(extrageri()$b < 0), procent(afara)
      )
    }
  )

  output$inaltimi <- grafic_png(
    "inaltimi", session,
    deseneaza = function() grafic_inaltimi(inaltimi(), paleta()),
    alt = function() {
      y <- inaltimi()
      q <- quantile(y, c(0.05, 0.5, 0.95))
      sprintf(
        "Histograma a 4.000 de înălțimi simulate din distribuția predictivă a priori. Mediana este %s cm, 90%% dintre valori sunt între %s și %s cm, iar %s sunt imposibile (sub 0 sau peste 272 cm).",
        format_ro(q[2]), format_ro(q[1]), format_ro(q[3]), procent(imposibil(y))
      )
    }
  )
}

# Extrage cu o sămânță fixă fără să schimbe starea generatorului din sesiune.
cu_samanta <- function(seed, expr) {
  vechi <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv())
  on.exit(if (is.null(vechi)) rm(".Random.seed", envir = globalenv()) else assign(".Random.seed", vechi, envir = globalenv()))
  set.seed(seed)
  expr
}

shinyApp(ui, server)
