# Generează _bayes-bg.html: prior, verosimilitate și posterior (model normal-normal),
# desenate mare și discret în fundalul homepage-ului.
# Rulează din folderul Blog cu: Rscript _bayes-bg.R

prior_mu <- 0;   prior_sd <- 2      # prior
lik_mu   <- 2.2; lik_sd   <- 0.9    # verosimilitatea (datele)

post_var <- 1 / (1 / prior_sd^2 + 1 / lik_sd^2)
post_mu  <- post_var * (prior_mu / prior_sd^2 + lik_mu / lik_sd^2)
post_sd  <- sqrt(post_var)

W <- 1200; H <- 360
x <- seq(-5.5, 6.5, length.out = 300)
dens <- list(
  prior      = dnorm(x, prior_mu, prior_sd),
  likelihood = dnorm(x, lik_mu, lik_sd),
  posterior  = dnorm(x, post_mu, post_sd)
)
ymax <- max(unlist(dens)) * 1.15
sx <- function(v) (v - min(x)) / diff(range(x)) * W
sy <- function(d) H - d / ymax * H

path <- function(d) {
  pts <- sprintf("%.1f,%.1f", sx(x), sy(d))
  sprintf("M%.1f,%d L%s L%.1f,%d Z", sx(min(x)), H, paste(pts, collapse = " "), sx(max(x)), H)
}

curve <- function(d) {
  paste0("M", paste(sprintf("%.1f,%.1f", sx(x), sy(d)), collapse = " L"))
}

svg <- c(
  '<div class="bayes-bg" aria-hidden="true">',
  sprintf('<svg viewBox="0 0 %d %d" preserveAspectRatio="xMidYMax meet" xmlns="http://www.w3.org/2000/svg">', W, H),
  # suprafața fără contur + doar curba conturată, ca să nu apară linia de bază
  sprintf('<path class="%s area" d="%s"/>', names(dens), vapply(dens, path, "")),
  sprintf('<path class="%s line" d="%s"/>', names(dens), vapply(dens, curve, "")),
  '</svg>',
  '</div>',
  '<script>',
  '  // mută graficul în coloana din stânga, sub linkuri',
  '  document.querySelector(".about-entity")?.appendChild(document.querySelector(".bayes-bg"));',
  '</script>'
)
writeLines(svg, "_bayes-bg.html")
