# Generează images/favicon.png: prior, verosimilitate și posterior în culorile Hokusai2.
# Rulează din folderul Blog cu: Rscript _favicon.R
x <- seq(-5.5, 6.5, length.out = 400)
prior_sd <- 2; lik_mu <- 2.2; lik_sd <- 0.9
post_var <- 1 / (1 / prior_sd^2 + 1 / lik_sd^2)
post_mu  <- post_var * (lik_mu / lik_sd^2)
d <- list(dnorm(x, 0, prior_sd), dnorm(x, lik_mu, lik_sd), dnorm(x, post_mu, sqrt(post_var)))
col <- c("#72aeb6", "#2f70a1", "#0a3351")

png("images/favicon.png", width = 256, height = 256, bg = "transparent", type = "quartz")
par(mar = c(0, 0, 0, 0))
plot(NA, xlim = c(-4.5, 5.5), ylim = c(0, max(unlist(d)) * 1.15), axes = FALSE, xaxs = "i", yaxs = "i")
for (i in 1:3) {
  polygon(c(x, rev(x)), c(d[[i]], rep(0, length(x))), col = adjustcolor(col[i], .55), border = NA)
  lines(x, d[[i]], col = col[i], lwd = 14)
}
dev.off()
