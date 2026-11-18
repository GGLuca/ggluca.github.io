# Imaginea pentru cardul din listă (featured.jpg), cu graficul din aplicație.
# Rulare din rădăcina proiectului: Rscript aplicatii/atenuare/_featured.R
dir <- "aplicatii/atenuare"
env <- new.env()
suppressMessages(sys.source(file.path(dir, "app.R"), env, chdir = TRUE))
set.seed(2026)
rel <- 0.6; n <- 200
xtrue <- rnorm(n)
xobs  <- xtrue + rnorm(n, 0, sqrt((1 - rel) / rel))
y     <- xtrue
fit   <- lm(y ~ xobs)
s <- list(xobs = xobs, y = y, a = unname(coef(fit)[1]), b = unname(coef(fit)[2]))
jpeg(file.path(dir, "featured.jpg"), width = 1200, height = 675, res = 144, quality = 85)
env$grafic_dispersie(s, beta = 1)
dev.off()
