# Imaginea pentru cardul din listă (featured.jpg), cu graficul din aplicație.
# Rulare din rădăcina proiectului: Rscript aplicatii/prior-predictiv/_featured.R
dir <- "aplicatii/prior-predictiv"
env <- new.env()
suppressMessages(sys.source(file.path(dir, "app.R"), env, chdir = TRUE))
set.seed(2026)
jpeg(file.path(dir, "featured.jpg"), width = 1200, height = 675, res = 144, quality = 85)
env$grafic_linii(rnorm(100, 178, 20), rnorm(100, 0, 10), centrat = TRUE)
dev.off()
