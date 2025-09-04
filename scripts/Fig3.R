library(ggplot2)
library(ggalluvial)
library(patchwork)
library(Cairo) # For graphics device

if (file.exists("/.dockerenv")) { # Check if running in Docker
  # Assume igmm/cvallejo-predicct/libdr/ is passed to the data volume
  prefix <- "data/"
} else {
  # Assume running outside of a Docker container and the IGC(/IGMM) datastore is
  # mounted at /Volumes
  prefix <- "/Volumes/igmm/cvallejo-predicct/libdr/"
}

p1 <- readRDS(paste0(prefix, "processed/plots/fc-alluvial.RDS"))
p2 <- readRDS(paste0(prefix, "processed/plots/crp-alluvial.RDS"))

p <- p1 / p2  +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 13))

# Use Cairo for safe PDF output
tryCatch({
  Cairo::CairoPDF("paper/Figure-S7.pdf", width = 12, height = 12)
  print(p)
  dev.off()
  cat("PDF saved successfully with Cairo\n")
}, error = function(e) {
  if (!is.null(dev.list())) dev.off()
  cat("Cairo PDF approach failed:", e$message, "\n")
})

# Use Cairo for safe PNG output
tryCatch({
  Cairo::CairoPNG("paper/Figure-S7.png", 
                  width = 12 * 300, 
                  height = 12 * 300, 
                  dpi = 300)
  print(p)
  dev.off()
  cat("PNG saved successfully with Cairo\n")
}, error = function(e) {
  if (!is.null(dev.list())) dev.off()
  cat("Cairo PNG approach failed:", e$message, "\n")
  # Fallback to basic png device
  tryCatch({
    png("paper/Figure-S7.png", 
        width = 12 * 300, 
        height = 12 * 300, 
        res = 300)
    print(p)
    dev.off()
    cat("PNG saved successfully with basic png device\n")
  }, error = function(e2) {
    if (!is.null(dev.list())) dev.off()
    cat("Both Cairo and PNG approaches failed:", e2$message, "\n")
  })
})
