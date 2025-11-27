plot_corr <- function(df, x, y) {
  
  # Convert Col Names (String)
  x_sym <- rlang::sym(x)
  y_sym <- rlang::sym(y)
  
  # Create Plot
  p <- ggplot(df, aes(!!x_sym, !!y_sym)) +
    geom_hex() +
    geom_smooth(method = "lm", se = FALSE, color = "orange", linewidth = 1) +
    theme_minimal()
  
  print(p)
  
  # Building filename dynamically
  fname <- paste0("../img/plots/", x, "_vs_", y, ".png")
  
  # Saving the plot
  ggsave(fname, p, width = 6, height = 4, dpi = 300)
  
  # Calculate
  c <- cor(df[[x]], df[[y]], use = "complete.obs")
  
  return(c)
}