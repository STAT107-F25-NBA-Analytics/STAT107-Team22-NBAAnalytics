plot_corr <- function(df, x, y) {
  
  # Convert Col Names (String)
  x_sym <- rlang::sym(x)
  y_sym <- rlang::sym(y)
  
  # Making Col Names File-Nomenclature Friendly
  y_mod <- gsub(" ", "_", y_sym)
  x_mod <- gsub(" ", "_", x_sym)
  
  # Create Plot
  p <- ggplot(df, aes(!!x_sym, !!y_sym)) +
    geom_hex() +
    geom_smooth(method = "lm", se = FALSE, color = "orange", linewidth = 1) +
    scale_fill_continuous(name = "Count") +
    theme_minimal()
  
  print(p)
  print(x_sym)
  print(y_sym)
  print(x_mod)
  print(y_mod)
  
  # Building filename dynamically
  fname <- paste0("../img/plots/", x_mod, "_vs_", y_mod, ".png")
  
  # Saving the plot
  ggsave(fname, p, width = 6, height = 4, dpi = 600)
  
  # Calculate
  c <- cor(df[[x]], df[[y]], use = "complete.obs")
  
  return(c)
}