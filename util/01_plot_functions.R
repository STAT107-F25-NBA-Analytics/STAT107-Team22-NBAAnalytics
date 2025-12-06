plot_corr <- function(df, x, y) {
  
  # Convert Col Names (String)
  x_sym <- sym(x)
  y_sym <- sym(y)
  
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

timeseriesgraph <- function(y) {
  # Producing format compatible for operations on metric
  y_sym <- sym(y)
  
  y_mod <- gsub(" ", "_", y_sym)
  
  p <- ggplot(nba_processed %>%
           group_by(`Season`) %>%
           summarize(mean_seasonal = mean(!!y_sym)),
         aes(`Season`, mean_seasonal, group = 1)) +
    geom_line(linewidth = 1.4, color = "#1F78B4") +
    geom_point(size = 3, color = "#1F78B4") +
    labs(
      title = paste( y, "by Season"),
      x = "Season",
      y = y
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold", size = 18)
    )
  
  print(p)
  
  # Building filename dynamically
  fname <- paste0("../img/plots/", y_mod, "_Time-Series", ".png")
  
  # Saving the plot
  ggsave(fname, p, width = 6, height = 4, dpi = 600)
}

winningness_prop <- function(y) {
  # Producing format compatible for operations on metric
  y_sym <- sym(y)
  
  y_mod <- gsub(" ", "_", y_sym)
  
  p <- nba_processed %>%
    mutate(metric_bin = cut(!!y_sym, breaks = 100)) %>% 
    group_by(metric_bin) %>%
    summarize(
      win_rate = mean(`Win/Loss Status` == "W"),                            
      bin_mid = (as.numeric(sub("\\((.+),.*", "\\1", metric_bin)) +
                   as.numeric(sub(".*,(.+)\\]", "\\1", metric_bin))) / 2
    ) %>%                                                         
    ggplot(aes(x = bin_mid, y = win_rate)) +
    geom_point(size = 3, color = "#1F78B4") +
    scale_y_continuous(
      breaks = c(0,1),
      labels = c("Loss", "Win")
    ) +
    labs(
      title = paste(y, "vs Winningness"),
      x = y,
      y = "Empirical Win Rate"
    ) +
    theme_minimal(base_size = 14)
  
  print(p)
  
  # Building filename dynamically
  fname <- paste0("../img/plots/", y_mod, "_Winningness-For-Props", ".png")
  
  # Saving the plot
  ggsave(fname, p, width = 6, height = 4, dpi = 600)
}

winningness_of <- function(y) {
  # Producing format compatible for operations on metric
  y_sym <- sym(y)
  
  y_mod <- gsub(" ", "_", y_sym)
  
  p <- nba_processed %>%
    mutate(metric_bin = cut(!!y_sym, breaks = 100)) %>%  # bin metric
    group_by(metric_bin) %>%
    summarize(win_rate = mean(`Win/Loss Status` == "W"),
              mid = mean(!!y_sym)) %>%
    ggplot(aes(x = mid, y = win_rate)) +
    geom_point(size = 3, color = "#1F78B4") +
    scale_y_continuous(breaks = c(0, 1),
                       labels = c("Loss", "Win")) +
    labs(
      title = paste(y, "vs Winningness"),
      x = y,
      y = "Empirical Win Rate (Raw)"
    ) +
    theme_minimal(base_size = 14)
  
  print(p)
  
  # Building filename dynamically
  fname <- paste0("../img/plots/", y_mod, "_Winningness", ".png")
  
  # Saving the plot
  ggsave(fname, p, width = 6, height = 4, dpi = 600)
}

