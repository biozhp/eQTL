library(ggplot2)
library(dplyr)
library(tidyverse)
df <- read.table("all.env.hap.txt",header = T,sep = "\t")
unique_traits <- unique(df$Trait)
fit_results_all <- data.frame()
for (trait in unique_traits) {
  df_trait <- df %>% filter(Trait == trait)
  env_order <- df_trait %>%
    group_by(Env) %>%
    summarise(mean_pheno = mean(Phenotype)) %>%
    arrange(mean_pheno) %>%
    pull(Env)
  df_trait$Env <- factor(df_trait$Env, levels = env_order)
  df_trait$Env_num <- as.numeric(df_trait$Env)
  fit_results <- df_trait %>%
    group_by(Hap) %>%
    do({
      model <- lm(Phenotype ~ Env_num, data = .)
      coef_vals <- coef(model)
      intercept <- round(coef_vals[1], 4)
      slope <- round(coef_vals[2], 4)
      p_val <- signif(summary(model)$coefficients[2, 4], 3)
      eqn <- paste0("y = ", slope, "x + ", intercept, ", P = ", p_val)
      data.frame(Trait = trait, Hap = unique(.$Hap), Intercept = intercept, Slope = slope, P_value = p_val, Equation = eqn)
    })
  fit_results_all <- rbind(fit_results_all, fit_results)
  p <- ggplot(df_trait, aes(x = Env, y = Phenotype, fill = Hap)) +
    geom_boxplot(position = position_dodge(width = 0.9), alpha = 0.4, outlier.shape = NA) +
    geom_smooth(data = df_trait,
                aes(x = Env_num, y = Phenotype, group = Hap, color = Hap),
                method = "lm", se = FALSE, linetype = "dashed",
                inherit.aes = FALSE) +
    labs(title = paste("Trait:", trait),
         subtitle = paste(fit_results$Hap, fit_results$Equation, sep = ": ", collapse = " | "),
         x = "Env", y = trait) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = c("HapA" = "#ec347c", "HapB" = "#00aae3")) +
    scale_color_manual(values = c("HapA" = "#ec347c", "HapB" = "#00aae3"))
  
  filename <- paste0("Trait_all.env_", trait, ".pdf")
  ggsave(filename, plot = p, width = 8, height = 5, dpi = 300)
}
write.table(fit_results_all, "avephe.early.env_results.txt", sep="\t",row.names = FALSE,quote=F)
