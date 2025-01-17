library(tidyverse)
library(cowplot)
library(hrbrthemes)

rm(list = ls())

#-------------------------------------------------------------------------------
# inputs
#-------------------------------------------------------------------------------

# get regression results data
reg.df <- readRDS("regressions/03c_out - reg results clean.rds")

# ------------------------------------------------------------------------------
# reformat data for use in figures
# ------------------------------------------------------------------------------

# get rid of unused variables
# not plotting r2 or n
# if any weather var is included, all are included; only keep one for readability
# reshape the data is needed for ggplot structure
reg.df <- reg.df %>%
  select(-r2, -n, -ln_temp_minfb32, -ln_temp_maxfg80) %>%
  pivot_longer(wolf_dum:excl_boundary_ever)

# clean up independent variable category names
unique(reg.df$name)

names.lut <- c("wolf_dum"="Wolf presence",
               "wolf_pop_tens"="Wolf pop. (tens)",
               "ln_precip_inch"="Weather",
               "Lln_deer_pop_prehunt"="Deer pop.[t-1]",
               "year"="Year",
               "year...ever_wolves_88_10"="Year × wolf county",
               "y88_10"="1988-2010",
               "y88_16"="1988-2016",
               "excl_exit_ever"="Wolf exit counties",
               "excl_boundary_ever"="Wolf area boundary")

reg.df$name <- names.lut[reg.df$name]

# create a mapping of variables to categories in a dataframe
# this will be used in the figure to construct the 0/1 ids for independent var's later
categories.df <- bind_rows(data.frame(name = names.lut[1:4], category = "Covariates"),
                           data.frame(name = names.lut[5:6], category = "Year effects"),
                           data.frame(name = names.lut[7:8], category = "Time period"),
                           data.frame(name = names.lut[9:10], category = "Counties excluded"))

reg.df <- left_join(reg.df, categories.df, by = "name")


# create rows for covariates plotted in wolf transform models
transforms.v <- str_subset(unique(reg.df$model_id), "S3")

transforms.df <- reg.df %>%
  filter(model_id %in% transforms.v
         & x_var %in% c("wolf_pop_tens:dvc_dum", "wolf_dum:dvc_dum")) %>%
  select(coef, se, model_id, x_var) %>%
  unique() %>%
  mutate(name = x_var,
         name = ifelse(name == "wolf_dum:dvc_dum", "Wolf presence", "Wolf pop. (tens)"),
         value = T,
         category = "Coefficient plotted")

transforms_rep.df <- transforms.df %>%
  filter(model_id %in% transforms.v)

trans.lut <- c("Wolf presence" = "Wolf pop. (tens)",
               "Wolf pop. (tens)" = "Wolf presence")

transforms_rep.df$name <- trans.lut[transforms_rep.df$name]

transforms_rep.df$value <- F

transforms.df <- rbind(transforms.df, transforms_rep.df)

reg.df <- rbind(reg.df, transforms.df) %>%
  mutate(value =  as.factor(as.numeric(value)))

# clean up names for parsing
reg.df$name <- gsub(" ", "~", reg.df$name)
reg.df$name <- gsub("~×~", " %*% ", reg.df$name)
reg.df$name <- as.factor(reg.df$name)

# clean up effect type
nets.v <- str_subset(unique(reg.df$model_id), "net")

behs.v <- str_subset(unique(reg.df$model_id), "beh")

reg.df <- reg.df %>%
  mutate(effect = ifelse(model_id %in% nets.v, "A. Net effect", "B. Behavioral effect"))

# clean up model numbers
models_used <- rbind(data.frame(model_id = c(setdiff(nets.v, transforms.v),
                                          setdiff(behs.v, transforms.v)),
                                clean_model = c(1:5)),
                     data.frame(model_id = transforms.v,
                                clean_model = c(1,2,4)))

reg.df <- left_join(reg.df, models_used, by = "model_id")

# ------------------------------------------------------------------------------
# separate out different figures
# ------------------------------------------------------------------------------

# fig. 3
main_data <- reg.df[reg.df$model_id %in% c(setdiff(nets.v, transforms.v),
                                           setdiff(behs.v, transforms.v)), ] %>%
  filter(x_var == "wolf_dum:dvc_dum" & name != "Wolf~pop.~(tens)")

# fig. S1
deer_data <- reg.df[reg.df$model_id %in% c(setdiff(nets.v, transforms.v),
                                           setdiff(behs.v, transforms.v)), ] %>%
  filter(x_var == "Lln_deer_pop_prehunt:dvc_dum"& name != "Wolf~pop.~(tens)")

# fig. S3
wolf_transform <- reg.df[reg.df$model_id %in% transforms.v, ] %>%
  filter(x_var %in% c("wolf_dum:dvc_dum", "wolf_pop_tens:dvc_dum")) %>%
  mutate(clean_model = ifelse(x_var == "wolf_pop_tens:dvc_dum" & clean_model == 2, 3, clean_model)) %>%
  filter(!name %in% c("1988-2016", "Year %*% wolf~county"))

# ------------------------------------------------------------------------------
# plot functions
# ------------------------------------------------------------------------------

coef_plot.f <- function(data_to_plot, coef_to_plot, y_axis_lab){

  # data_to_plot <- main_data
  # coef_to_plot <- "Wolf~presence"
  # y_axis_lab <- "test"

  coef_plot <- ggplot(data_to_plot %>% filter(name %in% coef_to_plot), aes(x = clean_model, y = coef)) +
    geom_hline(yintercept = 0, linetype = "dashed", size = 0.3) +
    geom_errorbar(aes(ymin=coef-qnorm(.975)*se, ymax=coef+qnorm(.975)*se, color = "95% conf."), width=0, size = 1.5) +
    geom_errorbar(aes(ymin=coef-qnorm(.95)*se, ymax=coef+qnorm(.95)*se, color = "90% conf."), width=0, size = 1.5) +
    geom_point(fill = "white", shape = 21) +
    labs(y = y_axis_lab) +
    theme_ipsum_rc(base_family = "sans", base_size = 7,
                   axis_title_size = 7,
                   strip_text_size = 7, strip_text_face = "bold",
                   plot_margin = unit(c(5.5,5.5,5.5,5.5),"points"),
                   panel_spacing = unit(1, "lines"))+
    theme(panel.border = element_rect(colour = "#cccccc", fill = NA, size = 0.15),
          axis.title.x = element_blank(), axis.text.x = element_blank(),
          legend.title = element_blank(),
          legend.key = element_rect(fill = 'transparent', color = NA),
          legend.key.size = unit(0.5,"line"),
          legend.background = element_rect(fill = 'transparent', color = NA)) +
    scale_color_manual(breaks = c("90% conf.", "95% conf."),
                       values = c("#e34a33", "#f6c4bd"))

  return(coef_plot)

}

# Function to create a specification plot for a single category.
make_spec_plot <- function(data_to_plot, var_group) {

  # data_to_plot <- main_data
  # var_group = "Covariates"

  specs <- data_to_plot %>%
    filter(category %in% var_group) %>%
    mutate(name = factor(name, levels = rev(unique(name))))

  spec_plot <- ggplot(specs, aes(x = clean_model, y = name, color = value)) +
    geom_point() +
    theme_ipsum_rc(base_family = "sans", base_size = 7,
                   axis_title_size = 7,
                   plot_margin = unit(c(5.5,5.5,5.5,5.5),"points"),
                   panel_spacing = unit(1, "lines"))+
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text = element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "none") +
    scale_color_manual(values = c("grey90", "grey50"), breaks = c("0", "1")) +
    scale_y_discrete(labels = parse(text = levels(specs$name)))

  return(spec_plot)
}

# ------------------------------------------------------------------------------
# create figures
# ------------------------------------------------------------------------------

# figure 3
spec_cols <- unique(main_data$category)

coef_plot <- coef_plot.f(main_data, "Wolf~presence", "Effect of wolves on ln(DVC)") +
  facet_grid(.~effect)+
  annotate("text", x = c(1.7), y = c(0.07),
           label = c("Preferred"), size=2)+
  #ylim(-0.62,0.1)+
  geom_segment(aes(x = 1, y = 0.04, xend = 1, yend = 0.01),
               arrow = arrow(length = unit(0.1, "cm")), size = 0.2)+
  theme(legend.position = c(0.33,0.1)) # use x=0.68 for fig S2

spec_plots <- lapply(spec_cols, function(x){
  make_spec_plot(x, data_to_plot = main_data) +
    facet_grid(.~effect, scales = "free_x")
})

combined_plot <- plot_grid(plotlist = c(list(coef_plot), spec_plots),
                           labels = c("", spec_cols), label_size = 7, label_fontface = "italic", vjust = 0.5, hjust = -0.1,
                           rel_heights = c(0.7, 0.25, 0.2, 0.2, 0.2), ncol = 1, align = "v")

ggsave("figures/03d_out - fig3.pdf", height = 3.5, width = 3.42, units = "in", dpi = 900)

ggsave("figures/03d_out - fig3.png", height = 3.5, width = 3.42, units = "in", dpi = 900)

# figure S1
spec_cols <- unique(deer_data$category)

coef_plot <- coef_plot.f(deer_data, "Deer~pop.[t-1]", "Effect of deer on ln(DVC)") +
  annotate("text", x = c(1.6), y = c(0.56),
           label = c("Preferred"), size=2)+
  geom_segment(aes(x = 1, y = 0.53, xend = 1, yend = 0.5),
               arrow = arrow(length = unit(0.1, "cm")), size = 0.2) +
  theme(legend.position = c(0.275,0.16))


spec_plots <- lapply(spec_cols, function(x){
  make_spec_plot(x, data_to_plot = deer_data)
})

combined_plot <- plot_grid(plotlist = c(list(coef_plot), spec_plots),
                           labels = c("", spec_cols), label_size = 7, label_fontface = "italic", vjust = 0.5, hjust = -0.1,
                           rel_heights = c(0.6, 0.25, 0.2, 0.2, 0.2), ncol = 1, align = "v")

ggsave("figures/03d_out - figS1.pdf", height = 3.25, width = 2.35, units = "in", dpi = 900)

ggsave("figures/03d_out - figS1.png", height = 3.25, width = 2.35, units = "in", dpi = 900)

# figure S3
spec_cols <- c("Coefficient plotted", "Covariates", "Year effects", "Time period")

coef_plot <- coef_plot.f(wolf_transform, c("Wolf~presence", "Wolf~pop.~(tens)"), "Effect of wolves on ln(DVC)") +
  facet_grid(.~effect)+

  annotate("text", x = c(1.4), y = c(0.17),
           label = c("Preferred"), size=2.)+
  geom_segment(aes(x = 1, y = 0.14, xend = 1, yend = 0.11),
               arrow = arrow(length = unit(0.1, "cm")), size = 0.2)+

  annotate("text", x = c(2.5), y = c(0.17),
           label = c("Model 2"), size=2)+
  geom_segment(aes(x = 2, y = 0.14, xend = 3, yend = 0.14), size = 0.2)+
  geom_segment(aes(x = 2, y = 0.14, xend = 2, yend = 0.11),
               arrow = arrow(length = unit(0.1, "cm")), size = 0.2)+
  geom_segment(aes(x = 3, y = 0.14, xend = 3, yend = 0.11),
               arrow = arrow(length = unit(0.1, "cm")), size = 0.2)+

  annotate("text", x = c(3.65), y = c(0.17),
           label = c("Model 3"), size=2)+
  geom_segment(aes(x = 4, y = 0.14, xend = 4, yend = 0.11),
               arrow = arrow(length = unit(0.1, "cm")), size = 0.2)+

  theme(legend.position = c(0.35,0.15))

spec_plots <- lapply(spec_cols, function(x){
  make_spec_plot(x, data_to_plot = wolf_transform) +
    facet_grid(.~effect, scales = "free_x")
})

combined_plot <- plot_grid(plotlist = c(list(coef_plot), spec_plots),
                           labels = c("", spec_cols), label_size = 7, label_fontface = "italic", vjust = 0.5, hjust = -0.1,
                           rel_heights = c(0.7, 0.2, 0.31, 0.1, 0.1), ncol = 1, align = "v")

ggsave("figures/03d_out - figS3.pdf", height = 3.75, width = 3.42, units = "in", dpi = 900)

ggsave("figures/03d_out - figS3.png", height = 3.75, width = 3.42, units = "in", dpi = 900)
