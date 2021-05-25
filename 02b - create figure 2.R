library(hrbrthemes)
library(cowplot)

rm(list = ls())

# ------------------------------------------------------------------------------
# parameters
# ------------------------------------------------------------------------------

# chart formatting theme
# theme_ipsum_rc is from 'hbrthemes' package
theme <- theme_ipsum_rc(base_family = "sans",
                        base_size = 7,
                        axis_title_size = 7,
                        strip_text_size = 7,
                        strip_text_face = "bold",
                        plot_margin = unit(c(5.5,5.5,5.5,5.5),"points"),
                        panel_spacing = unit(1, "lines"))+
  theme(panel.border = element_rect(colour = "#cccccc", fill = NA, size = 0.15),
        legend.position = "none",
        legend.title = element_blank(),
        legend.key = element_rect(fill = 'transparent', color = NA),
        legend.key.size = unit(0.5,"line"),
        legend.background = element_rect(fill = 'transparent', color = NA))

# input results from previous program
plot2_data.df <- readRDS("02a_out - fig2_data.rds")

# ------------------------------------------------------------------------------
# Plot Fig. 2A-D: Trends in key variables
# ------------------------------------------------------------------------------

# create function to plot each facet
# plotting separately so we can add more facets in the next step
plot_2ad.f <- function(plot_data, var_to_plot, x_label, y_label) {

  ggplot(plot_data %>%
           filter(facet_titles == c(var_to_plot)),
         aes(x = year, y= value_chart, color = ever_wolves_pretty, group = ever_wolves_pretty)) +
    geom_line() +
    xlab(x_label) +
    ylab(y_label) +
    theme +
    facet_grid(.~facet_titles) +
    scale_color_manual(breaks = c("Wolf county", "Non-wolf county"),
                       values = c("#e34a33", "grey60"))

}

# plot each facet
# again, plotting separately so we can add more facets in the next step
p1 <- plot_2ad.f(plot2_data.df[["trends"]], "a. Wolf density", x_label = "Year", y_label = "Number") +
  theme(legend.position = c(0.3,0.93))
p2 <- plot_2ad.f(plot2_data.df[["trends"]], "b. Deer density", x_label = "Year", y_label = "Index, 1981 = 100")
p3 <- plot_2ad.f(plot2_data.df[["trends"]], "c. Deer-vehicle collisions", x_label = "Year", y_label = "Index, 1988 = 100")
p4 <- plot_2ad.f(plot2_data.df[["trends"]], "d. Other vehicle collisions", x_label = "Year", y_label = "Index, 1988 = 100")

# save plots in a running list to combine with later plots
plots.l <- list(p1, p2, p3, p4)

# ------------------------------------------------------------------------------
# Plot Fig 2E: motivating trend in DVC shares
# ------------------------------------------------------------------------------

# plot DVC shares over times
p5 <- ggplot(plot2_data.df[["dvc_shares"]], aes(x=years_to_wolves, y=value)) +
  geom_line(color = "#e34a33") +
  geom_vline(xintercept = 0, linetype = "solid", color = "grey30")+
  facet_wrap(.~variable) +
  xlab("Years since wolf recolonization") +
  ylab("Percent") +
  theme+
  annotate("text", x = c(-2.5), y = c(32.25),
           label = c("Before"), size=2) +
  annotate("text", x = c(2.5), y = c(32.25),
           label = c("After"), size=2)

# add plot to running list of plots
plots.l[[5]] <- p5

# ------------------------------------------------------------------------------
# Data for Fig 2F: event study
# ------------------------------------------------------------------------------

# plot event study for DVC shares
p6 <- ggplot(plot2_data.df[["event_study"]], aes(x=years_to_wolves, y=beta)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.3)+
  geom_vline(xintercept = 0, linetype = "solid", color = "grey30")+
  facet_wrap(.~variable) +
  geom_errorbar(aes(ymin=beta-qnorm(.975)*se, ymax=beta+qnorm(.975)*se, color = "95% conf."), width=0, size = 1.5) +
  geom_errorbar(aes(ymin=beta-qnorm(.95)*se, ymax=beta+qnorm(.95)*se, color = "90% conf."), width=0, size = 1.5) +
  geom_line(aes(x=years_to_wolves, y = beta), color = "grey40") +
  geom_point(shape = 21, size = 1.5, fill = "white", color = "grey30") +
  xlab("Years since wolf recolonization") +
  ylab("Percent") +
  theme+
  theme(legend.position = c(0.8,0.82))+
  scale_color_manual(breaks = c("90% conf.", "95% conf."),
                     values = c("#e34a33", "#f6c4bd"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("solid", "solid"))))+
  annotate("text", x = c(-2.5), y = c(12.25),
           label = c("Before"), size=2) +
  annotate("text", x = c(2.5), y = c(12.25),
           label = c("After"), size=2)+
  scale_x_continuous(breaks = c(-10,-5,0,5,10),
                     labels = c(expression(""<=-10), -5, 0, 5, expression("">=10)))

# add plot to running list
plots.l[[6]] <- p6

# combine all plot panels
combined_plot <- plot_grid(plotlist = plots.l, nrow = 2, byrow = F, align = "v")

ggsave("figures/02b_out - fig2.pdf",
       height = 3.5, width = 7, units = "in", dpi = 900)

