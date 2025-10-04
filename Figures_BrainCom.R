#!/usr/bin/env Rscript
# =============================================================================
# Title: Figures for Systematic Review 
  # Author: Ieva Andrulyte
# Repo: https://github.com/andrulyte
# Description:
#   Reproducible script to generate Figures 2–7 for the manuscript.
#   - Figure 2: Publications per year + cumulative
#   - Figure 3: Risk of bias summary & traffic light (robvis)
#   - Figure 4: Functional methods & ROI/task summaries (six bar charts)
#   - Figure 5: Handedness consideration
#   - Figure 6: Diffusion/tractography/metrics/ROI vs whole brain/tract ROIs
#   - Figure 7: AF diffusion metrics vs dominance (healthy/clinical)
#
# -----------------------------------------------------------------------------
# REQUIRED INPUT: data table structure
# -----------------------------------------------------------------------------
# The script expects a data frame named `data` with at least the following column:
#
#   Column | Type    | Description
#   --------|---------|----------------------------------------------
#   Study   | string  | Full citation of each study, including author
#                      name(s) and publication year (4-digit number).
#
# Example:
#   data <- data.frame(
#     Study = c(
#       "Häberling et al., 2011",
#       "Karpychev et al., 2022",
#       "Westerhausen et al., 2006",
#       "Chang et al., 2017"
#     )
#   )
#
# Notes:
#   - Graphical abstract & Figure 1 were produced externally (PowerPoint/Canva).
#   - No new data were generated or analysed for this manuscript.
# =============================================================================

# ----------------------------- SETUP -----------------------------------------
options(stringsAsFactors = FALSE, scipen = 999)

# Packages
req_pkgs <- c(
  "ggplot2", "dplyr", "patchwork", "viridis", "robvis",
  "ggpattern", "readr", "fs"
)

to_install <- req_pkgs[!req_pkgs %in% installed.packages()[, "Package"]]
if (length(to_install)) install.packages(to_install, dependencies = TRUE)

library(ggplot2)
library(dplyr)
library(patchwork)
library(viridis)
library(robvis)
library(ggpattern)
library(readr)
library(fs)

# Output directory
dir_create("figures")

# save TIFF with consistent defaults
save_tiff <- function(plot, filename, width_mm, height_mm, dpi = 600, compression = "lzw") {
  ggsave(
    file.path("figures", filename),
    plot = plot,
    width = width_mm, height = height_mm, units = "mm",
    dpi = dpi, compression = compression
  )
}

# ----------------------------- DATA INPUT ------------------------------------
# Source `data` from a separate file:
# source("data/data_input.R")  # should create `data` with a column `Study`


# ----------------------------- UTILITIES -------------------------------------
# Extract 4-digit Year from Study string and coerce to numeric.
extract_year <- function(x) {
  as.numeric(sub(".*(\\b[0-9]{4}\\b).*", "\\1", x))
}

# Common minimal theme (print-friendly)
common_theme <- theme_minimal(base_size = 18) +
  theme(
    axis.text.x  = element_text(colour = "#2c3e50"),
    axis.text.y  = element_text(colour = "#2c3e50"),
    axis.title.x = element_text(face = "bold", colour = "#2c3e50"),
    axis.title.y = element_text(face = "bold", colour = "#2c3e50"),
    plot.title   = element_text(hjust = 0.5, face = "bold", colour = "#2c3e50"),
    panel.grid   = element_blank(),
    panel.background = element_rect(fill = "white"),
    legend.position = "none"
  )

fill_scale <- scale_fill_viridis_d(option = "viridis", direction = 1)

# =============================================================================
# Figure 2: Publications by year + cumulative (dual axis)
# =============================================================================
data <- data %>% mutate(Year = extract_year(Study))

data_cumulative <- data %>%
  filter(!is.na(Year)) %>%
  count(Year, name = "Count") %>%
  arrange(Year) %>%
  mutate(Cumulative = cumsum(Count))

fig2 <- ggplot(data_cumulative, aes(x = factor(Year))) +
  geom_bar(aes(y = Count), stat = "identity", fill = "#3498db", colour = "#2c3e50", width = 0.7) +
  geom_line(aes(y = Cumulative / max(Cumulative) * max(Count), group = 1),
            colour = "#e74c3c", linewidth = 1) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Included Publications by Year and Cumulative Total",
    x = "Publication year",
    y = "Number of publications"
  ) +
  scale_y_continuous(
    name = "Number of publications",
    sec.axis = sec_axis(
      trans = ~ . * max(data_cumulative$Cumulative) / max(data_cumulative$Count),
      name = "Cumulative number of publications"
    )
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, colour = "#34495e", size = 12, face = "italic"),
    axis.text.y = element_text(colour = "#34495e", size = 12),
    axis.title.x = element_text(size = 14, face = "bold", colour = "#2c3e50", margin = margin(t = 10)),
    axis.title.y = element_text(size = 14, face = "bold", colour = "#2c3e50"),
    axis.title.y.right = element_text(size = 14, face = "bold", colour = "#e74c3c"),
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold", colour = "#2c3e50"),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.border = element_rect(colour = "#2c3e50", fill = NA, linewidth = 0.8)
  )

save_tiff(fig2, "Figure2.tif", width_mm = 170, height_mm = 120)  # double-column width example

# =============================================================================
# Figure 3: Risk of bias (robvis)
# =============================================================================
# Build robvis table 
rob_data <- data.frame(
  Study = c(
    "Häberling et al., 2011", "Karpychev et al., 2022", "Westerhausen et al., 2006",
    "Chang et al., 2017", "Propper et al., 2010", "Vernooij et al., 2007",
    "Verhelst et al., 2021", "Nowell et al., 2016", "Gerrits et al., 2022",
    "Tantillo et al., 2016", "Matsumoto et al., 2008", "Yazbek et al., 2021",
    "Perlaki et al., 2013", "Piervincenzi et al., 2016", "Liégeois et al., 2016",
    "Delgado-Fernández et al., 2020", "Rodrigo et al., 2008", "Wu et al., 2022",
    "Ocklenburg et al., 2013", "Ellmore et al., 2010", "Ocklenburg et al., 2014",
    "James et al., 2015", "Briellmann et al., 2003", "Silva & Citterio, 2017",
    "Timocin et al., 2020"
  ),
  Selection = c(
    "Low risk", "Low risk", "Low risk", "Some concerns", "Some concerns", "Some concerns",
    "Some concerns", "Some concerns", "Some concerns", "Some concerns", "Some concerns",
    "Some concerns", "Some concerns", "High risk", "Some concerns", "Some concerns",
    "Some concerns", "High risk", "Some concerns", "High risk", "Some concerns",
    "Some concerns", "High risk", "High risk", "Some concerns"
  ),
  Comparability = c(
    "Low risk", "Low risk", "Some concerns", "Low risk", "Low risk", "Low risk",
    "Low risk", "Low risk", "Low risk", "Some concerns", "Low risk", "Some concerns",
    "Low risk", "Low risk", "Low risk", "Low risk", "Low risk", "Low risk",
    "Low risk", "Low risk", "Some concerns", "High risk", "Some concerns",
    "Some concerns", "High risk"
  ),
  Outcome = c(
    "Low risk", "Low risk", "Low risk", "Low risk", "Low risk", "Low risk",
    "Low risk", "Low risk", "Low risk", "Low risk", "Low risk", "Low risk",
    "Low risk", "Low risk", "Some concerns", "Some concerns", "Some concerns",
    "Low risk", "Some concerns", "Low risk", "Some concerns", "Some concerns",
    "Some concerns", "Some concerns", "Low risk"
  ),
  Overall = c(
    "Low risk", "Low risk", "Low risk", "Low risk", "Low risk", "Low risk",
    "Low risk", "Low risk", "Low risk", "Some concerns", "Some concerns", "Some concerns",
    "Some concerns", "Some concerns", "Some concerns", "Some concerns", "Some concerns",
    "Some concerns", "Some concerns", "Some concerns", "Some concerns", "High risk",
    "High risk", "High risk", "High risk"
  ),
  Weights = c(11, 11, 11, 10, 10, 10, 10, 10, 10, 9, 9, 9, 9, 8, 8, 8, 8, 8, 8, 8, 7, 5, 5, 5, 5)
)

# Use preferred labels
rob_data <- rob_data %>%
  mutate(
    Selection     = sub(" risk", "", Selection),
    Comparability = sub(" risk", "", Comparability),
    Outcome       = sub(" risk", "", Outcome),
    Overall       = sub(" risk", "", Overall)
  )

# Simplify Study names
simplify_study_name <- function(study_name) {
  gsub("^(.*?),\\s*(\\d{4}).*$", "\\1 \\2", study_name)
}
rob_data$Study <- vapply(rob_data$Study, simplify_study_name, character(1))

# robvis plots 
fig3a <- rob_summary(data = rob_data, tool = "Generic", overall = TRUE, weighted = FALSE)
fig3b <- rob_traffic_light(data = rob_data, tool = "ROB1", psize = 8)

save_tiff(fig3a, "Figure3a_rob_summary.tif", 170, 120)
save_tiff(fig3b, "Figure3b_rob_traffic_light.tif", 170, 120)

# =============================================================================
# Figure 4: fMRI figure
# =============================================================================
data_func_studies <- data.frame(
  Method = c("fMRI", "WADA", "Dichotic Listening", "fMRI & WADA"),
  Frequency = c(21, 2, 1, 1)
) %>% mutate(Proportion = Frequency / sum(Frequency) * 100)

data_rois <- data.frame(
  Category = c("Frontal", "Temporal", "Parietotemporal", "Whole brain", "Frontotemporoparietal"),
  Frequency = c(18, 6, 5, 2, 1)
) %>% mutate(Proportion = Frequency / sum(Frequency) * 100)

data_fmri_tasks <- data.frame(
  Category = c("Phonological", "Semantic", "Phonological+semantic"),
  Frequency = c(9, 5, 8)
) %>% mutate(Proportion = Frequency / sum(Frequency) * 100)

data_fmri_difficulty <- data.frame(
  Category = c("Generative", "Multiple choice", "Repetition", "Completion", "Indicative",
               "Passive listening", "Naming", "Rhyme", "Word grouping"),
  Frequency = c(15, 1, 1, 4, 1, 1, 3, 1, 1)
) %>% mutate(Proportion = Frequency / sum(Frequency) * 100)

data_fmri_baseline <- data.frame(
  Category = c("Active", "Rest/passive"),
  Frequency = c(4, 17)
) %>% mutate(Proportion = Frequency / sum(Frequency) * 100)

data_fmri_li <- data.frame(
  Category = c("Zero", "Missing", "0.2", "0.1", "0.4", "LI toolbox"),
  Frequency = c(5, 2, 5, 5, 1, 3)
) %>% mutate(Proportion = Frequency / sum(Frequency) * 100)

bar_func_studies <- ggplot(data_func_studies, aes(x = reorder(Method, -Frequency), y = Frequency, fill = Method)) +
  geom_bar(stat = "identity", colour = "black", width = 0.7) +
  labs(title = "Methods Used to Determine Language Lateralisation", x = "Method", y = "Count of Studies") +
  fill_scale + common_theme

bar_rois <- ggplot(data_rois, aes(x = reorder(Category, -Frequency), y = Frequency, fill = Category)) +
  geom_bar(stat = "identity", colour = "black", width = 0.7) +
  labs(title = "ROIs Used for Laterality Index Calculation", x = "ROI", y = "Count of Studies") +
  fill_scale + common_theme +
  theme(axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1))

bar_fmri_tasks <- ggplot(data_fmri_tasks, aes(x = reorder(Category, -Frequency), y = Frequency, fill = Category)) +
  geom_bar(stat = "identity", colour = "black", width = 0.7) +
  labs(title = "Categories of fMRI Language Tasks", x = "Task Category", y = "Count of Studies") +
  scale_y_continuous(breaks = seq(0, max(data_fmri_tasks$Frequency), by = 2)) +
  fill_scale + common_theme

bar_fmri_difficulty <- ggplot(data_fmri_difficulty, aes(x = reorder(Category, -Frequency), y = Frequency, fill = Category)) +
  geom_bar(stat = "identity", colour = "black", width = 0.7) +
  labs(title = "Categories of fMRI Language Task Difficulty", x = "Task Difficulty", y = "Count of Studies") +
  fill_scale + common_theme +
  theme(axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1))

bar_fmri_baseline <- ggplot(data_fmri_baseline, aes(x = reorder(Category, -Frequency), y = Frequency, fill = Category)) +
  geom_bar(stat = "identity", colour = "black", width = 0.7) +
  labs(title = "Categories of fMRI Task Baseline", x = "Baseline", y = "Count of Studies") +
  fill_scale + common_theme

bar_fmri_li <- ggplot(data_fmri_li, aes(x = reorder(Category, -Frequency), y = Frequency, fill = Category)) +
  geom_bar(stat = "identity", colour = "black", width = 0.7) +
  labs(title = "Laterality Index Thresholds for fMRI Studies", x = "LI Threshold", y = "Count of Studies") +
  fill_scale + common_theme

fig4 <- (bar_func_studies | bar_rois) /
  (bar_fmri_tasks | bar_fmri_difficulty) /
  (bar_fmri_baseline | bar_fmri_li) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 18, face = "bold"))

save_tiff(fig4, "Figure4.tif", width_mm = 425, height_mm = 525)

# =============================================================================
# Figure 5: Handedness figure
# =============================================================================
data_handedness <- data.frame(
  Category = c("Not considered", "Considered", "Only right-handers", "Only left-handers"),
  Frequency = c(8, 7, 6, 3)
)

fig5 <- ggplot(data_handedness, aes(x = reorder(Category, -Frequency), y = Frequency, fill = Category)) +
  geom_bar(stat = "identity", colour = "black", width = 0.7) +
  theme_minimal(base_size = 18) +
  labs(title = "Handedness Consideration in Study", x = "Handedness Consideration", y = "Count of Studies") +
  scale_fill_viridis_d(option = "viridis", direction = 1) +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1, vjust = 1, size = 12, colour = "#2c3e50"),
    axis.text.y = element_text(size = 12, colour = "#2c3e50"),
    plot.title  = element_text(hjust = 0.5, size = 16, face = "bold", colour = "#2c3e50"),
    axis.title.x = element_text(size = 14, face = "bold", colour = "#2c3e50"),
    axis.title.y = element_text(size = 14, face = "bold", colour = "#2c3e50"),
    panel.grid   = element_blank(),
    panel.background = element_rect(fill = "white"),
    legend.position  = "none"
  )

save_tiff(fig5, "Figure5.tif", width_mm = 170, height_mm = 120)

# =============================================================================
# Figure 6: Diffusion MRI figure
# =============================================================================
common_theme_large <- theme_minimal(base_size = 18) +
  theme(
    axis.text.x  = element_text(colour = "#2c3e50", size = 16),
    axis.text.y  = element_text(colour = "#2c3e50", size = 16),
    axis.title.x = element_text(face = "bold", colour = "#2c3e50", size = 18),
    axis.title.y = element_text(face = "bold", colour = "#2c3e50", size = 18),
    plot.title   = element_text(hjust = 0.5, face = "bold", colour = "#2c3e50", size = 20),
    panel.grid   = element_blank(),
    panel.background = element_rect(fill = "white"),
    legend.position  = "none"
  )

data_diffusion <- data.frame(Category = c("DTI", "CSD"), Frequency = c(23, 3))

data_tractography <- data.frame(
  Category = c("Deterministic", "Probabilistic", "Missing"),
  Frequency = c(11, 9, 5)
) %>% arrange(desc(Frequency))

data_diffusion_metrics <- data.frame(
  Category = c("FA", "Volume", "MD", "Fibres", "Length", "FBA", "FD", "ADC", "Voxels"),
  Frequency = c(20, 9, 5, 4, 3, 2, 2, 1, 1)
)

data_roi_vs_whole_brain <- data.frame(Category = c("ROI", "Whole brain"), Frequency = c(21, 4))

data_brain_regions <- data.frame(
  Category = c("AF", "CC", "UF", "IFOF", "ILF", "OR", "SLF", "CST", "CBT", "Temporal", "Parietal"),
  Frequency = c(15, 5, 4, 3, 2, 1, 1, 1, 1, 1, 1)
)

b1 <- ggplot(data_diffusion, aes(x = reorder(Category, -Frequency), y = Frequency, fill = Category)) +
  geom_bar(stat = "identity", colour = "black", width = 0.7) +
  labs(title = "Types of Diffusion Analyses", x = "Diffusion Analysis", y = "Count of Studies") +
  scale_fill_viridis_d(option = "viridis") + common_theme_large

b2 <- ggplot(data_tractography, aes(x = reorder(Category, -Frequency), y = Frequency, fill = Category)) +
  geom_bar(stat = "identity", colour = "black", width = 0.7) +
  labs(title = "Types of Tractography", x = "Tractography Type", y = "Count of Studies") +
  scale_fill_viridis_d(option = "magma") + common_theme_large

b3 <- ggplot(data_diffusion_metrics, aes(x = reorder(Category, -Frequency), y = Frequency, fill = Category)) +
  geom_bar(stat = "identity", colour = "black", width = 0.7) +
  labs(title = "Types of Diffusion Metrics", x = "Diffusion Metric", y = "Count of Studies") +
  scale_fill_viridis_d(option = "plasma") + common_theme_large +
  theme(axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1))

b4 <- ggplot(data_roi_vs_whole_brain, aes(x = reorder(Category, -Frequency), y = Frequency, fill = Category)) +
  geom_bar(stat = "identity", colour = "black", width = 0.7) +
  labs(title = "ROI vs Whole Brain Approaches", x = "Approach", y = "Count of Studies") +
  scale_fill_viridis_d(option = "cividis") + common_theme_large

b5 <- ggplot(data_brain_regions, aes(x = reorder(Category, -Frequency), y = Frequency, fill = Category)) +
  geom_bar(stat = "identity", colour = "black", width = 0.7) +
  labs(title = "Tract ROIs Used in Studies", x = "Fibre Bundle", y = "Count of Studies") +
  scale_fill_viridis_d(option = "inferno") + common_theme_large +
  theme(axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1))

fig6 <- (b1 + b2 + b3) / (b4 + b5) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 18, face = "bold"))

save_tiff(fig6, "Figure6.tif", width_mm = 425, height_mm = 525)

# =============================================================================
# Figure 7: Arcuate fasciculus figure 
# =============================================================================
af_df <- data.frame(
  Metric = rep(c("Volume", "Length", "RFD", "Fixel-based", "FA", "Streamlines",
                 "Volume", "FA", "Streamlines", "Length", "MD"), each = 3),
  Population = c(rep("Healthy", 18), rep("Clinical", 15)),
  Category = c(
    "Yes", "No", "Handedness dependent",  # Volume (Healthy)
    "Yes", "No", "Handedness dependent",  # Length (Healthy)
    "Yes", "No", "Handedness dependent",  # RFD (Healthy)
    "Yes", "No", "Handedness dependent",  # Fixel-based (Healthy)
    "Yes", "No", "Handedness dependent",  # FA (Healthy)
    "Yes", "No", "Handedness dependent",  # Streamlines (Healthy)
    "Yes", "No", "Lesion dependent",      # Volume (Clinical)
    "Yes", "No", "Lesion dependent",      # FA (Clinical)
    "Yes", "No", "Lesion dependent",      # Streamlines (Clinical)
    "Yes", "No", "Lesion dependent",      # Length (Clinical)
    "Yes", "No", "Handedness dependent"   # MD (Clinical)
  ),
  Count = c(
    0, 0, 2,  # Volume (Healthy)
    0, 1, 0,  # Length (Healthy)
    1, 0, 1,  # RFD (Healthy)
    0, 2, 0,  # Fixel-based (Healthy)
    0, 2, 3,  # FA (Healthy)
    0, 1, 0,  # Streamlines (Healthy)
    4, 1, 0,  # Volume (Clinical)
    4, 0, 1,  # FA (Clinical)
    3, 0, 0,  # Streamlines (Clinical)
    0, 1, 0,  # Length (Clinical)
    0, 1, 0   # MD (Clinical)
  )
)

af_df <- af_df %>%
  filter(Count > 0) %>%
  mutate(
    Pattern = ifelse(Category == "No", "stripe", "none"),
    Category = dplyr::recode(Category,
                             "Yes" = "Significant association",
                             "No"  = "No significant association")
  ) %>%
  # Remove the duplicated/undesired row per your original slice(-12)
  # (Here we replicate intent safely by removing the 12th row after filtering if present)
  { if (nrow(.) >= 12) slice(., -12) else . }

fig7 <- ggplot(af_df, aes(x = Population, y = Count, fill = Category)) +
  geom_bar(stat = "identity", position = "stack", colour = "black", width = 0.7) +
  facet_wrap(~ Metric, scales = "fixed", nrow = 1) +
  scale_fill_manual(
    values = c(
      "Significant association"    = "#1b9e77",
      "No significant association" = "#d95f02",
      "Handedness dependent"       = "#7570b3"
    )
  ) +
  guides(pattern = "none", fill = guide_legend(title = NULL)) +
  ylim(0, 5) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Association of diffusion-based MRI metrics with functional language dominance",
    x = "Population", y = "Count of Studies"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, colour = "#2c3e50", size = 12),
    axis.text.y = element_text(colour = "#2c3e50", size = 12),
    plot.title  = element_text(hjust = 0.5, size = 16, face = "bold", colour = "#2c3e50"),
    axis.title.x = element_text(size = 14, face = "bold", colour = "#2c3e50"),
    axis.title.y = element_text(size = 14, face = "bold", colour = "#2c3e50"),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 50),
    legend.position = "top",
    legend.text = element_text(size = 12)
  )

save_tiff(fig7, "Figure7.tif", width_mm = 255, height_mm = 135)

# ----------------------------- End of script ---------------------------------
