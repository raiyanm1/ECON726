# -----------------------------------------------------------
# Minimal Replication — Kobierecki & Pierzgalski (2021)
# Method: gsynth (IFE)
# Key result: UK 2012 Olympics, GDP per capita growth annual percentage
# -----------------------------------------------------------

library(gsynth)
library(dplyr)
library(ggplot2)

# load("data.ME.26.12.21.RData")  # contains data_long

dat <- data_long %>%
  transmute(
    cntry_code = as.character(cntry_code),
    year       = as.integer(year),
    gdppcgr    = as.numeric(gdppcgr),
    inflation  = as.numeric(inflation),
    industry   = as.numeric(industry),
    export     = as.numeric(export),
    lifeexp    = as.numeric(lifeexp)
  ) %>%
  filter(!is.na(cntry_code), !is.na(year), !is.na(gdppcgr))

# ---- CONFIG ----
treated_iso <- "GBR"
event_year  <- 2012

donors <- c(
  "CZE","LUX","SVN","ISR","DNK","FIN","SWE","ISL","NZL",
  "EST","LTU","LVA","SVK","HUN","CHL","TUR","CRI","MEX","COL","IND"
)

pool <- c(treated_iso, donors)

dat_sub <- dat %>%
  filter(cntry_code %in% pool, year >= 1993, year <= 2019) %>%
  mutate(D = as.integer(cntry_code == treated_iso & year >= event_year))

# ---- Run main gsynth ----
fit <- gsynth(
  gdppcgr ~ D + inflation + industry + export + lifeexp,
  data      = dat_sub,
  index     = c("cntry_code","year"),
  se        = TRUE,
  inference = "parametric",
  force     = "two-way",
  EM        = TRUE,
  CV        = TRUE,
  r         = c(0,4),
  min.T0    = 5,
  nboots    = 2000,
  criterion = "mspe",
  seed      = 2000
)

print(fit)

if (!dir.exists("results")) dir.create("results")

# ---- Figure 1 ATT gap ----
p1 <- plot(
  fit,
  theme.bw = TRUE,
  main = paste0(treated_iso, " ATT gap with 95% CI")
)
ggsave("results/att_gap_plot.png", plot = p1, width = 10, height = 6.5, dpi = 150)

# ---- Figure 2 treated vs counterfactual ----
p2 <- plot(
  fit,
  type = "counterfactual",
  raw = "band",
  shade.post = FALSE,
  theme.bw = TRUE,
  axis.adjust = TRUE,
  main = paste0(treated_iso, " treated vs counterfactual")
)
p2 <- p2 + ggplot2::geom_vline(xintercept = event_year, linetype = 2)
ggsave("results/counterfactual_plot.png", plot = p2, width = 10, height = 6.5, dpi = 150)

cat("Saved main figures\n")

# -----------------------------------------------------------
# Extra figures for your 3 pages of figures
# -----------------------------------------------------------

# ---- Figure 3 pre treatment fit only ----
p3 <- p2 +
  ggplot2::coord_cartesian(xlim = c(min(dat_sub$year), event_year - 1)) +
  ggplot2::ggtitle("UK pre treatment fit")

ggsave("results/pre_treatment_fit.png", plot = p3, width = 10, height = 6.5, dpi = 150)


cat("Saved pre treatment fit figure\n")

# ---- Figures 4 and 5 placebo in time ----
placebo_year <- 2008

dat_placebo <- dat %>%
  filter(cntry_code %in% pool, year >= 1993, year <= 2019) %>%
  mutate(D = as.integer(cntry_code == treated_iso & year >= placebo_year))

table(dat_placebo$D, useNA = "ifany")

fit_placebo <- gsynth(
  gdppcgr ~ D + inflation + industry + export + lifeexp,
  data      = dat_placebo,
  index     = c("cntry_code","year"),
  se        = TRUE,
  inference = "parametric",
  force     = "two-way",
  EM        = TRUE,
  CV        = TRUE,
  r         = c(0,4),
  min.T0    = 5,
  nboots    = 2000,
  criterion = "mspe",
  seed      = 2000
)

p4 <- plot(
  fit_placebo,
  theme.bw = TRUE,
  main = "Placebo ATT gap with 95% CI"
)
ggsave("results/placebo_att_gap.png", plot = p4, width = 10, height = 6.5, dpi = 150)

p5 <- plot(
  fit_placebo,
  type = "counterfactual",
  raw = "band",
  shade.post = FALSE,
  theme.bw = TRUE,
  axis.adjust = TRUE,
  main = "Placebo treated vs counterfactual"
)
p5 <- p5 + ggplot2::geom_vline(xintercept = placebo_year, linetype = 2)
ggsave("results/placebo_counterfactual.png", plot = p5, width = 10, height = 6.5, dpi = 150)

cat("Saved placebo figures\n")

# ---- Figure 6 cumulative ATT after 2012 ----
att_df <- data.frame(
  year = fit$time,
  att  = fit$att
) %>%
  filter(year >= event_year) %>%
  mutate(cum_att = cumsum(att))

p6 <- ggplot(att_df, aes(x = year, y = cum_att)) +
  geom_line(size = 1) +
  theme_bw() +
  labs(
    title = "Cumulative ATT after 2012",
    x = "Year",
    y = "Cumulative effect"
  )

ggsave("results/cumulative_att.png", plot = p6, width = 10, height = 6.5, dpi = 150)

cat("Saved cumulative ATT figure\n")
