# -----------------------------------------------------------
# Minimal Replication — Kobierecki & Pierzgalski (2021)
# Method: gsynth (IFE)
# Key result: One treated country (default: UK 2012), GDP per-capita growth

# -----------------------------------------------------------

# ---- Packages ----

# install.packages("devtools")
# devtools::install_github("xuyiqing/gsynth")
library(gsynth)
library(dplyr)
library(ggplot2)


# load("data.ME.26.12.21.RData")  # contains `data_long`



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

# ---- CONFIG: choose country & event year ----
treated_iso <- "GBR"   # "GBR" (UK), "BRA", "CAN", or "ZAF"
event_year  <- 2012    # 2012 (UK), 2014 (BRA – World Cup), 2010 (CAN/ZAF)

# donor pool (authors’ style; small OECD/emerging set used in their code)
donors <- c("CZE","LUX","SVN","ISR","DNK","FIN","SWE","ISL","NZL",
            "EST","LTU","LVA","SVK","HUN","CHL","TUR","CRI","MEX","COL","IND")

pool <- c(treated_iso, donors)

dat_sub <- dat %>%
  filter(cntry_code %in% pool, year >= 1993, year <= 2019)

# Create treatment indicator matching the paper’s “post” definition
dat_sub <- dat_sub %>%
  mutate(D = as.integer(cntry_code == treated_iso & year >= event_year))

# ---- Run gsynth (IFE) exactly like the paper (compact) ----
fit <- gsynth(
  gdppcgr ~ D + inflation + industry + export + lifeexp,
  data      = dat_sub,
  index     = c("cntry_code","year"),
  se        = TRUE,
  inference = "parametric",
  force     = "two-way",
  EM        = TRUE,
  CV        = TRUE,
  r         = c(0,4),     # factor number search range
  min.T0    = 5,
  nboots    = 2000,
  criterion = "mspe",
  seed      = 2000
)

print(fit)

# ---- save 2 key figures (ggplot approach) ----
if (!dir.exists("results")) dir.create("results")

# 1) ATT / gap plot
p1 <- plot(
  fit,
  theme.bw = TRUE,
  main = paste0(treated_iso, " — ATT (gap) with 95% CI")
)
p1 <- p1 + ggplot2::geom_vline(xintercept = event_year, linetype = 2)
ggplot2::ggsave("results/att_gap_plot.png", plot = p1, width = 10, height = 6.5, dpi = 150)

# 2) Treated vs counterfactual
p2 <- plot(
  fit,
  type = "counterfactual",
  raw = "band",
  shade.post = FALSE,
  theme.bw = TRUE,
  axis.adjust = TRUE,
  main = paste0(treated_iso, " — Treated vs Counterfactual")
)
p2 <- p2 + ggplot2::geom_vline(xintercept = event_year, linetype = 2)
ggplot2::ggsave("results/counterfactual_plot.png", plot = p2, width = 10, height = 6.5, dpi = 150)


cat("Saved:\n - results/att_gap_plot.png\n - results/counterfactual_plot.png\n")
