library(tidyverse)
library(boot)
source("functions.R")

posterior_files <- c(
  "model/twopl_irt_model-1.csv",
  "model/twopl_irt_model-2.csv",
  "model/twopl_irt_model-3.csv",
  "model/twopl_irt_model-4.csv"
)

eta_estimates <- posterior_files %>%
  purrr::map_dfr(importCmdstanPosterior, parameter = "eta")

# Convert to long format
posterior_long <- eta_estimates %>%
  as_tibble() %>%
  mutate(draw_id = row_number()) %>%
  pivot_longer(cols = -draw_id,
               names_to = "mep_id_stan",
               values_to = "eta") |>
  mutate(mep_id_stan = as.numeric(str_replace(mep_id_stan, "eta\\.", "")))


alpha_estimates <- posterior_files %>%
  purrr::map_dfr(importCmdstanPosterior, parameter = "alpha")
beta_estimates <- posterior_files %>%
  purrr::map_dfr(importCmdstanPosterior, parameter = "beta")

eta_means <- eta_estimates |> colMeans()
alpha_means <- alpha_estimates |> colMeans()

beta_means <- beta_estimates |> colMeans()
# Find 5 items with lowest discrimination
# beta_means |> sort() |> head(5)

vote_titles <- df_stan |> distinct(vote_id_stan, display_title) |> arrange(vote_id_stan)

### item information in alpha-beta space ----
alpha_beta_df <- as.data.frame(rbind(alpha_means, beta_means)) |> t() |> as.data.frame() |> cbind(vote_titles)


ggplot(alpha_beta_df, aes(x = alpha_means, y = beta_means)) +
  geom_point() +
  ggtitle("Difficulty vs Discriminations Parameter for all Indicators (posterior means, n=43)") +
  labs(x = "Difficulty (α)", y = "Discrimination (β)") +
  theme_light()
ggsave("figures/alpha_v_beta.png", dpi = 300)

### ICC ----
eta_range <- seq(min(eta_means), max(eta_means), length.out = 100)

irc_data <- expand_grid(item = 1:43, eta = eta_range)

irc_data <- irc_data %>%
  # for each item-eta combination
  rowwise() %>%
  # calculate probability of turning out
  mutate(prob = inv.logit(beta_means[item] * (eta - alpha_means[item]))) %>%
  ungroup() |>
  left_join(vote_titles, by = c("item" = "vote_id_stan"))


selected_items <- c(3, 8, 26, 43, 10)
ggplot(irc_data, aes(x = eta, y = prob)) +
  geom_line(aes(group = as.factor(item)), color = "black") +
  geom_line(data = subset(irc_data, item %in% selected_items),
            aes(group = as.factor(item), color = as.factor(display_title))) +
  geom_vline(
    aes(xintercept = median(eta_means), color = "Median"),
    linewidth = 1,
    linetype = "solid"
  ) +
  labs(
    title = "Item Response Curves for all 43 Indicator Votes",
    subtitle = "5 Votes with lowest discrimination parameter colored. Median latent support of MEPs in pink for reference.",
    x = "Latent Support for Ukraine",
    y = "Probability of Voting in Favor",
    colour = ""
  )  + theme_light() +
  theme(legend.position = "bottom", legend.box = "vertical")
ggsave("figures/curves.png", dpi = 300)

### Groups 9 versus 10 -----

posterior_summary_groups_9 <- posterior_long |>
  filter(mep_id_stan %in% unique(member_groups_term_9$mep_id_stan)) |>
  left_join((select(df_stan, group_code, mep_id_stan) |> distinct())) |>
  group_by(group_code) %>%
  summarize(
    lower = quantile(eta, (1 - 0.95) / 2),
    estimate = mean(eta),
    upper = quantile(eta, 1 - (1 - 0.95) / 2),
    .groups = "drop"
  ) |> mutate(term = 9)

posterior_summary_groups_10 <- posterior_long |>
  filter(mep_id_stan %in% unique(member_groups_term_10$mep_id_stan)) |>
  left_join((select(df_stan, group_code, mep_id_stan) |> distinct())) |>
  group_by(group_code) %>%
  summarize(
    lower = quantile(eta, (1 - 0.95) / 2),
    estimate = mean(eta),
    upper = quantile(eta, 1 - (1 - 0.95) / 2),
    .groups = "drop"
  ) |> mutate(term = 10)

posteriors_groups_both_terms <- rbind(posterior_summary_groups_9, posterior_summary_groups_10) |>
  filter(group_code != "NI")

term_labeller <- as_labeller(c(`9` = "Groups During 9th Term", `10` = "Groups During 10th Term"))

ggplot(posteriors_groups_both_terms, aes(x = reorder(group_code, -estimate), y = estimate)) +
  facet_wrap(vars(term), labeller = term_labeller) +
  geom_pointrange(aes(ymin = lower, ymax = upper),
                  size = 0.5,
                  fatten = 3) +
  scale_x_discrete(labels = c(
    "RENEW",
    "EPP",
    "Greens/EFA",
    "S&D",
    "ECR",
    "The Left",
    "ID",
    "PfE",
    "ESN"
  )) +
  theme_light() +
  theme(
    axis.text.y = element_text(size = 10),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(
      angle = 45,
      vjust = 0.5,
      hjust = 0.5
    )
  ) +
  labs(
    y = "Support for Ukraine",
    x = ""
  )

### Countries within Groups ----

posterior_summary_countries_in_group <- posterior_long |>
  left_join((
    select(df_stan, group_code, country_code, mep_id_stan) |> distinct()
  )) |>
  filter(group_code != "NI") |>
  group_by(group_code) |>
  mutate(group_avg = mean(eta)) |>
  ungroup() |>
  # filter(group_code %in% c("GUE_NGL", "PFE", "ESN", "ID")) |>
  group_by(group_code, country_code) %>%
  summarize(
    lower = quantile(eta, (1 - 0.95) / 2),
    estimate = mean(eta),
    upper = quantile(eta, 1 - (1 - 0.95) / 2),
    group_avg = mean(group_avg),
    # trick to get this through the summarize
    .groups = "drop"
  ) |>
  ungroup() |>
  mutate(marker = ifelse(
    country_code %in% c("HUN"),
    "Hungary",
    "Other"
  )) |>
  mutate(highlight = ifelse(
    country_code %in% c('LTU', 'EST', 'FIN', 'LVA'),
    'Post-Soviet States & Finland',
    ifelse(country_code %in% c('HUN', 'ROU', 'SVK', 'POL'), 'Bordering Ukraine', 'Other')
  ))

group_labeller <- as_labeller(
  c(
    `ECR` = "ECR",
    `EPP` = "EPP",
    `ESN` = "ESN*",
    `GREEN_EFA` = "Greens/EFA",
    `GUE_NGL` = "The Left*",
    `ID` = "ID*",
    `PFE` = "PfE*",
    `RENEW` = 'RENEW',
    `SD` = "S&D"
  )
)

ggplot(posterior_summary_countries_in_group,
       aes(x = reorder(country_code, as.factor(country_code)), y = estimate)) +
  facet_wrap(vars(group_code), labeller = group_labeller, ncol = 2) +
  annotate(
    "rect",
    ymin = -0.25,
    ymax = Inf,
    xmin = -Inf,
    xmax = Inf,
    fill = "gray90",
    alpha = 0.3
  ) +
  geom_pointrange(
    aes(
      ymin = lower,
      ymax = upper,
      color = highlight,
      shape = marker
    ),
    size = 0.5,
    fatten = 3
  ) +
  geom_hline(aes(yintercept = group_avg, colour = "lightgray")) +
  scale_shape_manual(
    name = "",
    values = c(
      "Hungary" = 15,
      "Other" = 19
    )
  ) +
  scale_color_manual(
    name = "",
    values = c(
      "Post-Soviet States & Finland" = "blue",
      "Bordering Ukraine" = "red",
      "Other" = "azure3"
    )
  ) +
  theme_light() +
  theme(
    axis.text.y = element_text(size = 10),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(
      size = 6,
      angle = 45,
      vjust = 0.5,
      hjust = 0.5
    )
  ) +
  labs(
    y = "Support for Ukraine",
    x = ""
  )