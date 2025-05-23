library(tidyverse)

votes <- read.csv("data/votes.csv") |>
  mutate(timestamp = ymd_hms(timestamp),
         resolution = str_detect(procedure_reference, "RSP"),
         olp = str_detect(procedure_reference, "COD"))

votes_geo_areas <- read.csv("data/geo_area_votes.csv")
rus_ukr_votes <- votes_geo_areas |>
  filter(geo_area_code == "UKR" | geo_area_code == "RUS") |>
  pull(vote_id) |>
  unique()

excluded_votes <- c(141001,
                    146523,
                    166551,
                    168586,
                    153059,
                    165939,
                    154564,
                    169770,
                    165216,
                    150204)

main_votes_rus_ukr <- votes |>
  filter(timestamp > "2022-01-02") |>
  filter(id %in% rus_ukr_votes) |>
  filter(is_main == "True") |>
  filter(!(id %in% excluded_votes))

member_votes <- read.csv("data/member_votes.csv")
votes_members_rus_ukr <- member_votes |>
  inner_join(main_votes_rus_ukr, by=c("vote_id" = "id")) |>
  filter(position != "DID_NOT_VOTE")

df_stan <- votes_members_rus_ukr %>%
  mutate(mep_group_combinations_id =  paste0(member_id, "_", group_code),
         mep_id_stan = as.integer(factor(mep_group_combinations_id)),
         vote_id_stan = as.integer(factor(vote_id))) |>
  mutate(position_stan = if_else(position == "FOR", 1, 0))

# ---- Prepare look-ups for Members from 9th/10th terms only
member_groups_term_10 <- df_stan |>
  filter(timestamp > ymd("2024-06-01")) |>
  distinct(mep_id_stan, .keep_all = TRUE) |>
  select(mep_id_stan, member_id, group_code)

member_groups_term_9 <- df_stan |>
  filter(timestamp < ymd("2024-06-01")) |>
  distinct(mep_id_stan, .keep_all = TRUE) |>
  select(mep_id_stan, member_id, group_code)

swtches <- df_stan |> 
  distinct(member_id, group_code) |>
  filter(group_code != "PFE") |>
  filter(group_code != "ESN") |>
  group_by(member_id) |>
  filter(n() > 1) |>
  distinct(member_id) |>
  nrow()
