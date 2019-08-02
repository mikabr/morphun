library(tidyverse)
library(wordbankr)
days_in_month <- 365.2425/12.0

# read in google sheet with morphun item info
# https://docs.google.com/spreadsheets/d/1ZUXVnxshJHoToEWLM-aRUJ0HLdi7ZqnC3_3sx2DxN3o/edit#gid=0
# googlesheets::gs_auth()
morph_gs <- googlesheets::gs_key("1ZUXVnxshJHoToEWLM-aRUJ0HLdi7ZqnC3_3sx2DxN3o")
morph <- googlesheets::gs_read(morph_gs, ws = 1)

# check that each row is a unique stem
assertthat::assert_that(n_distinct(morph$stem) == nrow(morph))

# read in Kuperman ratings
kupx <- readxl::read_xlsx("AoA_ratings_Kuperman_et_al_BRM.xlsx")
kup <- kupx %>%
  select(word = Word, freq = Freq_pm, rating = Rating.Mean) %>%
  mutate(rating = as.numeric(rating))

# read in English CDI data
cdi <- get_instrument_data("English (American)", "WS", iteminfo = TRUE, administrations = TRUE)

# estimate AoAs for CDI data
aoa <- cdi %>%
  filter(type == "word") %>%
  fit_aoa() %>%
  select(lexical_class, definition, aoa) %>%
  mutate(definition = str_remove(definition, " \\(.*\\)"))

# combine CDI AoAs and Kuperman ratings
aoa_aug <- aoa %>%
  left_join(kup, by = c("definition" = "word")) %>%
  filter(!is.na(rating))

# predict AoA from rating and frequency
aoa_model <- lm(aoa ~ rating + log(freq), data = aoa_aug)

# combine morphun items with AoAs and Kuperman
aoa_verbs <- aoa %>% filter(lexical_class == "verbs")
morph_aug <- morph %>%
  left_join(kup, by = c("stem" = "word")) %>%
  left_join(aoa_verbs, by = c("stem" = "definition")) %>%
  select(stem, aoa, rating, freq)

# impute AoAs for morphun items from AoA model
morph_pred <- broom::augment(aoa_model, newdata = morph_aug) %>%
  mutate(aoa_imputed = if_else(is.na(aoa), .fitted, aoa),
         aoa = round(aoa_imputed * days_in_month, 1)) %>%
  select(stem, aoa)

# combine morphun item info with imputed AoAs, convert from wide to long, add ids, write csv
morph_combined <- morph %>%
  left_join(morph_pred) %>%
  gather(option_type, option, past, stem_add_d, past_add_d, group_trans) %>%
  filter(!is.na(option)) %>%
  arrange(group, stem) %>%
  mutate(id = row_number()) %>%
  mutate(stem_id = group_indices(., factor(stem, levels = unique(stem)))) %>%
  group_by(stem) %>%
  mutate(option_id = paste(stem_id, 1:n(), sep = "_"))

write_csv(morph_combined, "morphun_items.csv")
