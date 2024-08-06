library(tidyverse)
library(spacyr)
library(here)

spacy_initialize(model = "en_core_web_sm")
foo <- read_csv("https://raw.githubusercontent.com/vboyce/multiparty-tangrams/main/data/study1/filtered_chat.csv")

stop <- readLines("stopwords.txt") |>
  str_c(collapse = " ", sep = "") |>
  str_split("\\s+", simplify=T) 

funct <- c("DET", "PRON", "ADP", "CCONJ", "SCONJ", "AUX", "PART", "PUNCT", "SYM", "X", "INTJ", "SPACE", "NUM")

do_parse <- function(text) {
  spacy_parse(text) |> 
    filter(!lemma %in% stop) |> # filter(!pos %in% funct)|>
    pull(lemma) |>
    str_c(collapse = " ")
}

do_parse("an apple went away")
location <- "test_data"

do_write <- function(gameId, tangram, data) {
  # loc <- str_c(df$gameId,"_", df$tangram,".tsv")
  loc <- str_c(gameId, "_", tangram, ".tsv")
  # df %>% ungroup() |> select(data) |>
  unnest(data) |> write_tsv(here(location, loc), col_names = F)
}

complete_only <- foo |>
  filter(numPlayers == 4) |>
  filter(!is.na(text)) |>
  select(gameId, trialNum) |>
  unique() |>
  group_by(gameId) |>
  tally() |>
  filter(n == 72)

selected <- foo |>
  filter(numPlayers == 4) |> # filter(gameId=="hYBwiiFBSBi9ySwpJ") |>
  # filter(target=="/experiment/tangram_A.png") |>
  inner_join(complete_only |> select(gameId)) |>
  filter(!is.chitchat) |>
  mutate(tangram = str_sub(target, -5, -5)) |>
  select(gameId, role, spellchecked, tangram) |>
  rowwise() |>
  mutate(parse = do_parse(spellchecked)) |>
  select(gameId, tangram, role, parse) |>
  group_by(gameId, tangram) |>
  mutate(role = ifelse(row_number() %% 2 == 1, "A", "B")) |>
  nest(data = c(role, parse))

selected |> pwalk(do_write)


### steps for post:

out_location <- "test_out"

do_combine <- function(gameId, tangram) {
  a <- read_tsv(here(out_location, str_c(gameId, "_", tangram, "_tsv-lexicon.tsv")))
  b <- read_tsv(here(out_location, str_c(gameId, "_", tangram, "_tsv-lexicon-self-rep-A.tsv")))
  c <- read_tsv(here(out_location, str_c(gameId, "_", tangram, "_tsv-lexicon-self-rep-B.tsv")))
  a |>
    bind_rows(b) |>
    bind_rows(c) |>
    select(Words = `Surface Form`) |>
    unique()
}

ban_list <- c("look")
post_process <- function(phrase) {
  left <- spacy_parse(phrase) |>
    filter(!pos %in% funct) |>
    filter(!lemma %in% ban_list)
  return(nrow(left))
}

# post_process("this is a test")
all <- foo |>
  filter(numPlayers == 4) |>
  select(gameId, target) |>
  unique() |>
  inner_join(complete_only |> select(gameId)) |>
  mutate(tangram = str_sub(target, -5, -5)) |>
  select(gameId, tangram) |>
  mutate(words = pmap(list(gameId, tangram), do_combine))


filtered <- all |>
  unnest(words) |>
  rowwise() |>
  mutate(keep = post_process(Words))

filtered |>
  filter(keep != 0) |>
  write_csv(here("4p_result.csv"))
# ???

## try to cross map

# detect when it was first used (repNumber, speaker)
# detect who & when it was used by (total)

# for tangram/gameId
# get the lemmatized stuff
# get the extracted utterances
# for extracted utterance, mark which ones it's in and nest
# post-process??

said <- foo |>
  filter(numPlayers == 4) |>
  filter(gameId == "hYBwiiFBSBi9ySwpJ") |>
  filter(target == "/experiment/tangram_A.png") |>
  inner_join(complete_only |> select(gameId)) |>
  filter(!is.chitchat) |>
  mutate(tangram = str_sub(target, -5, -5)) |>
  # select(gameId, role, spellchecked, tangram) |>
  select(gameId, tangram, trialNum, repNum, playerId, role, spellchecked) |>
  rowwise() |>
  mutate(parse = do_parse(spellchecked))

extracted <- foo |>
  filter(gameId == "hYBwiiFBSBi9ySwpJ") |>
  filter(target == "/experiment/tangram_A.png") |>
  filter(numPlayers == 4) |>
  select(gameId, target) |>
  unique() |>
  inner_join(complete_only |> select(gameId)) |>
  mutate(tangram = str_sub(target, -5, -5)) |>
  select(gameId, tangram) |>
  mutate(words = pmap(list(gameId, tangram), do_combine)) |>
  unnest(words) |>
  rowwise() |>
  mutate(keep = post_process((Words))) |>
  filter(keep)

do_item <- function(df, phrase) {
  df |>
    filter(str_detect(parse, phrase))
}

extracted |>
  mutate(source = list(map_df(Words, ~ do_group(said, .)))) |>
  unnest(source)

## at scale-ish

said <- foo |>
  filter(numPlayers == 4) |>
  inner_join(complete_only |> select(gameId)) |>
  filter(!is.chitchat) |>
  mutate(tangram = str_sub(target, -5, -5)) |>
  # select(gameId, role, spellchecked, tangram) |>
  select(gameId, tangram, trialNum, repNum, playerId, role, spellchecked) |>
  rowwise() |>
  mutate(parse = do_parse(spellchecked)) |>
  group_by(gameId, tangram) |>
  nest()

extracted <- foo |>
  filter(numPlayers == 4) |>
  select(gameId, target) |>
  unique() |>
  inner_join(complete_only |> select(gameId)) |>
  mutate(tangram = str_sub(target, -5, -5)) |>
  select(gameId, tangram) |>
  mutate(words = pmap(list(gameId, tangram), do_combine)) |>
  unnest(words) |>
  rowwise() |>
  mutate(keep = post_process((Words)))
filter(keep)
group_by(gameId, tangram) |>
  nest() |>
  rename(phrases = data)

extracted_2 <- extracted |>
  filter(!is.na(keep)) |>
  filter(keep > 0) |> select(-keep) |> 
  filter(!is.na(Words)) |> 
  group_by(gameId, tangram) |>
  nest() |>
  rename(phrases = data) |> 
  left_join(said) |> 
  rowwise() |> 
  head() |> 
  unnest(phrases) |> 
  rowwise() |> 
  mutate(source = list(do_item(data,Words)))


