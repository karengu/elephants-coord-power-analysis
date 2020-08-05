library(brms)
library(tidyverse)
library(broom)
library(tidybayes)

df.query.pilot1 <- read.csv('preprocessing/elephants-coord-20200414.csv')
df.query.pilot2 <- read.csv('preprocessing/elephants-coord-20200427.csv')
df.query <- bind_rows(
  df.query.pilot1 %>%
    filter(!memory_fail, !slider_fail, !bad_expln, !bad_ratings) %>%
    mutate(val = response, key = "response1", rep = "20200414", first_conjunct = TRUE),
  df.query.pilot2 %>%
    filter(!memory_fail, !slider_fail, !bad_expln, !bad_ratings) %>%
    gather("key", "val", response1:response2) %>%
    mutate(rep = "20200427", first_conjunct = (key=="response1" & conjunct_order=="same") | (key=="response2" & conjunct_order=="reverse")) %>%
    select(-question_order, -conjunct_order, -chapter_num, -query_predicate1, -query_predicate2)
)
df.query.critical <- df.query %>%
  select(workerid, condition,
         trial_type, predicate_1, first_conjunct,
         key, val, rep) %>%
  filter(trial_type == "critical")

df.query.s.onehot <- df.query.critical %>%
  select(workerid, predicate_1, val, condition, first_conjunct) %>%
  mutate(condition = factor(condition, levels=c("s", "np", "pp", "vp")))
num.workers <- unique(df.query.s.onehot$workerid) %>% length()
num.items <- unique(df.query.s.onehot$predicate_1) %>% length()

print("Fitting base model")
fit.s.onehot <- brm(val ~ condition + (1 + condition | workerid) + (1 + condition | predicate_1), df.query.s.onehot, family=zero_one_inflated_beta(), iter=3000, control = list(adapt_delta=0.9))

get.items.for.participant <- function(workerid) {
  sample(df.query.s.onehot$predicate_1, 8, replace = F)
}

create.new.worker <- function(workerid) {
  mutation <- sample(c(letters, 0:9), length(workerid), replace = TRUE)[1: length(workerid)]
  mutation.index <- sample(1:32, length(workerid), replace = TRUE)[1: length(workerid)]
  newid <- workerid
  str_sub(newid, mutation.index, mutation.index) <- mutation
  newid
}

# fit a single model for a simulation of n.participants
sim.and.fit <- function(seed, n.participants) {
  print(paste('Fitting model for', n.participants, "participants with seed", seed, sep=" "))
  fit.simulation <- fit.s.onehot
  
  set.seed(seed)
  
  participants <- sample(
    unique(df.query.s.onehot$workerid), ifelse(n.participants >= num.workers, num.workers, n.participants)
  )
  if (n.participants > num.workers) {
    participants <- c(participants, create.new.worker(
      sample(unique(df.query.s.onehot$workerid), n.participants-num.workers, replace = TRUE)
    ))
  }
  items <- sample(df.query.s.onehot$predicate_1, 8, replace = F)
  trials.per.participant <- c(
    rep(c("s"), each = 2), rep(c("vp"), each = 2), rep(c("pp"), each = 2), rep(c("np"), each = 2)
  )
  newdata <- tibble(
    workerid = participants,
    predicate_1 = map(workerid, get.items.for.participant)
  ) %>%
    unnest(cols = c(predicate_1)) %>%
    mutate(condition = rep(trials.per.participant, times=n.participants))
  
  d <- add_predicted_draws(
    model=fit.s.onehot,
    newdata=newdata,
    n=1,
    allow_new_levels = TRUE
  ) %>% 
    select(-.chain, -.iteration) %>% 
    mutate(val=.prediction)
  
  d$condition <- factor(
    d$condition, levels=c("s", "np", "pp", "vp")
  )
  
  fit.simulation <- update(fit.simulation,
                           newdata = d, 
                           seed = seed)
  
  fit.simulation %>%
    tidy(prob = .95)
}

# fit n.sim models for a simulation of n.participants
analyze.power <- function(n.participants, n.sim) {
  print(paste('Beginning simulation for', n.participants, "participants", sep=" "))
  sims <-
    tibble(seed = 1:n.sim) %>% 
    mutate(tidy = map(seed, sim.and.fit, n.participants = n.participants)) %>% 
    unnest(tidy)
  
  sims %>% write.csv(paste(paste('./output/elephants-coord-sim', n.participants, sep="-"), '.csv', sep=""))

  sims %>% select(seed, term, lower) %>% spread(term, lower)
}

power.data <- tibble(trial = seq(50, 300, by=50)) %>%
  mutate(tidy = map(trial, analyze.power, n.sim = 100)) %>%
  unnest(tidy)

power.data %>% write.csv('./output/elephants-coord-power-data.csv')
