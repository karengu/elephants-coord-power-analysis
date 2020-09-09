library(optparse)
library(brms)
library(tidyverse)
library(broom.mixed)
library(tidybayes)

option_list = list(
  make_option(c("-l", "--lower"), type="integer", default=50, 
              help="lower bound for number of participants [default= %default]", metavar="integer"),
  make_option(c("-u", "--upper"), type="integer", default=300, 
              help="upper bound for number of participants [default= %default]", metavar="integer"),
  make_option(c("-b", "--by"), type="integer", default=50,
              help="step size for number of participants [default=%default]", metavar="integer"),
  make_option(c("-s", "--simulations"), type="integer", default=100,
              help="number of simulations [default=%default]", metavar="integer"),
  make_option(c("-i", "--index"), type="integer", default=1,
              help="batch index [default=%default]", metavar="integer"),
  make_option(c("--lower.items"), type="integer", default=11,
             help="lower bound for number of items [default=%default]", metavar="integer"),
  make_option(c("--upper.items"), type="integer", default=21,
              help="upper bound for number of items [default=%default]", metavar="integer"),
  make_option(c("--by.items"), type="integer", default=3,
              help="step size for number of items [default=%default]", metavar="integer")
)
opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

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
fit.s.onehot <- brm(
  val ~ condition + (1 + condition | workerid) + (1 + condition | predicate_1), 
  df.query.s.onehot, 
  family=zero_one_inflated_beta(), iter=3000, chains=4, cores=4, control = list(adapt_delta=0.9)
)

get.items.for.participant <- function(workerid, items) {
  sample(items, 8, replace = F)
}

create.new.worker <- function(workerid) {
  mutation <- sample(c(letters, 0:9), length(workerid), replace = TRUE)[1: length(workerid)]
  mutation.index <- sample(1:32, length(workerid), replace = TRUE)[1: length(workerid)]
  newid <- workerid
  str_sub(newid, mutation.index, mutation.index) <- mutation
  newid
}

create.new.item <- function(item) {
  mutation <- sample(letters, length(item), replace = TRUE)[1:length(item)]
  mutation.index <- sample(1:length(item), length(item), replace = TRUE)[1:length(item)]
  newitem <- item
  str_sub(newitem, mutation.index, mutation.index) <- mutation
  newitem
}

fit.intermediate <- fit.s.onehot

# fit a single model for a simulation of n.participants
sim.and.fit <- function(seed, n.participants, n.items, last.sim, first.num) {
  print(paste('Fitting model for', n.participants, "participants and", n.items, "items with seed", seed, sep=" "))
  set.seed(seed)
  
  if (n.items > length(unique(df.query.s.onehot$predicate_1))) {
    experimental.items <- c(
      create.new.item(sample(
        unique(df.query.s.onehot$predicate_1),
        n.items-length(unique(df.query.s.onehot$predicate_1)), 
        replace = TRUE
      )),
      unique(df.query.s.onehot$predicate_1)
    )
  } else {
    experimental.items <- sample(unique(df.query.s.onehot$predicate_1), n.items, replace = FALSE)
  }
  
  if (first.num) {
    # This is the first time we're simulating a group of participants, so start from scratch
    n.participants.to.sample <- n.participants
  } else {
    # We already simulated a group of participants, so build on top of it
    n.participants.to.sample <- n.participants - fit.intermediate$data %>% distinct(workerid) %>% nrow()
  }
  
  participants <- create.new.worker(
    sample(unique(df.query.s.onehot$workerid), n.participants.to.sample, replace = TRUE)
  )
  
  trials.per.participant <- c(
    rep(c("s"), each = 2), rep(c("vp"), each = 2), rep(c("pp"), each = 2), rep(c("np"), each = 2)
  )
  newdata <- tibble(
    workerid = participants,
    predicate_1 = map(workerid, get.items.for.participant, items=experimental.items)
  ) %>%
    unnest(cols = c(predicate_1)) %>%
    mutate(condition = rep(trials.per.participant, times=n.participants.to.sample))
  
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
  
  fit.simulation <- update(fit.intermediate,
                           newdata = d, 
                           seed = seed,
                           chains = 4,
                           cores = 4)
  if (last.sim) {
    fit.intermediate <- fit.simulation
  }
  
  sim.result <- fit.simulation %>%
    tidy(prob = .95)
  
  sim.summary <- sim.result %>% 
    select(group, term, conf.low) %>% 
    spread(term, conf.low) %>% 
    mutate(seed=seed, n.participants=n.participants, n.items=n.items)
  
  filename.to.write <- paste(paste('./output/elephants-coord-sim', n.participants, 'items', n.items, sep="-"), '.csv', sep="")
  
  if (!file.exists(filename.to.write)) {
    sim.summary %>%
      write.table(
        filename.to.write,
        sep=",",
        row.names = FALSE,
      )
  } else {
    sim.summary %>%
      write.table(
        filename.to.write,
        append = TRUE,
        col.names = FALSE,
        row.names = FALSE,
        sep=","
      )
  }
  
  sim.result

}

# fit n.sim models for a simulation of n.participants
analyze.power <- function(n.participants, n.items, beginning.seed, ending.seed, first.num) {
  print(paste('Beginning simulation for', n.participants, "participants and", n.items, "items", sep=" "))
  sims <-
    tibble(seed = beginning.seed:ending.seed) %>% 
    mutate(tidy = map(
      seed, 
      sim.and.fit, 
      n.participants = n.participants, 
      n.items = n.items,
      last.sim = seed == ending.seed, 
      first.num = first.num)
    ) %>%
    unnest(tidy)
  sims
}

power.data <- cross_df(list(
    trial = seq(opt$lower, opt$upper, by=opt$by),
    item = seq(opt$lower.items, opt$upper.items, by=opt$by.items)
  )) %>%
  mutate(tidy = map2(
    trial, item, analyze.power, 
    beginning.seed = (opt$index-1)*opt$simulations + 1, 
    ending.seed = (opt$index-1)*opt$simulations + 1 + opt$simulations, 
    first.num = trial == opt$lower)
  ) %>%
  unnest(tidy)

power.data %>% write.csv('./output/elephants-coord-power-data.csv')

power.data %>% select(group, term, conf.low, trial, seed) %>% filter(is.na(group)) %>% spread(term, conf.low) %>% mutate(achieved = conditionnp > 0 | conditionpp > 0 | conditionvp > 0) %>% group_by(trial) %>% summarize(power=mean(achieved))
