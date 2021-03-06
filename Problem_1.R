# Assignment 4
# Problem 1
library(tidyverse)

main = function() {
  population_count = 20000
  simulations = 100

  zipf_p0_1   = richGetRicherSimulations(population_count, simulations, 0.1)
  zipf_p0_01  = richGetRicherSimulations(population_count, simulations, 0.01)
  zipf_p0_001 = richGetRicherSimulations(population_count, simulations, 0.001)

  plotZipfDistribution(zipf_p0_1,   0.1,   "images/Problem1_p0_1.png")
  plotZipfDistribution(zipf_p0_01,  0.01,  "images/Problem1_p0_01.png")
  plotZipfDistribution(zipf_p0_001, 0.001, "images/Problem1_p0_001.png")

  zipf_p0_1_filtered   = zipf_p0_1   %>% filter(log_rank < 3.2)
  zipf_p0_01_filtered  = zipf_p0_01  %>% filter(log_rank < 2.25)
  zipf_p0_001_filtered = zipf_p0_001 %>% filter(log_rank < 1.3)

  plotZipfDistributionLm(zipf_p0_1_filtered,   0.1,   "images/Problem1_p0_1_lm.png")
  plotZipfDistributionLm(zipf_p0_01_filtered,  0.01,  "images/Problem1_p0_01_lm.png")
  plotZipfDistributionLm(zipf_p0_001_filtered, 0.001, "images/Problem1_p0_001_lm.png")

  lm(log_freq ~ log_rank, zipf_p0_1_filtered)
  lm(log_freq ~ log_rank, zipf_p0_01_filtered)
  lm(log_freq ~ log_rank, zipf_p0_001_filtered)
}

# ---- Rich Get Richer Model ---------------------------------------------------

richGetRicherPopulation = function(population_count, mutation_probability) {
  counter = 0
  population = c(counter)
  counter = counter + 1

  for (index in 2:population_count) {
    if (runif(1) < mutation_probability) {
      population = c(population, counter)
      counter = counter + 1
    } else {
      next_element = population[runif(1, 1, length(population))]
      population = c(population, next_element)
    }
  }

  return (population)
}

richGetRicherSimulations = function(
  population_count, simulations, mutation_probability) {

  cumulative_population = c()

  for (run in 1:simulations) {
    population = richGetRicherPopulation(population_count, mutation_probability)
    cumulative_population = c(cumulative_population, population)
  }

  cumulative_frequency = table(cumulative_population)

  cumulative_df = as.data.frame(cumulative_frequency) %>%
    mutate(cumulative_population = as.numeric(as.character(cumulative_population))) %>%
    mutate(log_rank = log10(1 + cumulative_population)) %>%
    mutate(log_freq = log10(Freq))

}

# ---- Plotting ----------------------------------------------------------------

plotZipfDistribution = function(df, probability, file_name) {
  ggplot(df,
       aes(x=log_rank, y=log_freq)) +
  labs(title=paste('Rich Get Richer, p=', probability),
       x='Log10 Rank',
       y='Nk, Log10 Frequency') +
  geom_point()

  img_width = 4
  img_height = img_width * 7 / 8
  ggsave(file_name, device='png', units='in', width=img_width, height=img_height)
}

plotZipfDistributionLm = function(df, probability, file_name) {
  ggplot(df,
       aes(x=log_rank, y=log_freq)) +
  labs(title=paste('p=', probability, 'Regression'),
       x='Log10 Rank',
       y='Nk, Log10 Frequency') +
  geom_point() +
  geom_smooth(method='lm')

  img_width = 3
  img_height = img_width * 7 / 8
  ggsave(file_name, device='png', units='in', width=img_width, height=img_height)
}

main()
