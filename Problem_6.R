# Assignment 4
# Problem 4

main = function() {
  gamma = 5/2
  simulations = 1000

  max_samples_10_1 = maxSamplesFromPowerLawSimiulations(gamma, 10^1, simulations)
  max_samples_10_2 = maxSamplesFromPowerLawSimiulations(gamma, 10^2, simulations)
  max_samples_10_3 = maxSamplesFromPowerLawSimiulations(gamma, 10^3, simulations)
  max_samples_10_4 = maxSamplesFromPowerLawSimiulations(gamma, 10^4, simulations)
  max_samples_10_5 = maxSamplesFromPowerLawSimiulations(gamma, 10^5, simulations)
  max_samples_10_6 = maxSamplesFromPowerLawSimiulations(gamma, 10^6, simulations)

  plotSampleSet(max_samples_10_1, '10^1')
  plotSampleSet(max_samples_10_2, '10^2')
  plotSampleSet(max_samples_10_3, '10^3')
  plotSampleSet(max_samples_10_4, '10^4')
  plotSampleSet(max_samples_10_5, '10^5')
  plotSampleSet(max_samples_10_6, '10^6')

  kmax_samples = c(
    mean(max_samples_10_1$max_samples),
    mean(max_samples_10_2$max_samples),
    mean(max_samples_10_3$max_samples),
    mean(max_samples_10_4$max_samples),
    mean(max_samples_10_5$max_samples),
    mean(max_samples_10_6$max_samples)
  )

  sample_size = c(10^1, 10^2, 10^3, 10^4, 10^5, 10^6)

  max_samples_matrix = matrix(c(kmax_samples, sample_size),
    nrow=6, ncol=2, byrow=FALSE)
  max_samples_df = data.frame(max_samples_matrix)
  colnames(max_samples_df) = c('max_sample', 'sample_size')

  max_samples_df$log_max_sample = log10(max_samples_df$max_sample)
  max_samples_df$log_sample_size = log10(max_samples_df$sample_size)

  plotMaxSamples(max_samples_df)
}

# ---- Power Law Sampling ------------------------------------------------------

maxSamplesFromPowerLawSimiulations = function(gamma, samples, simulations) {

  max_samples = c()

  for (run in 1:simulations) {
    max_samples = c(max_samples, maxSampleFromPowerLaw(gamma, samples))
  }

  max_samples_df = as.data.frame(max_samples)
  max_samples_df$index = row.names(max_samples_df)

  return (max_samples_df)
}

maxSampleFromPowerLaw = function(gamma, samples) {
  max = 0

  for (i in 1:samples) {
    sample = powerLaw(gamma)
    if (sample > max) {
      max = sample
    }
  }

  return (sample)
}

powerLaw = function(gamma) {
  k = runif(1)
  c = gamma - 1
  return ( c*(1 - k)^(-gamma) )
}

# ---- Plotting ----------------------------------------------------------------

plotSampleSet = function(max_samples_df, samples) {
  file_name = paste0('images/Problem6_', samples, '.png')

  ggplot(max_samples_df,
       aes(x=index, y=max_samples)) +
  labs(title=paste('Kmax, N=', samples),
       x='Sample Number',
       y='Kmax') +
  geom_point()

  img_width = 2
  img_height = img_width * 7 / 8
  ggsave(file_name, device='png', units='in', width=img_width, height=img_height)
}

plotMaxSamples = function(max_samples_df) {
  file_name = paste0('images/Problem6_average_max.png')

  ggplot(max_samples_df,
       aes(x=log_sample_size, y=log_max_sample)) +
  labs(title='Average Kmax for n=1000 sample sets  ',
       x='Log10 Sample Size',
       y='Log10 Avg Kmax') +
  geom_point()

  img_width = 4
  img_height = img_width * 7 / 8
  ggsave(file_name, device='png', units='in', width=img_width, height=img_height)
}

main()
