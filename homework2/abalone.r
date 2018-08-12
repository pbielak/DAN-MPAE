# Abalone dataset analysis
source('utils.r')

abalone_data <- read.csv('data.txt')

###### Estymacja punktowa dla wieku (liczba pierścieni - Rings) badanych osobników
pe_results <- run_point_estimation(abalone_data, 'Rings')
print(paste('Mean:', round(pe_results[1], 2)))
print(paste('Variance:', round(pe_results[2], 2)))

###### Estymacja przedziałowa dla wieku
alpha = 0.99
ie_results <- run_interval_estimation(abalone_data, 'Rings', alpha)
ie_results <- round(ie_results, 3)
print(paste('For confidence level:', 1 - alpha))
print(paste('Mean interval:', ie_results[1], ie_results[2]))
print(paste('Variance interval:', ie_results[3], ie_results[4]))

alphas <- c(0.50, 0.60, 0.70, 0.80, 0.90, 0.95, 0.99)
make_interval_plot(alphas, abalone_data, 'Rings')


###### Weryfikacja hipotezy
types <- c('two.sided', 'less', 'greater')
alphas <- c(0.99, 0.95, 0.50)

for (type in types) {
  for (alpha in alphas) {
    res <- run_hypothesis_verification(abalone_data, 'Rings', c('M', 'F'), type, alpha)
    print(paste('Type:', type, 'Conf level:', 1 - alpha, '=>', paste(res, collapse = ', ')))
  }
}