# Assignment 4
# Problem 4

ulysses_filename = 'ulysses.txt'

ulysses_data_raw = read.table(
  ulysses_filename,
  sep=':',
  col.names=c('word', 'freq'),
  quote='',
  as.is=TRUE
)

ulysses_data = aggregate(ulysses_data_raw, by=list('freq'), FUN=sum)
