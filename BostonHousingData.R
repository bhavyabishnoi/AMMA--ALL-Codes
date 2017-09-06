housing <- read.table(file="https://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data")

housing.desc <- read.csv(file="housingdata_desc.csv")
names(housing.desc)

names(housing) <- housing.desc$Name