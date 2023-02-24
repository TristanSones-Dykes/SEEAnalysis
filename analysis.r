library(tidyverse)

# enter "dat" directory, enter all subdirectories with name ending in "1","2","3" and load all csv files
# with name n into a list of dataframes
load_data <- function(n) {
    # enter dat directory
    dat <- list.files(path = "dat", full.names = TRUE)
    # enter all subdirectories with name ending in "1","2","3"
    dat <- unlist(lapply(dat, function(x) list.files(path = x, pattern = "[1-3]$", full.names = TRUE)))
    # load all csv files with name n into a list of dataframes grouped by subdirectory ending in "1","2","3"
    dat_vec <- vector("list", length = 3)
    for (i in 1:3) {
        dat_vec[[i]] <- list.files(path = dat[i], pattern = n, full.names = TRUE)
        dat_vec[[i]] <- lapply(dat_vec[[i]], read.csv)
    }
    return(dat)
}

populations <- load_data("population.csv")
experiments <- load_data("experiment.csv")

pop <- populations[[1]]

# set pop$ticks to the rowname modulo 27
to_plot <- pop %>%
    rownames_to_column() %>%
    mutate(ticks = ((as.integer(rowname) - 1) %/% 27) * 100) %>%
    select(-rowname)

vlines <- data.frame(x = (1:12) * 30000)

ggplot(to_plot, aes(x = ticks, y = `Population.Every.100.Ticks`, group = Arena, color = Arena)) +
    geom_line() +
    geom_vline(data = vlines, aes(xintercept = x, linetype = "Dispersion Event"))
