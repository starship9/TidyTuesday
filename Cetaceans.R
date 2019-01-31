cetaceans <-
  read.csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-12-18/allCetaceanData.csv",
    stringsAsFactors = FALSE
  )
str(cetaceans)
head(cetaceans)
tail(cetaceans)
summary(cetaceans$transferDate)

library(tidyverse)
cetaceans <- cetaceans %>% select(-X)
head(cetaceans)


library(forcats)
cetaceans %>% count(species) %>%  mutate(species = as.factor(species)) %>% mutate(species = fct_reorder(species, n)) %>% ggplot(mapping = aes(x = species, y = n + 1)) + geom_col() + scale_y_log10() + coord_flip() + xlab("Species") + ylab("Natural logarithm of the total number of animals per species")

str(cetaceans)

library(lubridate)
ymd(cetaceans$entryDate)

cetaceans %>% mutate(entryDate = ymd(entryDate),
                     transferDate = ymd(transferDate)) %>% ggplot(mapping = aes(x = entryDate)) + geom_histogram(binwidth = 500) + xlab("Date of animal entering US population") + ylab("Number of animals per year")
summary(cetaceans)
dim(cetaceans)

str(cetaceans)

summary(cetaceans$birthYear)
head(cetaceans$birthYear)

cetaceans %>% filter(!is.na(birthYear)) %>% count(birthYear) %>% mutate(birthYear = fct_reorder(birthYear, n))  %>% ggplot(mapping = aes(x = birthYear, y = n + 1)) + geom_col() + coord_flip()

library(stringr)

cetaceans %>% filter(!is.na(COD), COD != "", COD != "-") %>% count(COD) %>% filter(COD !=
                                                                                     "UNDETERMINED",
                                                                                   COD != "UNKNOWN",
                                                                                   COD != "(NO RECORD)",
                                                                                   !str_detect(COD, fixed("Pneumonia", ignore_case = TRUE))) %>% mutate(COD = fct_reorder(COD, n)) %>% top_n(30) %>%  ggplot(mapping = aes(x = COD, y = n)) + geom_col() + coord_flip()

cetaceans %>% group_by(species) %>% summarise(count = n()) %>% ungroup()
str(cetaceans)

cetaceans %>% filter(!is.na(statusDate)) %>% mutate(age = as.numeric(year(statusDate)) - as.numeric(birthYear)) %>% filter(!is.na(age))  %>% ggplot(mapping = aes(x = age)) + geom_histogram()

cetaceans %>% filter(!is.na(statusDate), !is.na(birthYear), !is.na(COD), status == "Died") %>% mutate(age = as.numeric(year(statusDate)) - as.numeric(birthYear)) %>% filter(!is.na(age)) %>%  group_by(species) %>% summarise(meanAge = mean(age)) %>% mutate(species = as.factor(species), species = fct_reorder(species, meanAge)) %>% ggplot(mapping = aes(x = species, y = meanAge)) + geom_col() + coord_flip()

library(broom)

cetaceans %>% filter(!is.na(statusDate), !is.na(birthYear), !is.na(COD), status == "Died") %>% mutate(age = as.numeric(year(statusDate)) - as.numeric(birthYear)) %>% filter(!is.na(age)) %>%  group_by(species) %>% summarise(meanAge = mean(age), sdAge = sd(age), count = n(), error = qnorm(0.975)*sdAge/sqrt(count)) %>% mutate(species = as.factor(species), species = fct_reorder(species, meanAge)) %>%  filter(!is.na(sdAge)) %>%  ggplot(mapping = aes(x = species, y = meanAge)) + geom_point() + geom_errorbar(aes(ymin = meanAge - error, ymax = meanAge + error))  + coord_flip() + ylab("Average age per species")

cetaceans %>% filter(!is.na(statusDate), !is.na(birthYear), !is.na(COD), status == "Died") %>% mutate(age = as.numeric(year(statusDate)) - as.numeric(birthYear)) %>% filter(!is.na(age)) %>%  group_by(species) %>% summarise(meanAge = mean(age)) %>% mutate(species = as.factor(species), species = fct_reorder(species, meanAge))

unique(cetaceans$status)
unique(cetaceans$region)
unique(cetaceans$originLocation)

cetaceans %>% group_by(originLocation) %>% summarise(count = n()) %>% mutate(originLocation = fct_reorder(originLocation, count)) %>% filter(originLocation!="Unknown") %>% top_n(20) %>% ggplot(mapping = aes(x = originLocation, y = count)) + geom_col() + coord_flip() + xlab("Original location") + ylab("Number of species") + ggtitle("Number of species found per location")

cetaceans %>% filter(!is.na(statusDate), !is.na(birthYear), !is.na(COD), status == "Died") %>% mutate(age = as.numeric(year(statusDate)) - as.numeric(birthYear))
