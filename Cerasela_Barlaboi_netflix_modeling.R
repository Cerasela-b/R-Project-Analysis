# Bibliotesci necesare

library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)

# Importarea datelor

certifications <- read_csv("certifications.csv")

glimpse(certifications)
summary(certifications)

countries <- read_csv("countries.csv")
 
glimpse(countries)
summary(countries)

genres <- read_csv("genres.csv")

glimpse(genres)
summary(genres)

ratings <- read_csv(
  "ratings.csv",
  col_select = -c(imdb_id, imdb_votes, tmdb_score, tmdb_popularity)
) %>%
  drop_na()


glimpse(ratings)
summary(ratings)

# imdb_score NA's   :482    
# Trebuie sa curat valorile NA din imdb_score

titles <- read_csv(
  "titles.csv",
  col_select = -c(title, type, description, age_certification, seasons)
) %>%
  drop_na()

glimpse(titles)
summary(titles)

# am eliminat coloanele de care nu am nevoie si am curatat valorile NA
# realease year ramane in format double 

# Voi raspunde la intrebarea:
# Care factori prezic cel mai bine scorul IMDb al unui conținut
# – genul, durata, anul, țara de producție?

# Pentru analiza mea voi folosi urmatoarele seturi de date:
  # countries
  # genres
  # ratings (IMDb_score)
  # titles (runtime, release_year)

# Toate aceste date au o cheie comuna "id".

# Voi uni countries si genre cu left_join

df_countries_genres <- left_join(countries, genres, by = "id")

# Voi uni ratings si titles cu full_join

df_ratings_titles <- full_join(ratings, titles)

# Pentru a raspunde la intrebare voi combina df_countries_genre cu df_rating_titles
# astefel voi avea toate datele necesare intr-un singur data frame

df_final <- df_ratings_titles %>%
  left_join(df_countries_genres, by = "id")

# Vizualizare"

# Relatia dintre durata si scorul IMDb

ggplot(df_final, aes(x = runtime, y = imdb_score)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relatia dintre durata continutului si scorul IMDb",
       x = "Durata (minute)",
       y = "Scor IMDb")

# Linia este usor descendenta, ca cat durata este mai mare cu aata scorul IMDb scade
# In special pentru continutul cu durata mai mare de 150 de minute

# Relatia dintre anul lansarii si scorul IMDb

ggplot(df_final, aes(x = release_year, y = imdb_score)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "Evolutia scorului IMDb în functie de anul lansarii",
       x = "Anul lansarii",
       y = "Scor IMDb")
# De asemenea linia este usor descendenta, dupa anul 2010 scaderea a fost mai mare
# Pare ca odata cu aparitia unui numar mai mare de continut, a scazut nota IMDb


# Calcul scor IMDb mediu pe gen

df_genre_imdb <- df_final %>%
  drop_na(imdb_score, genre) %>%   # elimină NA înainte de agregare
  group_by(genre) %>%
  summarize(
    avg_imdb = mean(imdb_score, na.rm = TRUE),
    count = n()
  ) %>%
  arrange(desc(avg_imdb))

df_genre_imdb

ggplot(df_genre_imdb, aes(x = reorder(genre, avg_imdb), y = avg_imdb)) +
  geom_col(fill = "skyblue") +
  coord_flip() +
  labs(title = "Genurile cu cel mai mare scor mediu IMDb",
       x = "Gen",
       y = "Scor mediu IMDb")


# Distributia scorului IMDb in functie de tara


# Filtrare dupa numarul minim de titluri

df_country_imdb_filtered <- df_country_imdb %>%
  filter(count >= 20) %>%   
  arrange(desc(avg_imdb))

top_countries <- df_final %>%
  group_by(production_country) %>%
  filter(n() >= 20) %>% 
  ungroup() %>%
  filter(production_country %in% c("US","GB","FR","JP","KR","DE","CA","AU","BR","IN"))  # exemple top țări

ggplot(top_countries, aes(x = production_country, y = imdb_score)) +
  geom_boxplot(fill = "lightgreen") +
  coord_flip() +
  labs(title = "Distributia scorurilor IMDb pe tarile de top",
       x = "Tara de productie",
       y = "Scor IMDb")

# Voi folosi regresia liniara pentru a stabili cum influenteaza durata, anul lansarii, genul si
# tara de productie scorul IMDb

# Definesc top 10 genuri si top 10 tari dupa numar de titluri

top_genres <- df_final %>%
  group_by(genre) %>%
  tally() %>%
  slice_max(n, n = 10) %>%
  pull(genre)

top_countries <- df_final %>%
  group_by(production_country) %>%
  tally() %>%
  slice_max(n, n = 10) %>%
  pull(production_country)

# Creez variabile noi cu „Other” pentru rest

df_model_simple <- df_final %>%
  mutate(
    genre_simple = ifelse(genre %in% top_genres, genre, "Other"),
    country_simple = ifelse(production_country %in% top_countries, production_country, "Other")
  ) %>%
  select(imdb_score, runtime, release_year, genre_simple, country_simple) %>%
  drop_na()

# Impart datele in training si test

set.seed(123)
n <- nrow(df_model_simple)
train_index <- sample(1:n, size = 0.8 * n)
train_data <- df_model_simple[train_index, ]
test_data <- df_model_simple[-train_index, ]

# Model liniar simplificat

model_simple <- lm(imdb_score ~ runtime + release_year + genre_simple + country_simple, data = train_data)
summary(model_simple)

# Vizualizare

ggplot(train_data, aes(x = runtime, y = imdb_score)) +
  geom_point(aes(color = release_year), alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1.2) +
  labs(
    title = "How factors influence IMDb score",
    x = "Runtime (min)",
    y = "IMDb score",
    color = "Release year"
  ) +
  theme_minimal()

# Predictii pe test_data

pred <- predict(model_simple, newdata = test_data)

# Adaugam predictiile in data frame-ul test

test_data <- test_data %>% 
  mutate(predicted_imdb = pred)


# RMSE (Root Mean Squared Error)

rmse <- sqrt(mean((test_data$predicted_imdb - test_data$imdb_score)^2))
rmse

# MSE (Mean Squared Error)

mse <- mean((test_data$predicted_imdb - test_data$imdb_score)^2)
mse

library(ggplot2)

ggplot(test_data, aes(x = imdb_score, y = predicted_imdb)) +
  geom_point(alpha = 0.4, color = "blue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
    title = "Predictii vs scoruri reale IMDb",
    x = "Scor real IMDb",
    y = "Predictie IMDb"
  ) +
  theme_minimal()

# 5. Recomandare finala 

# RECOMANDARE STRATEGICA:

# Analiza arata ca factorii cei mai importanti pentru un scor ridicat IMDb sunt:

# - Genurile Documentar, Drama si Crime
# - Tarile Coreea de Sud, Japonia si Marea Britanie
# Durata si anul lansarii au efect mai mic.
# Managementul ar trebui sa prioritizeze continutul din aceste genuri si tari pentru
# a maximiza scorul IMDb si atractivitatea platformei.











