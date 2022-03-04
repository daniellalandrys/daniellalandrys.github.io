
Ratings_2 <- ratings %>%
  select(id,
         name,
         year, 
         rank, 
         average, 
         )
view(Ratings_2)

plot(Ratings_2$year, Ratings_2$average)

# estimation locale de la densité

tmp <- Ratings_2[, c("year", "average")]
tmp <- tmp[complete.cases(tmp), ]
filled.contour(kde2d(tmp$year, tmp$average), color = terrain.colors)

# on essaye un nv graphe
smoothScatter(Ratings_2[, c("average", "year")])

# on va décoouper les années
range(Ratings_2$year)

# Maintenant on va créer des classes pour la variable year
Ratings_2$year_class <- cut(Ratings_2$year, 
                            c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2025), 
                            include.lowest = TRUE, 
                      )

#maintenat on va réessayer les statistiques
plot(Ratings_2$year_class, Ratings_2$average)

#quel année a eu le plus de jeu 
summary(Ratings_2$year_class)

#Pirate plot
library(yarrr) 
pirateplot(
  average ~ year_class,
  data = Ratings_2,
  theme = 1, inf.method = "ci",
  bar.f.o = .1, bar.f.col = "grey10"
)

#ggplot
library(ggplot2) 
ggplot(Ratings_2) +
  aes(x = year_class, y = average, colour = year_class, fill = year_class)  +
  geom_pirate() +
  xlab("Année") +
  ylab("Notes") +
  ggtitle("Répartition par année selon les notes des utilisateurs sur 10") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

tapply(Ratings_2$year_class, Ratings_2$average)



install.packages("gtsummary")
library(gtsummary)

Ratings_2 %>% tbl_summary(include = c("average", "year_class"))

df <- merge(x = ratings, y = details, by = "id", all.x = TRUE)

names(df)
tb_joined$minplayers
tb_joined$maxplayers

#acm
install.packages("ade4", dep = TRUE)
library(ade4)

acm <- dudi.acm(df_clean)

acm_1 <- df_clean



# getting the class of vector
class(acm_1$average)

# modifying the col2 of data frame

acm_1$playingtime < - as.factor(acm_1$playingtime)

#################################################
print("Modified Class")
class(acm_1$playingtime)
#################################################
library(kableExtra)
ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/ratings.csv')
ratings %>%
  kbl(caption = "Notes des jeux vidéos") %>%
  kable_paper("hover", full_width = F)


look_for(details, "sun")

describe(details, "description", "Photosynthesis")

library(skimr)
skim(details, contains("Photosynthesis"))

select(details, contains("Photosynthesis"))

slice(details, 150)

kbl(head(slice(details, 150)), digits = 2, format = "html", row.names = TRUE, align = 'l', caption = "Details des jeux vidéos") %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  kableExtra::kable_styling(position = "center", bootstrap_options = "striped")

slice(details, 150)



