# 19 de febrero de 2018
str_join("Letter: ", letters)

library(tibble)
library(stringr)


notas <- tibble(nombre = c("Ana", "Juan", "Pedro", "Esteban", "Alicia", "Sofia", "Eva"),
       apellido = c("Lopez", "Martinez", "Gomez", "Juarez", "Sosa", "Jara", "Dominguez"),
       nota_final = c(10, 3, 7, 8, 9, 7, 7))
write.csv(notas, "notas_mat_2015.csv")

notas <- read.csv("notas_mat_2015.csv")

library(kableExtra)

tabla <- notas %>% mutate(nombre_completo = str_c(nombre," ", apellido),
                 nombre_abr = abbreviate(nombre_completo),
                 año = 2015,
                 materia = "mat") %>%
  select(-nombre, -apellido, -X) %>%
  relocate(nota_final, .after = materia)



kableExtra::kable_styling(knitr::kable(tabla, align = "l"),
                          bootstrap_options = "condensed",
                          font_size = 15,
                          position = "center")

