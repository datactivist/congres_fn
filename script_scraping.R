library(rvest)
library(stringr)
library(downloader)
library(tidyverse)

urls <- paste0("http://congres-fn.fr/lettre/", letters)

lire_page <- function(url) {
  page <- read_html(url) %>% 
    html_nodes(".et_pb_team_member")
  nom <- page %>% 
    map_chr(~ html_node(., ".nom") %>% 
          html_text() %>% 
          str_replace_all("[0-9]+ - \\s+", "")
    )
  departement <- page %>% 
    map_chr(~ html_node(., ".departement") %>% 
              html_text()
    )
  adhesion <- page %>%
    map_int(~ html_node(., ".adhesion") %>%
              html_text() %>% 
              str_replace_all(fixed("Adhésion en "), "") %>%
              as.integer()
    )
  age <- page %>% 
    map_int(~ html_node(., ".age") %>% 
              html_text() %>% 
              str_extract("^[0-9]+") %>% 
              as.integer()
    )
  metier <- page %>% 
    map_chr(~ html_node(., ".age") %>% 
              html_text() %>% 
              str_replace("^[0-9]+ ans - \\s+", "")
    )
  fonctions <- page %>% 
    map_chr(~ html_node(., ".fonctions") %>%
              html_text() %>% 
              str_replace("^\\s+", "")
    )
  mandats <- page %>% 
    map_chr(~ html_node(., ".mandats") %>% 
              html_text() %>% 
              str_replace("^\\s+", "")
    )
  bio <- page %>% 
    map_chr(~ html_node(., ".bio") %>% 
              html_text()
    )
  photo <- page %>% 
    map_chr(~ html_node(., ".wp-post-image") %>% 
              html_attr("src")
    )
  tibble(nom, departement, adhesion, age, metier, fonctions, mandats, bio, photo)
}

candidats <- map_df(urls, lire_page)

write_excel_csv(candidats, "./candidats.csv")

walk(candidats$photo, function(x) {
  Sys.sleep(0.5)
  download(URLencode(x), paste0("./photos/", basename(x)))
  }
)
