library(rvest)
library(tidyverse)
library(tidytext)


url_list <- read_html('https://www.poetryfoundation.org/articles/70139/the-poetry-of-world-war-i')
url_list <- url_list %>% html_nodes('div.c-userContent p a') %>% html_attr('href')
url_list <- url_list[grepl('/poem/',url_list)]
url_list <- gsub('http://www.poetryfoundation.org/','',url_list)
url_list <- paste0('http://www.poetryfoundation.org/',url_list)

poem_urls <- lapply(url_list, read_html)

#define functions
get_text <- function(url) {
  text <- url %>% html_nodes('div.c-feature-bd div') %>% html_text()
  text <- text[!grepl('Poetry Out Loud Note',text)]
  text <- paste(text[!grepl('\n',text)], collapse = ' ')
}

get_title <- function(url) {
  title <- url %>% html_nodes('div.c-feature-hd h1') %>% html_text()
  title <- title[1]
}

get_author <- function(url) {
  author <- url %>% html_nodes('div span') %>% html_text()
  author <- author[grepl('By ',author)][1]
  author <- gsub('\n ','',author)
  author <- trimws(gsub('By ','',author))
}

results <- data.frame(text = as.character(lapply(poem_urls,  get_text)),
                      title = as.character(lapply(poem_urls, get_title)),
                      author = as.character(lapply(poem_urls, get_author)),
                      url = url_list, stringsAsFactors = FALSE)

results$year <- NA
results$year[1:14] <- 1914
results$year[15:32] <- 1915
results$year[33:49] <- 1916
results$year[50:67] <- 1917
results$year[68:82] <- 1918
results$year[83:101] <- '1919-28'

words <- results %>% unnest_tokens('word','text')
words <- words %>% anti_join(stop_words)                                

#analysis
word_count <- words %>%
  group_by(word, year) %>%
  summarise(count = n())

word_count <- word_count %>% spread(key = 'year', value = 'count', fill = 0)
word_count$total <- rowSums(word_count[,2:7])

#filter to 15
word_count <- word_count[word_count$total >= 15, 1:7]
word_count <- gather(word_count, 'year','total',-word)

ggplot(word_count, aes(x = reorder(str_to_title(word), desc(word)), y = total, group = year, fill = year)) +
  facet_grid(cols = vars(year)) + theme_minimal() + 
  geom_col() + coord_flip() +
  theme(legend.position = 'none',
        panel.grid.minor.x = element_blank(),
        panel.grid.major = element_line(color = '#BABABA')) +
  scale_fill_manual(values = c('#F79090','#F86F6F','#EA3030','#C82222','#9B1111','#6C0707')) +
  labs(title = 'Most common words mentioned in First World War poetry, by year', 
       y = 'Count of mentions',x = '', 
       caption = 'Source: Analysis of WW1 poetry at poetryfoundation.org')

ggsave('ww1_poetry.png',last_plot(),width = 9.5, height = 7.8)


