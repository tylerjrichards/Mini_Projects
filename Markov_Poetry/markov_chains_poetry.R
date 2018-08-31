library(gutenbergr)
library(tidyverse)
library(markovchain)
shakespeare <- gutenberg_works(title == "Shakespeare's Sonnets") %>%  
                                 pull(gutenberg_id) %>% 
                                 gutenberg_download(verbose = F)

`%not_in%` <- function(lhs, rhs) {
  !(lhs %in% rhs)
}

bills_words <- shakespeare %>% 
  mutate(text = text %>% 
           str_trim() %>% 
           str_replace_all("--", " ") %>% 
           str_replace_all("[^[:alnum:][:space:]']", "") %>% 
           str_replace_all("^M{0,4}(CM|CD|D?C{0,3})(XC|XL|L?X{0,3})(IX|IV|V?I{0,3})$", 
                           "") %>% 
           str_to_lower()) %>% 
  filter(text %not_in% c("the sonnets", "by william shakespeare", "", " ")) %>% 
  pull(text) %>% 
  str_split(" ") %>% 
  unlist() 


punctuation <- shakespeare %>% 
  pull(text) %>% 
  str_extract_all("[^[:alnum:][:space:]']") %>% 
  unlist()

punctuation_probs <- punctuation[punctuation %not_in% c("-", "(", ")")] %>% 
  table() %>% 
  prop.table()

sonnet_chain <- markovchainFit(bills_words)
cat(markovchainSequence(n=10, markovchain = sonnet_chain$estimate),collapse = " ")

write_a_line <- function(n_lines = 1) {
  walk(1:n_lines, function(.x) {
    # put together lines of more or less average length
    lines <- markovchainSequence(n = sample(c(6:9), 1), 
                                 markovchain = sonnet_chain$estimate) %>% 
      paste(collapse = " ")
    
    #  add end-of-line punctuation based on their occurence 
    end_punctuation <- ifelse(.x == n_lines, ".", 
                              sample(names(punctuation_probs), 
                                     size = 1, 
                                     prob = punctuation_probs))
    cat(paste0(lines, end_punctuation, "  \n"))
  })
}

psuedosonnet <- function() {
  walk(1:3, function(.x) {
    write_a_line(4)
    cat("  \n")
  })
  
  write_a_line(2)
}

psuedosonnet()

