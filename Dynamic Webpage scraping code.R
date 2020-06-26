# Crawling
library("rvest")
library("RSelenium")
library("seleniumPipes")
library('RCurl')

# Data manipulation
library('dplyr') # data manipulation
library('readr') # input/output
library('data.table') # data manipulation
library('tibble') # data wrangling
library('tidyr') # data wrangling
library('purrr')

# Treat text data
library('stringr') # string manipulation





### Crawler for Review dataset

# The goal is to get all review data including their rating and vote
# Input is series name vector and output is corresponding review dataset
# So, if I input "The+Walking+Dead", then it returns a data frame with all review with a primary key(title).


# This function has whole process of scraping, the input is title, for example "The+Walking+Dead".
# The process: First, click the "Load-more" button and collect the corresponding changed url. 
# This step will be repeated until the last page of reviews.
# So, eventually I have all urls to scrapge all pages of reviews.
# And one important thing is I will only collect reviews that have rating values.
# This is need for my further analysis of "Text classification".

get_all_review <- function(title){
  
  
  ## This part is to search a title name in IMDB, 
  
  name <- title
  url <- paste0("https://www.imdb.com/find?q=", name)
  
  # Scrape url from a title.
  href <- url %>%
    read_html() %>%
    html_nodes(xpath = "//td/a") %>%
    html_attr("href")
  
  
  # When searching title, there might be same name of stars, not TV series.
  # So, this step is needed to choose only one TV series.
  
  if(length(href) > 0){
    c <- grep("name", href)
    if(1 %in% c){
      href <- href[length(c) + 1]}else{
        href <- href[1]
      }
    
    href <- substr(href, 1, str_locate(href, "\\?")-1)
    url <- paste0("https://www.imdb.com", href)
    url <- paste0(url, "reviews/")
    
    
    
    ## Now start collecting review data
    
    # Navigate to page. This one opens url page in firefox.
    ffd$navigate(url)
    
    # For this step, I need to click "load-more" button to view all reviews.
    # And when I click "load-more" button, I can find corresponding hidden domain addresses.
    # So, my goal is to list all detailed domains by clicking "load-more" button.
    
    a <- ffd$getPageSource()[[1]] %>% 
      read_html() %>% 
      html_nodes(".load-more-data") %>% 
      html_attr("data-key")
    
    # Find the 'load-more' button. But when review is not many, then there is no load-more button.
    # So, we should divide two situations.
    
    button <- url %>%  
      read_html() %>% 
      html_nodes(".load-more-data") %>% 
      html_attr("data-key")

    
    # When there is "Load-More" button vs not
    if(!is.na(button)){  
      load_btn <- ffd$findElement(using = "css selector", ".ipl-load-more__button")
      
      # Get all different urls from clicking 'load-more' button
      url_tail <- c(); i <- 1; key <- "start"
      
      while(!is.na(key)){
        load_btn$clickElement()
        
        # Wait for elements to load.
        Sys.sleep(1.5)
        
        # Get HTML data and parse
        html_data <- ffd$getPageSource()[[1]]
        key <- html_data %>% 
          read_html() %>% 
          html_nodes(".load-more-data") %>% 
          html_attr("data-key")
        url_tail[i] <- key
        i <- i+1
      }
      
      # Variable part in each url when I click the "load-more" button
      url_tail <- url_tail[-length(url_tail)] 
      url_tail <- c(a, url_tail)
      
      # Collect all urls from all load-more buttons
      url_integrated <- c()
      for(i in 1:length(url_tail)){
        url_integrated[i] <- paste(url, url_tail[i], sep = "_ajax?paginationKey=")
      }
      url_integrated[length(url_integrated)+1] <- url}else{
        url_integrated <- url
      }
    
    
    ## Now, I have all hidden domains of review data for one TV series. Next step is to 
    # collect all reviews from these domains. The problem is that there is also no rating reviews.
    # So, I will exclude these missing rating info reviews here, too.
    
    
    # input is each url, I will use lapply function after to collect all series
    integrate_all <- function(urls){
      webpage <- read_html(urls)
      
      # scraping with css function
      scrape <- function(webpage, class) {
        
        result <- webpage %>% html_nodes(class) %>% html_text()
        
        return(result)
      }
      
      
      # extract only votes numbers
      extract_votes <- function(x){
        y <- unlist(regmatches(x, gregexpr("[[:digit:]|',']+", x)))
        y <- str_replace_all(y, ",", "")
        vote <- y[seq(1, length(y), by = 2)]
        total <- y[seq(2, length(y), by = 2)]
        return(list("vote" = vote, "total" = total))
      }
      
      # extract only rating numbers
      extract_ratings <- function(x){
        ratings <- unlist(regmatches(x, gregexpr("[[:digit:]]+", x)))
        ratings <- ratings[-seq(2,length(ratings), 2)]
        return(ratings)
      }
      
      # find rows that do not contain ratings
      find_no_rating <- function(webpage){
        
        string <- scrape(webpage, ".lister-item-content")
        string <- substr(string, 33, 36)
        no_rating_rows <- which(is.na(str_locate(string, "/10"))[,1])
        return(no_rating_rows)
      }
      
      # I will not collect reviews that have no ratings.
      no_review_rows <- find_no_rating(webpage)
      k <- scrape(webpage, ".rating-other-user-rating")
      if(!n_distinct(k) == 0){
        rating <- extract_ratings(k)
        
        review <- tibble(
          id = scrape(webpage, ".display-name-link"),
          date = scrape(webpage, ".review-date"),
          vote_get = extract_votes(scrape(webpage, ".text-muted"))$vote,
          vote_total = extract_votes(scrape(webpage, ".text-muted"))$total,
          title = scrape(webpage, ".title"),
          cotent = scrape(webpage, ".text")
        )
        
        if(sum(no_review_rows) > 0){
          review <- review %>% 
            slice(-no_review_rows) %>% 
            mutate(rating = rating)
        }
        
        review <- review %>%  
          mutate(rating = rating)
      }else{
        review <- NULL}}
    
    # So, lapply is to merge all review pages by clicking "Load-more" buttons.
    A <- lapply(url_integrated, integrate_all)
    reviews <- do.call(rbind, A)
    
    if(!is.null(reviews)){
      reviews <- reviews %>% 
        mutate(names = rep(name, nrow(reviews)))}
    
  }else{
    reviews <- NULL
  }
  return(reviews)
}

# We can use lapply to collect all reviews from multiple titles. 
result <- lapply(titles, get_all_review)
result <- do.call(rbind, result)

