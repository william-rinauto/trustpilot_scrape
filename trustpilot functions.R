get_trustpilot_reviews <- function(web_page,page_number = 1) {
  # if(page_number == 3) browser()
  #issue on page 3: Blurred comment with review text showing as: 
  #  "Lorem ipsum dolor sit amet, consectetur adipiscing elit."  
  
  #issue on page 4: no comments, only headline
  

  web_page <- read_html(web_page)
  # reviews <- web_page %>%
  #   html_nodes('.review-content__text') %>%
  #   html_text()
  # 
  # 
  # 
  # reviews1 <- lapply(reviews, str_remove_all,"\\\n?^\\s+|\\s+$") %>%
  #   unlist()
  # 
  # brief_summary <- web_page %>%
  #   html_nodes('.link--dark') %>%
  #   html_text()
  
  catch_missing_reviews <- web_page %>%
    html_nodes('.link--dark , .review-content__text') 
  
  review_names_index <- catch_missing_reviews %>%
    html_name() %>%
    as_tibble() %>%
    filter(!value == 'div') #Blurred out review was messing with logic that I was using to match reviews to other data

  first_type <- review_names_index[1]
  
  review_names_index1 <- review_names_index %>%
    mutate(new_review = ifelse(value == 'a',1,0)) %>%
    mutate(review_type = ifelse(value == 'a','brief_summary','review')) %>%
    mutate(review_num = cumsum(new_review)) %>%
    rownames_to_column('rownumber')
  
  reviews_and_briefs <- catch_missing_reviews %>%
    html_text()
  
  reviews_and_briefs1 <- lapply(reviews_and_briefs, str_remove_all,"\\\n?^\\s+|\\s+$") %>%
    unlist() %>%
    str_subset('Lorem ipsum dolor sit amet, consectetur adipiscing elit',negate = T) #Drop grayed out
  
  reviews_and_briefs2 <- data.frame(content = reviews_and_briefs1) %>%
    cbind(review_names_index1) %>%
    select(content, review_type,review_num,rownumber) %>%
    pivot_wider(
      id_cols = c('review_type','review_num'),
      names_from = review_type,
      values_from = content
    ) 
  
  if(!'review' %in% names(reviews_and_briefs2)) reviews_and_briefs2 <-  mutate(reviews_and_briefs2, review = NA)
  
  brief_summary <- reviews_and_briefs2$brief_summary %>% unlist()
  reviews <- reviews_and_briefs2$review %>% unlist()
  
  
  stars <- web_page %>%
    html_nodes('.review') %>%
    html_nodes('img') %>%
    html_attrs() %>%
    unlist()
  
  #Subsetting in the function becuase it makes it easier than subsetting later
  star_grab <- function(x) str_match(as.vector(x) %>% str_subset("\\d stars?: "),"(\\d) stars?")[,2]
  
  stars1 <- star_grab(stars)
  
  dates <- web_page %>%
    html_nodes('main') %>%
    html_nodes('script') %>%
    html_text()  %>%
    str_subset('publishedDate') %>%
    str_subset('reportedDate\\\":\\\"\\d{4}.+',negate = T) %>% #This prevents date from being pulled in grayed out review I saw on page 3
    str_extract("\\d{4}-\\d{2}-\\d{2}")
  
  return(
    tibble(
      review = reviews,
      date_published = dates,
      stars = stars1,
      brief_summary = brief_summary,
      page_number = page_number,
      run_date = today()
    )
  )
}

page_count_trustpilot <- function(web_page) { #Top end of loop given 20 reviews per page
  web_page %>%
    read_html() %>%
    html_nodes('.headline__review-count') %>%
    html_text() %>%
    str_remove_all(",") %>%
    as.numeric() %/%
    20 + 1
}