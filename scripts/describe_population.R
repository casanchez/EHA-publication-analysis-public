source("packages.R")

# load authorship data
targets::tar_load(auths_with_gender)

# create summary table
## number of unique countries
## number of unique institution ids
## gender count

# number of unique institutions and countries
auths_with_gender |>
  dplyr::select(institution_id,country_name) |>
  summarise(across(where(is.character), n_distinct)) 



### how many people are publishing on one health related topics from 2011 to 2021
### use topics in papers to find related works and population estimate for authors 

### using dois from publications, pull concept ids for all works in the corpus
options(openalexR.mailto = "collin.schwantes@yale.edu")

dois <- readr::read_csv("dois/dois.csv")

# get works based on DOIs
works <- openalexR::oa_fetch(entity = "works",doi = dois$identifier,
                             options = list(select = c("doi","topics",
                                                       "concepts",
                                                       "authorships",
                                                       "primary_location")))

works$original_identifer <- works$doi


### look at works not retrieved from open alex
works_not_oa <- dplyr::anti_join(dois,works,by = c("identifier" = "doi"))
works_not_oa

## most of these works had mismatching DOIs between the ids object and the DOI field.
## use DOI in ids OBJECT for better retrieval

## could not find https://ijisr.issr-journals.org/abstract.php?article=IJISR-18-353-04
works_not_oa_df <- data.frame(identifier = c(
  "https://doi.org/10.7916/1g9a-gs78",
  "https://doi.org/10.7916/d8qr4vkm",
  "https://doi.org/10.5167/uzh-203946",
  "https://doi.org/10.17863/cam.39009",
  "https://doi.org/10.7916/d8cz37kr",
  "https://doi.org/10.5455/javar.2016.c147",
  "https://doi.org/10.1128/jvi.01059-18",
  "https://doi.org/10.3929/ethz-b-000493866",
  "https://doi.org/10.5455/javar.2016.c153",
  "https://doi.org/10.7916/d8vd8fmd",
  "https://doi.org/10.5167/uzh-141192",
  "https://doi.org/10.17863/cam.65556",
  "https://doi.org/10.7916/d87942n5",
  "https://doi.org/10.5455/javar.2016.c181"
  ),
  oa_id = c("https://openalex.org/W2804947704")
)


works_updated_oa_id <- openalexR::oa_fetch(entity = "works",
                                     ids.openalex = works_not_oa_df$oa_id,
                                     options = list(select = c("doi","topics",
                                                               "concepts",
                                                               "authorships",
                                                               "primary_location")))


works_updated_oa_id$original_identifer <- "https://www.ajol.info/index.php/tjs/article/view/171309"


works_updated_oa_doi <- openalexR::oa_fetch(entity = "works",
                                           doi = works_not_oa_df$identifier,
                                           options = list(select = c("doi",
                                                                     "topics",
                                                                     "concepts",
                                                                     "authorships",
                                                                     "primary_location")))

## confusingly, the doi RETURNED from a query is consistently the DOI attribute
## see in the JSON file.

works_updated_oa_doi$original_identifer <- works_updated_oa_doi$doi

works_complete <- dplyr::bind_rows(works,works_updated_oa_id,works_updated_oa_doi)


works_complete$authorship_count <- works_complete$authorships |>
  purrr::map_dbl(\(x){
    nrow(x)
  })

works_complete |>
  dplyr::filter(doi == "https://doi.org/10.5455/javar.2016.c147")
  

works_complete |>
  dplyr::filter(doi == original_identifer) 

works_complete_for_publication_table <- works_complete |>
  dplyr::select(original_identifer,authorship_count, source_display_name, source_id)

readr::write_csv(works_complete_for_publication_table,"dois/works_with_count.csv")

works_complete <- works_complete|>
  dplyr::mutate(article_id = row_number())


concepts <- works$concepts |>
  purrr::map_df(function(x){
    x |>
      dplyr::select(id,score,display_name)
  })

### select topics that are in the 95th percentile 

concepts |>
  group_by(id,display_name) |>
  dplyr::summarise(count = n()) |>
  dplyr::arrange(desc(count)) |>
  View()


#### topics

works$topics


topics_df <- data.frame(id = character(), score = numeric(), display_name = character(), article_id = numeric())

### confirming numbers with for loop

# for(article_id in 1:nrow(works)){
#   work <- works[article_id,]
#   df <- work$topics[[1]] |>
#     dplyr::filter(type == "topic") |>
#     dplyr::select(id,score,display_name)
#   
#   df$article_id <- article_id
#   
#   if(article_id == 1){
#     topics_df = df
#     next()
#   }
#   
#   topics_df <- rbind(topics_df,df)
#   
# }
# 
# topics_df |>
#   dplyr::filter(id %in% top_topics) |>
#   pull(article_id) |>
#   unique() |>
#   length()





topics <- purrr::map2_df(works_complete$topics,works_complete$article_id, function(x,y){
    df <- x |>
      dplyr::filter(type == "topic") |>
      dplyr::select(id,score,display_name)
    
    df$article_id <- y
    
    return(df)
  })


top_topics <- topics |>
  group_by(id,display_name) |>
  dplyr::summarise(count = n()) |>
  dplyr::arrange(desc(count)) |>
  dplyr::ungroup() |>
  dplyr::slice(1:3) |>
  dplyr::pull(id)

## what coverage does this give for our corpus?


topics |>
  dplyr::filter(id %in% top_topics) |>
  pull(article_id) |>
  unique() |>
  length()

unique(topics$article_id) |> length()

### optimal coverage leaving the top three fixed

topics_vector <- topics |>
  dplyr::distinct(id) |>
  dplyr::filter(!id %in% top_topics) |>
  dplyr::pull(id)

optimal_topics <- data.frame(id = character(),coverage = numeric())

for(z in topics_vector){

  new_top <- append(top_topics,z)
  
  coverage <- topics |>
    dplyr::filter(id %in% new_top) |>
    pull(article_id) |>
    unique() |>
    length()
  
  new_df <- data.frame(id = z, coverage = coverage)
  
  optimal_topics <- rbind(optimal_topics,new_df)
    
}

optimal_topics |>
  dplyr::arrange(desc(coverage)) |>
  View()


### add two items


optimal_topics <- data.frame(topic_ids = list(),coverage = numeric())


for(z in topics_vector){
  topics_subset <- topics_vector[topics_vector != z]
  for(w in topics_subset){
    
    new_top <- append(top_topics,c(z,w))
    
    coverage <- topics |>
      dplyr::filter(id %in% new_top) |>
      pull(article_id) |>
      unique() |>
      length()
    
    new_df <- tibble::tibble(topic_ids = list(new_top), coverage = coverage)
    
    optimal_topics <- rbind(optimal_topics,new_df)
    
  }
}

optimal_topics_sorted <- optimal_topics |>
  dplyr::arrange(desc(coverage))

optimal_topics_sorted

### optimal coverage opening up all positions
### takes wayyyyy to long
# 
# topics |>
#   group_by(id,display_name) |>
#   dplyr::summarise(count = n()) |>
#   View()
# 
# 
# topics_vector_full <- topics |>
#   group_by(id,display_name) |>
#   dplyr::summarise(count = n()) |>
#   dplyr::filter(count > 9) |>
#   dplyr::pull(id)
# 
# 
# optimal_topics <- data.frame(topic_ids = list(),coverage = numeric())
# 
# 
# crossed_df <- tidyr::crossing(pos_1 = topics_vector_full,
#                                  pos_2 = topics_vector_full,
#                                  pos_3 = topics_vector_full,
#                                  pos_4 =topics_vector_full,
#                                  pos_5 =topics_vector_full)
# 
# 
# 
# 
# # all five items are unique
# combos_filter <- pmap_lgl(crossed_df, function(pos_1,pos_2,pos_3,pos_4, pos_5){
#   
#   vec_topics <- c(pos_1,pos_2,pos_3,pos_4, pos_5)
#   length(unique(vec_topics)) == 5
# }
# )
# 
# crossed_df_filtered <- crossed_df[combos_filter,]
# 
# 
# for(i in 1:nrow(crossed_df_filtered)){
#     
#     if(i%%100000 == 0){
#       print(i)
#     }
#   
#     new_top <- crossed_df_filtered[i,] |> as.character()
#     
#     coverage <- topics |>
#       dplyr::filter(id %in% new_top) |>
#       pull(article_id) |>
#       unique() |>
#       length()
#     
#     new_df <- tibble::tibble(topic_ids = list(new_top), coverage = coverage)
#     
#     optimal_topics <- rbind(optimal_topics,new_df)
#     
# }
# 
# optimal_topics_sorted <- optimal_topics |>
#   dplyr::arrange(desc(coverage))
# 
# optimal_topics_sorted


### back trace - start at the topic with the highest coverage and then
### work down the topics list

## start from some topic and remove that topic, then remove the next top topic,
topic_count <- topics |>
  group_by(id) |>
  dplyr::summarise(count = n()) |>
  ungroup() |>
  arrange(desc(count))

topics_with_count <- left_join(topics, topic_count,"id") |>
  arrange(desc(count))

topics_best_coverage <- c()

## start with highest and work down the list

## loop over a sorted vector of topics
for(a in topic_count$id){
  
  ## check that the topic is in the dataframe with articles
  if(a %in% topics_with_count$id){
    
    # give me all the articles associated with that topic
    topic_articles <- topics_with_count |>
      dplyr::filter(id == a)|>
      pull(article_id)
    
    # drop those articles from the dataframe with articles
    topics_with_count <- topics_with_count |>
      dplyr::filter(!article_id %in% topic_articles)
    
    topics_best_coverage <- append(topics_best_coverage,a)
    
    ## can we beat 309?
    coverage <- topics |>
      dplyr::filter(id %in% topics_best_coverage) |>
      pull(article_id) |>
      unique() |>
      length()
  }
  next()
  
}


# backtrace with random order
# find_best_coverage <- function(topics, topic_df, iterations = 1000){
# 
#   
#   
#   # record coverage
#   topic_df_coverage <- topic_df 
#   coverage_df <- tibble(ids = list(), coverage = numeric())
#   for(i in 1:iterations){
#     # print(i)
#   topics_random <- sample(topics,size = 5,replace = FALSE)
#   topics_best_coverage <- c()
#   topic_df_iter <- topic_df
#   for(a in topics_random){
#       # print(a)
#       if(a %in% topic_df_iter$id){
#         topic_articles <- topic_df_iter |>
#           dplyr::filter(id == a)|>
#           pull(article_id)
#         
#         # drop those articles from topics
#         topic_df_iter <- topic_df_iter |>
#           dplyr::filter(!article_id %in% topic_articles)
#         
#         topics_best_coverage <- append(topics_best_coverage,a)
#         
#         if(length(topics_best_coverage) == 5){
#            # browser()
#           ## can we beat 309?
#           coverage <- topic_df_coverage |>
#             dplyr::filter(id %in% topics_best_coverage) |>
#             pull(article_id) |>
#             unique() |>
#             length()
#           
#           df <- tibble(ids = list(topics_best_coverage), coverage = coverage)
#           
#           coverage_df <- rbind(coverage_df,df)
#           
#           
#           topics_best_coverage <- c()
#           break()
#         }
#       }
#   next()
#   }
#   }
#   return(coverage_df)
# }
# coverage_out <- find_best_coverage(topics = topics,topic_df = topics_with_count,iterations = 10000)


# backtrace - starting with top and swapping
find_best_coverage <- function( topics = topics, iterations = 1000, jitter_start){
  topic_count <- topics |>
    group_by(id) |>
    dplyr::summarise(count = n()) |>
    ungroup() |>
    arrange(desc(count))
  
  topics_with_count <- left_join(topics, topic_count,"id") |>
    arrange(desc(count))
  
  # record coverage
  topic_df_coverage <- topics_with_count 
  coverage_df <- tibble(ids = list(), coverage = numeric())
  for(i in 1:iterations){
    # print(i)
    if(i == 1){
      # back trace from highest
      topics_iter <- topic_count$id  
    }else{
      # jitter starting positions
      topics_iter <- topic_count$id
      if(i > jitter_start){
        i <- jitter_start
      }
      topics_iter[1:i] <- topics_iter[sample(1:i,size = i)] 
    }
    
    topics_best_coverage <- c()
    topic_df_iter <- topics_with_count
    for(a in topics_iter){
      # print(a)
      if(a %in% topic_df_iter$id){
        topic_articles <- topic_df_iter |>
          dplyr::filter(id == a)|>
          pull(article_id)
        
        # drop those articles from topics
        topic_df_iter <- topic_df_iter |>
          dplyr::filter(!article_id %in% topic_articles)
        
        topics_best_coverage <- append(topics_best_coverage,a)
        
        if(length(topics_best_coverage) == 5){
          # browser()
          ## can we beat 309?
          coverage <- topic_df_coverage |>
            dplyr::filter(id %in% topics_best_coverage) |>
            pull(article_id) |>
            unique() |>
            length()
          
          df <- tibble(ids = list(topics_best_coverage), coverage = coverage)
          
          coverage_df <- rbind(coverage_df,df)
          
          
          topics_best_coverage <- c()
          break()
        }
      }
      next()
    }
  }
  out <- coverage_df |>
    arrange(desc(coverage))
  
  return(out)
}
coverage_out <- find_best_coverage(topics = topics,iterations = 10000, jitter_start = 40)
coverage_out[1,"ids"][[1]][[1]]

### pull OA records for papers with those topics

works_topic <- openalexR::oa_fetch(entity = "works",
                                   topics.id = coverage_out[1,"ids"][[1]][[1]],
                                   from_publication_date = "2011-01-01",
                                   to_publication_date = "2022-12-31",
                                   options = list(
                                     select = c("authorships",
                                                "doi",
                                                "primary_location",
                                                "title"
                                     )
                                   ),
                                   output = "list"
                                   )


saveRDS(works_topic,file = "data/works_topic_list.rds")

### look at total number of unique authors in those works



### 
