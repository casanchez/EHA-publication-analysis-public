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

works <- openalexR::oa_fetch(entity = "works",doi = dois$identifier,options = list(select = c("topics,concepts")))

works <- works|>
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





topics <- purrr::map2_df(works$topics,works$article_id, function(x,y){
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
for(a in topic_count$id){
  
  if(a %in% topics_with_count$id){
    topic_articles <- topics_with_count |>
      dplyr::filter(id == a)|>
      pull(article_id)
    
    # drop those articles from topics
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
coverage_out



### pull OA records for papers with those topics

### look at total number of unique authors in those works