source("packages.R")


## given the nature of openalex data and infrastructure, we cannot guarantee
## that these results will be perfectly reproducible but this code
## was used to retrieve the openalex data and create outputs. 
## Our goal with this script is to understand the journals EHA is most often
## published in. This script involves some manual curation of openalex outputs 
## where the original data queried from openalex no longer matches the present
## state of the dataset/infrastructure. 

# load authorship data
targets::tar_make(auths_with_gender)
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
works$data_frame = "works"


### look at works not retrieved from open alex THEN manually list proper DOI or
# Open alex id for the work
works_not_oa <- dplyr::anti_join(dois,works,by = c("identifier" = "doi"))
works_not_oa

## most of these works had mismatching DOIs between the ids object and the DOI field.
## use DOI in ids OBJECT for better retrieval

## could not find https://ijisr.issr-journals.org/abstract.php?article=IJISR-18-353-04
works_not_oa_df <- data.frame(identifier = c(
  # "https://doi.org/10.7916/1g9a-gs78",
  # "https://doi.org/10.7916/d8qr4vkm",
  # "https://doi.org/10.5167/uzh-203946",
  # "https://doi.org/10.17863/cam.39009",
  # "https://doi.org/10.7916/d8cz37kr",
  "https://doi.org/10.5455/javar.2016.c147", # keep
  "https://doi.org/10.1128/jvi.01059-18", # keep
  # "https://doi.org/10.3929/ethz-b-000493866",
  "https://doi.org/10.5455/javar.2016.c153", #keep
  # "https://doi.org/10.7916/d8vd8fmd",
  # "https://doi.org/10.5167/uzh-141192",
  # "https://doi.org/10.17863/cam.65556",
  # "https://doi.org/10.7916/d87942n5",
  "https://doi.org/10.5455/javar.2016.c181" #keep
  ),
  oa_id = c("https://openalex.org/W2804947704") #keep
)


# get works based on openalex id
works_updated_oa_id <- openalexR::oa_fetch(entity = "works",
                                     ids.openalex = works_not_oa_df$oa_id,
                                     options = list(select = c("doi","topics",
                                                               "concepts",
                                                               "authorships",
                                                               "primary_location")))


works_updated_oa_id$original_identifer <- "https://www.ajol.info/index.php/tjs/article/view/171309"
works_updated_oa_id$data_frame = "oa"

## this is pulling from proper dois as published in the original work
works_updated_oa_doi <- openalexR::oa_fetch(entity = "works",
                                           doi = works_not_oa_df$identifier,
                                           options = list(select = c("doi",
                                                                     "topics",
                                                                     "concepts",
                                                                     "authorships",
                                                                     "primary_location")))
works_updated_oa_doi$data_frame = "doi"
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

## look for duplicates

dupes <- works_complete_for_publication_table$original_identifer |> duplicated()

works_complete_for_publication_table$dupes <- dupes

dupe_dois <- works_complete_for_publication_table |>
  dplyr::filter(dupes) |>
  dplyr::pull(original_identifer)


works_complete_for_publication_table |>
  dplyr::filter(original_identifer %in% dupe_dois) |>
  View()

### 

original_works_with_count <- readr::read_csv(file = "dois/works_with_count.csv")

readr::write_csv(works_complete_for_publication_table,"dois/works_with_count.csv")

works_complete <- works_complete|>
  dplyr::mutate(article_id = row_number())


