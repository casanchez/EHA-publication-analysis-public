################################################################################
#
# Project build script
#
################################################################################

# Load packages (in packages.R) and load project-specific functions in R folder
suppressPackageStartupMessages(source("packages.R"))
for (f in list.files(here::here("R"), full.names = TRUE)) source (f)
source("_network_analysis_targets.R")


# Set build options ------------------------------------------------------------


# Groups of targets ------------------------------------------------------------

## Data input
data_input_targets <- tar_plan(
  
  
  #de-identified data were created using scripts/deidentify_data.R 
  
  ## get from zenodo directly
  # https://zenodo.org/api/records/15226626/files-archive
  tar_file(contributors_csv, "data/deidentified_contributors.csv"),
  tar_file(authorship_csv, "data/deidentified_authorship.csv"),
  tar_file(publications_csv, "data/deidentified_publications.csv"),

  # https://datatopics.worldbank.org/world-development-indicators/the-world-by-income-and-region.html
  tar_file(income_data_xlsx, "data/OGHIST.xlsx"),
  
  contributor_data_raw = readr::read_csv(contributors_csv),
  authorship_data_raw = readr::read_csv(authorship_csv),
  publication_data_raw = readr::read_csv(publications_csv),
  income_data_raw = readxl::read_excel(income_data_xlsx, 
                                 sheet = "Country Analytical History"),
  
  # color palettes from github.com/BlakeRMills/MetBrewer
  gender_colors = met.brewer(name = "Java", n = 5)[c(1,5,3,2,4)],
  income_colors = met.brewer(name = "Hokusai3", n = 6)[c(4:1)],
  position_colors = met.brewer(name = "Signac", n = 14)[c(9,11)]
  
)


## Data processing
data_processing_targets <- tar_plan(
  
  income_data_clean = clean_income_data(income_data_raw),
  authorship_data = clean_authorship_data(authorship_data_raw, income_data_clean),
  contributor_data_intermed = clean_contributor_data(contributor_data_raw),
  auths_with_gender = add_gender_to_auths(contributor_data_intermed, 
                                          authorship_data),

  contrib_keep = unique(auths_with_gender$contributor_id),

  contributor_data = contributor_data_intermed %>%
    filter(contributor_id %in% contrib_keep),
  publication_data = clean_publication_data(publication_data_raw),

  # geographic comparison of publication focus vs authorship affiliations
  pubs_with_focal_country = compare_geography(publication_data,
                                              authorship_data),

  model_dat = prep_model_dat(auths_with_gender)
  
)


## Analysis
analysis_targets <- tar_plan(
  
  ## Gender by authorship position breakdown ----
  tar_target(gender_position_summary, 
             summarize_gender_by_position(auths_with_gender)),
  
  ## Geographic breakdown ----
  tar_target(authorship_country_summary,
             summarize_by_country(authorship_data, income_data_clean)),

  ## Gender by country breakdown ----
  tar_target(gender_country_summary,
             summarize_gender_by_country(auths_with_gender, income_data_clean)),

  ## income by authorship position breakdown ----
  tar_target(income_position_summary,
             summarize_income_by_position(auths_with_gender)),

  ## Gender over time by income ----
  tar_target(gender_income_time_summary,
             summarize_gender_income_over_time(auths_with_gender)),

  ## Gender over time by authorship position ----
  tar_target(gender_position_time_summary,
             summarize_gender_position_over_time(auths_with_gender)),

  ## Authorships by contributor and gender ----
  tar_target(authorships_contributor_gender_summary,
             summarize_authorships_contributor_gender(auths_with_gender)),
  
  # look at last authorships within highly productive authors
  tar_target(prolific_summary,
             examine_prolific(auths_with_gender, prolific_cutoff = 10)),

  ## Authorships by gender, position, income ----
  tar_target(gender_position_income_summary,
             summarize_gender_position_income(auths_with_gender)),
  
  # run model
  mod = lm(perc_female ~ year_centered*authorship_position + 
              authorship_position*income_majority, data = model_dat)
  
)

## Outputs
outputs_targets <- tar_plan(
  

  ## Authorships by contributor and gender (FIG 1)
  tar_target(authorships_histogram,
             plot_authorships_contributor_gender(
               authorships_contributor_gender_summary, gender_colors)),

  ## Authorships by gender, position, income (FIG 2)
  tar_target(gender_position_income_plot,
             plot_gender_position_income(gender_position_income_summary,
                                       gender_colors)),
  

  tar_target(propfem_position_time_plot,
             plot_propfem_position_over_time(gender_position_time_summary, 
                                             position_colors)),
  tar_target(model_preds_plot,
             plot_model_preds(mod, income_colors, gender_colors)),
  
  # proportion female over time by position, and model preds (FIG 3)
  tar_target(propfem_two_panel,
             plot_two_panel(propfem_position_time_plot, model_preds_plot, 
                            ncols = 2, nrows = 1, 
                            alignment = "h", align_axis = "hv")),
    
    
  ## gender/position plots (FIG S1)
  tar_target(gender_position_plot,
             plot_gender_by_position(gender_position_summary, gender_colors)),
  
  ## geographic plots (FIG S2)
  tar_target(country_income_plot,
             plot_by_country(authorship_country_summary,
                             variable = "country.name.en",
                             income_colors)),
  
  ## gender by country plot (FIG S3)
  tar_target(gender_country_plot,
             plot_gender_by_country(gender_country_summary, gender_colors)),
  
  ## income by authorship position plot (FIG S4)
  tar_target(income_position_plot,
             plot_income_by_position(income_position_summary, income_colors))
  
)

## plotting
plotting_targets <- tar_plan(
  
  # Figure 1
  tar_target(Fig1_tif,
             ggsave("figures/Fig1.tiff", authorships_histogram, 
                    height = 5.4, width = 5.2,
                    units = "in", dpi = 600,
                    compression = "lzw")),
  
  # Figure 2
  tar_target(Fig2_tif,
             ggsave("figures/Fig2.tiff", gender_position_income_plot, 
                    height = 5.2, width = 5.2,  
                    units = "in", dpi = 600,
                    compression = "lzw")),
  
  # Figure 3
  tar_target(Fig3_tif,
             ggsave("figures/Fig3.tiff", propfem_two_panel, 
                    height = 3, width = 5.2,  
                    units = "in", dpi = 600,
                    compression = "lzw")),
  
  # Figure 4
  tar_target(Fig4_tif,
             ggsave("figures/Fig4.tiff", networks_plot, 
                    height = 8, width = 5.2,  
                    units = "in", dpi = 600,
                    compression = "lzw")),
  
  # Figure S1
  tar_target(FigS1_tif,
             ggsave("figures/FigS1.tiff", gender_position_plot, 
                    height = 6, width = 6,  
                    units = "in", dpi = 600,
                    compression = "lzw")),
  
  # Figure S2
  tar_target(FigS2_tif,
             ggsave("figures/FigS2.tiff", country_income_plot, 
                    height = 9, width = 7,  
                    units = "in", dpi = 600,
                    compression = "lzw")),
  
  # Figure S3
  tar_target(FigS3_tif,
             ggsave("figures/FigS3.tiff", gender_country_plot, 
                    height = 6, width = 7,  
                    units = "in", dpi = 600,
                    compression = "lzw")),
  
  # Figure S4
  tar_target(FigS4_tif,
             ggsave("figures/FigS4.tiff", income_position_plot, 
                    height = 6, width = 8,  
                    units = "in", dpi = 600,
                    compression = "lzw")),
)


## Report
report_targets <- tar_plan(

  tar_render(
    main_text, path = "reports/main_text.Rmd",
    output_dir = "reports", knit_root_dir = here::here()
  )
)

# List targets -----------------------------------------------------------------

list(
  data_input_targets
  ,data_processing_targets
  ,analysis_targets
  ,outputs_targets
  ,plotting_targets
  ,report_targets
  ,network_targets
)