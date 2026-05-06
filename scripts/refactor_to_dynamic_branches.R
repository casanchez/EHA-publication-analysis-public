source("packages.R")
 tar_read(auths_with_gender,branches = 1)

ttar_read()tar_make(contrib_keep)
tar_read(contrib_keep, branches = 1)
tar_read(contrib_keep, branches = 2)

tar_read(contributor_data,branches = 1)
tar_read(contributor_data,branches = 2)

tar_make(model_dat)

tar_make(pubs_with_focal_country)
full_auths_with_gender <- tar_load(auths_with_gender,branches = 1)

debugonce(compare_geography)
compare_geography(publication_data = publication_data,authorship_data = auths_with_gender[[2]])
authorship_data


tar_make(authorship_country_summary)
tar_read(authorship_country_summary,branches = 1)
tar_read(authorship_country_summary,branches = 2) |> View()


tar_make(gender_country_summary)
tar_read(gender_country_summary,branches = 1)
tar_read(gender_country_summary,branches = 2) |> View()

tar_make(prolific_summary)
tar_read(prolific_summary,branches = 1)
tar_read(prolific_summary,branches = 2) |> View()

targets::tar_make(mod)

targets::tar_make(authorships_histogram)
tar_read(authorships_histogram,branches = 1)
tar_read(authorships_histogram,branches = 2)

tar_make(gender_position_income_plot)
tar_read(gender_position_income_plot, branches = 1)

tar_make(propfem_position_time_plot)
tar_read(propfem_position_time_plot, branches = 1)
tar_read(propfem_position_time_plot, branches = 2)


tar_make(model_preds_plot)
tar_read(model_preds_plot, branches = 1)
tar_read(model_preds_plot, branches = 2)

tar_make(propfem_two_panel)
tar_read(propfem_two_panel, branches = 2)

tar_make(gender_position_plot)
tar_read(gender_position_plot, branches = 1)
tar_read(gender_position_plot, branches = 2)

tar_make(country_income_plot)
tar_read(country_income_plot, branches = 1)

tar_make(contrib_reduced)
tar_make(g_contrib_gender)
tar_load(g_contrib_gender)

igraph::degree_distribution(g_contrib_gender[[2]]) |>
  plot()


tar_make(degree_distribution_plot)
tar_read(degree_distribution_plot,branches = 1)


tar_make(highly_connected_individuals)
tar_read(highly_connected_individuals)

tar_make(degree_centrality)
tar_read(degree_centrality)         
