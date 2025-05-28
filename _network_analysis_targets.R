network_targets <- tarchetypes::tar_plan(
  
  # get author and contributor data ####
  # reduce contributor data 
  tar_target(contrib_reduced, contributor_data %>% 
               dplyr::select(contributor_id,
                             gender_final,
                             nqg_classification,
                             institution_display_name,
                             institution_country_code) %>% 
               dplyr::mutate(contributor_id = as.character(contributor_id))
  ),
  
  # get primary affiliations
  tar_target(primary_affiliation, get_primary_affiliations(contrib_reduced)),
  tar_target(df_contrib, get_primary_country(primary_affiliation)),
  
  # reduce authorship dataframe
  tar_target(df_auth, authorship_data %>% 
               dplyr::select(all_of(c("contributor_id", "publication"))) %>% 
               dplyr::mutate(contributor_id = as.character(contributor_id)) %>% 
               dplyr::filter(!is.na(contributor_id))
  ),
  
  # make graph object ####
  tar_target(g_contrib_gender, make_graph_object(df_auth,df_contrib)),
  
  # basic graph attributes ####
  
  ## degree distribution 
  degree_distribution = plot(igraph::degree_distribution(g_contrib_gender)),
  
  # most people (80%) are publishing as first or last author with exactly 1 other person"
  
  tar_target(highly_connected_individuals, 
             get_highly_connected_individuals(g_contrib_gender)),
  
  # 87/498  contributors are publishing first or last author papers with 2 or more other contributors"
  
  ### How centralized is the graph based on a measure of degree - range 0-1 where
  ## 0 is maximally decentralized 1 is fully centralized
  degree_centrality = igraph::centr_degree(g_contrib_gender),
  
  # 0.065 is highly decentralized based on degree measures of centrality"
  
  btw_centrality =  igraph::centr_betw(g_contrib_gender,directed = FALSE),
  
  # "0.084 is a highly decentralized graph based on betweenness 
  # (how many nodes lie on shortest paths) measures of centrality"
  
  
  ## add centrality measures to vertices for plotting ####
  g_contrib_gender_btw = igraph::set_vertex_attr(
    g_contrib_gender,
    name = "betweenness",
    value = betweenness(graph = g_contrib_gender, directed = FALSE)),
  
  g_contrib_gender_btw_deg = igraph::set_vertex_attr(
    g_contrib_gender_btw,
    name = "degree",
    value =  igraph::degree(graph = g_contrib_gender, v = V(g_contrib_gender),
                            loops = FALSE)),
  
  
  g_contrib_gender_btw_deg_hc = igraph::set_vertex_attr(
    g_contrib_gender_btw_deg,
    name = "harmonic_centrality",
    value =  igraph::harmonic_centrality(graph = g_contrib_gender,
                                         vids = V(g_contrib_gender))),
  
  ##create plotting df ####
  tar_target(g_fortified, make_fortified_igraph_obj(g_contrib_gender_btw_deg_hc)),
  
  # network plots ####
  
  tar_target(gender_network_plot, 
             basic_network_plot(g_fortified, nodes_color = "gender") +
               scale_color_manual(values = gender_colors)),
  tar_target(primary_country_network_plot, 
             basic_network_plot(g_fortified, nodes_color = "primary_country")),
  tar_target(country_label_network_plot, 
             basic_network_plot(g_fortified, nodes_color = "country_label")),
  
  ## harmonic centrality #######
  tar_target(gender_harmonic_cent_plot, 
             ggplot(g_fortified, aes(x = x, y = y, xend = xend, yend = yend)) +
               geom_edges(color = "black", lwd = 0.2) +
               geom_nodes(aes(color = gender, size = harmonic_centrality)) +
               scale_size(name = "Harmonic \ncentrality",
                          range = c(0.25, 4), 
                          breaks = c(0, 1, 10, 25, 50)) +
               ggnetwork::theme_blank() +
               scale_color_manual(name = "Gender", 
                                  labels = c("Gendered \nfemale",  
                                             "Gendered \nmale", 
                                             "Gendered \nnonbinary", 
                                             "Unknown"),
                                  values = gender_colors) +
               ggnetwork::theme_blank() +
               theme(legend.title = element_text(color = "black", size = 10),
                     legend.text = element_text(color = "black", size = 8))),
  
  harmonic_cent_hist = g_fortified %>% 
    distinct(name, .keep_all = TRUE) %>% 
    ggplot() +
    geom_histogram(aes(x = harmonic_centrality, fill = gender), binwidth = 1) +
    theme_bw() +
    scale_fill_manual(values = gender_colors),
  
  # what is the 95th percentile for harmonic centrality
  hc_95 = quantile(unname(unlist(g_fortified %>% 
                                     distinct(name, .keep_all = T) %>% 
                                     select(harmonic_centrality))),
                     probs = 0.95),
  
  gender_high_cent_table = g_fortified %>% 
    distinct(name, .keep_all = TRUE)  %>% 
    filter(harmonic_centrality >= hc_95) %>% 
    group_by(gender) %>% 
    tally(),
  
  female_har_cent = gender_high_cent_table %>% 
    filter(gender == "gendered female") %>% 
    pull(),
  male_har_cent = gender_high_cent_table %>% 
    filter(gender == "gendered male") %>% 
    pull(),
  nb_har_cent = gender_high_cent_table %>% 
    filter(gender == "nonbinary") %>% 
    pull(),
  
  
  
  ## betweenness #####
  gender_betweenness_df = g_fortified %>% 
    distinct(name, .keep_all = TRUE) %>% 
    group_by(gender) %>% 
    summarize(group_size = n(),
              median_btw = median(betweenness, na.rm = TRUE),
              mean_btw = mean(betweenness, na.rm = TRUE),
              hdci_btw_low = ggdist::hdci(betweenness)[1],
              hdci_btw_high = ggdist::hdci(betweenness)[2],
              median_hc = median(harmonic_centrality, na.rm = TRUE),
              mean_hc = mean(harmonic_centrality, na.rm = TRUE),
              hdci_hc_low = ggdist::hdci(harmonic_centrality)[1],
              hdci_hc_high = ggdist::hdci(harmonic_centrality)[2],
    ),
  
  ## TABLE 1 ##
  gender_betweenness_table = gender_betweenness_df %>% 
    kableExtra::kable(digits = 2) %>% 
    kableExtra::kable_material(),
  
  
  tar_target(g_btwn_normal, normalize_data(g_fortified,col = "betweenness")),
  
  gender_betweenness_plot = ggplot(
    g_btwn_normal, 
    aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_edges(color = "black", lwd = 0.2) +
    geom_nodes(aes(color = gender, size = betweenness)) +
    scale_size(name = "Betweenness \ncentrality", range = c(0.5, 8), 
               breaks = c(0, 50, 500, 5000, 10000)) +
    scale_color_manual(name = "Gender", 
                       labels = c("Gendered \nfemale",  "Gendered \nmale", 
                                  "Gendered \nnonbinary", "Unknown"),
                       values = gender_colors) +
    ggnetwork::theme_blank() +
    theme(legend.title = element_text(color = "black", size = 10),
          legend.text = element_text(color = "black", size = 8)),
  
  
  ## add to caption in figure
  
  # what is the cutoff for 95th percentile of betweenness?
  btwn_95 = quantile(unname(unlist(g_fortified %>% 
                           distinct(name, .keep_all = T) %>% 
                           select(betweenness))),
                     probs = 0.95),
  
  gender_high_betweenness_table = g_fortified %>% 
    distinct(name, .keep_all = TRUE)  %>% 
    filter(betweenness >= btwn_95) %>% 
    group_by(gender) %>% 
    tally(),
  
  male_betweenness  = gender_high_betweenness_table %>% 
    filter(gender == "gendered male") %>% 
    pull(),
  
  female_betweenness  = gender_high_betweenness_table %>% 
    filter(gender == "gendered female") %>% 
    pull(),
  
  networks_plot = plot_two_panel(gender_betweenness_plot,
                                 gender_harmonic_cent_plot, 
                                 ncols = 1, nrows = 2,
                                 alignment = "none", align_axis = "none")
  
  
)




