
split_bill <- function(expenses) {
  expenses_agg <- expenses %>%
    unnest_longer(split_by, values_to = "share", indices_to = "owed_by") %>%
    group_by(id) %>%
    mutate(owed_amt = ifelse(paid_by != owed_by,
                             amount * share / sum(share),
                             0)) %>%
    ungroup() %>%
    group_by(paid_by, owed_by) %>%
    summarize(owed_amt = sum(owed_amt), .groups = "drop") %>%
    filter(owed_amt > 0) %>%
    select(2, 1, 3)

  # Max flow
  expenses_graph <- graph_from_data_frame(expenses_agg)
  expenses_edges <- as_edgelist(expenses_graph)
  for (i in seq(nrow(expenses_edges))) {
    from_node <- expenses_edges[i, 1]
    to_node <- expenses_edges[i, 2]
    capacity <- edge_attr(expenses_graph, "owed_amt")
    debt_flow <- max_flow(expenses_graph, from_node, to_node, capacity)
    if (debt_flow$value > 0) {
      flow <- igraph::as_data_frame(expenses_graph) %>%
        mutate(owed_amt = ifelse(from == from_node & to == to_node,
                                 debt_flow$value,
                                 owed_amt - debt_flow$flow))
      # Update expense graph
      expenses_graph <- expenses_graph %>%
        set_edge_attr(
          "owed_amt",
          value = flow$owed_amt
        )
    }
  }
  igraph::as_data_frame(expenses_graph) %>%
    filter(owed_amt > 0)
}
