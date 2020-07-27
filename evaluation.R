# This script contains evaluation functions

# Compute hit rate at k:
compute_hr <- function(test_pred, k){
  test_pred %>%
    group_by(user) %>%
    slice_max(order_by = pred, n = k) %>%
    summarize(hits = sum(label)) %>%
    summarize(hr = mean(hits)) 
  
}


# Compute ndcg:
compute_ndcg <- function(test_pred, k){
  
}
