import numpy as np
import pandas as pd



def samp_impl_neg( N, p, excludes=[] ):
    item_indices = np.arange(0,int(N))
    return np.random.choice( np.setdiff1d(item_indices, excludes), size=int(p) )

# Input: 
#   userIDs: array containing unique user IDs
#   itemIDs: array containing unique item IDs
#   num_ratings_2sample: vector specifying number of implicit negatives to samples for each user. ith entry should correspond to ith entry in UserIDs vector.
#   df_exclude: dataframe containing user-item pairs that are already assigned (to datasets such as training, validation, and/or test)
# TODO: Make df_exclude an optional argument
# TODO: Add an optional probability vector for each item (could be used to assign higher probability to popular items)
def sample_implicit_negatives(userIDs, itemIDs, num_ratings_2sample, df_exclude):

  # group the pandas df by user
  df_gpd = train.groupby("user")

  # iterate to create positive lists
  df_users = [ (user,np.array(pd.DataFrame(df_small).item, dtype=int)) for (user,df_small) in df_gpd]

  sampled_negative = [ (u, samp_impl_neg(r.num_items, len(its)*r.neg_pos_ratio_train, excludes=its)) for (u,its) in df_users ]
  
  return sampled_negatives
