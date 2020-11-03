import numpy as np
import pandas as pd

def p_rebalance( p_broke ):
    # make p sum to one, proportionally
    return p_broke/p_broke.sum()

# Helper function:
def samp_impl_neg(item_ids , n, excludes=[] , num_batches, p = None):
    #mod the p vector for excludes
    if p:
        item_indices = np.isin( item_ids, excludes ) # find the indices of the excludes
        p_unbalanced = p[ ~item_indices ]
        p = p_unbalanced/p_unbalanced.sum()
        items_to_sample = np.setdiff1d(item_ids, excludes)
    for i in range(0, num_batches):
        TODO = np.random.choice(items_to_sample , size=int(n), p=p , replace = False) 
    return TODO

# Input: 
#   user_ids: vector containing unique user IDs
#   item_ids: vector containing unique item IDs
#   num_ratings_to_sample: vector specifying number of implicit negatives to samples for each user. ith entry should correspond to ith entry in user_ids vector.
#   df_exclude: dataframe containing user-item pairs that are already assigned (to datasets such as training, validation, and/or test). 
#               Must include "user" and "item" columns
#  num_batches: Use for multiple "batches" of samples. Useful for multiple epochs.
#  p: probability vector. Same length as item_ids. Gives sampling probability for each item.
# TODO: Add an optional probability vector for each item (could be used to assign higher probability to popular items)
def sample_implicit_negatives(user_ids, item_ids, num_items_to_sample, df_exclude, num_batches = 1, p = None):
    # dictionary for user ids and associated number of items to sample
    d = {k:v for (k,v) in zip(user_ids, num_items_to_sample)}
    # group the pandas df by user
    gpd = df_exclude.groupby("user")
    # iterate to create exclude lists
    user_exclude_items = [ (user, np.array(pd.DataFrame(df_small).item, dtype=int)) for (user, df_small) in gpd if user in d.keys()]
    implicits = [ (user, samp_impl_neg(item_ids, d[user], excludes=items)) for (user, items) in user_exclude_items]
    df = pd.DataFrame(implicits, columns = ["user", "item"])
    return df.explode("item", ignore_index = True)
    #TODO: See what class the item column is. Seems to be a list type.
