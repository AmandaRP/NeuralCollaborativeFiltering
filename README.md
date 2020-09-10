Neural Collaborative Filtering
================

This is an implementation of Neural Collaborative Filtering (NCF) using
[R Keras](https://keras.rstudio.com/index.html). The model (a deep
neural network for binary implicit feedback) is described in the
following paper and implemented by its authors using Keras (see [this
GitHub
repo](https://github.com/hexiangnan/neural_collaborative_filtering)).

> Xiangnan He, Lizi Liao, Hanwang Zhang, Liqiang Nie, Xia Hu and
> Tat-Seng Chua (2017). [Neural Collaborative
> Filtering](https://dl.acm.org/doi/10.1145/3038912.3052569). In
> Proceedings of WWW ’17, Perth, Australia, April 03-07, 2017.

## Examples

1.  Two datasets are provided by the NCF paper authors in their [GitHub
    repo](https://github.com/hexiangnan/neural_collaborative_filtering/tree/master/Data):
    Movie Lense and Pinterest, both of which are described in detail in
    their paper. See the `ml_pinterest.R` script in this repo. Choose
    between the two datasets via the `movielense` flag.
2.  Coming soon: Book recommendations from GoodReads
