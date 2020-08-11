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

## To start

Run the code in “run.R”– it will source the necessary scripts, fit the
model, and provide evaluation metrics.

## Data

Two datasets are provided by the NCF paper authors on their [GitHub
repo](https://github.com/hexiangnan/neural_collaborative_filtering/tree/master/Data):
Movie Lense and Pinterest, both of which are described in detail in
their paper. The “readData.R” scipt (which is called by “run.R”) will
read the data. You can choose between the two datasets via the
`movielense` flag in “run.R”.