Neural Collaborative Filtering
================

## Neural Collaborative Filtering (NCF)

This is an implementation of NCF using [R
Keras](https://keras.rstudio.com/index.html). The model is described in
the following paper and implemented by its authors using Keras (see
[this GitHub
repo](https://github.com/hexiangnan/neural_collaborative_filtering)).

> Xiangnan He, Lizi Liao, Hanwang Zhang, Liqiang Nie, Xia Hu and
> Tat-Seng Chua (2017). [Neural Collaborative
> Filtering](https://dl.acm.org/doi/10.1145/3038912.3052569). In
> Proceedings of WWW ’17, Perth, Australia, April 03-07, 2017.

## To start

Run the code in “run.R”– it will source the necessary scripts, fit the
model, and provide evaluation metrics. You can choose between the Movie
Lense dataset or a Pinterest dataset.

## Data

Data is provided by the NCF paper authors on their [GitHub
repo](https://github.com/hexiangnan/neural_collaborative_filtering/tree/master/Data).
They provide two datasets: Movie Lense and Pinterest, which is described
in their paper. The “readData.R” scipt will read these datasets. read
this data.
