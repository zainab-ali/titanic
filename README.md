# Titanic

[![Build Status](https://travis-ci.org/zainab-ali/titanic.svg?branch=master)](https://travis-ci.org/zainab-ali/titanic)

## Aim

To predict whether a person would have lived or died on the Titanic.

## The dataset

This project contains a dataset of passengers on the Titanic.  It can be used to assess different predictive models of survival.

The dataset has been obtained from http://biostat.mc.vanderbilt.edu/DataSets.
The full dataset is located in `original.csv`. It has been randomly shuffled into training and validation data `train.csv` and test data `test.csv`.

Predictive models should be trained and validated on the training data before being tested on the test data.


## The problem

This problem is stochastic in nature, so is *inconsistent*.  We cannot expect to find a model which correctly predicts all of training points.

## Hypotheses

The project currently contains four hypothesis classes:

1. Everyone Dies
2. Females survive
3. Greedy decision trees
4. Pruned decision trees

These hypotheses can be run using the `titanic.app.HypothesisRunner`.
