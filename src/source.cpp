#include <Rcpp.h>

#include <iostream>
#include <vector>
#include <algorithm>
#include <limits>
#include <cmath>

using namespace::Rcpp;

// [[Rcpp::export]]
std::vector<double> get_k_neighbour_1(std::vector< std::vector< double > > data, int k)
{
    std::vector<double> data_k_neighbour(data.size(), 0.0);
    for(int data_i = 0; data_i != data.size(); ++data_i)
    {
        std::vector<double> temp(data.size(), 0.0);
        for(int col_i = 0; col_i != data[data_i].size(); ++col_i){
            double Y = data[data_i][col_i];
            for(int row_i = 0; row_i != data.size(); ++row_i)
            {
                temp[row_i] = std::max(temp[row_i], std::abs(Y - data[row_i][col_i]));
            }
         }
         std::sort(temp.begin(), temp.end());
         data_k_neighbour[data_i] = temp[k];
    }
    return data_k_neighbour;
}


// [[Rcpp::export]]
std::vector<double> get_k_neighbour_2(NumericMatrix data, int k)
{
   std::vector<double> data_k_neighbour(data.nrow(), 0.0);
   for(int data_i = 0; data_i != data.nrow(); ++data_i)
   {
       std::vector<double> temp(data.nrow(), 0.0);
       for(int col_i = 0; col_i != data.ncol(); ++col_i){
           double Y = data(data_i,col_i);
           for(int row_i = 0; row_i != data.nrow(); ++row_i)
           {
               temp[row_i] = std::max(temp[row_i], std::abs(Y - data(row_i,col_i)));
           }
       }
       std::sort(temp.begin(), temp.end());
       data_k_neighbour[data_i] = temp[k];
   }
   return data_k_neighbour;
}

// [[Rcpp::export]]
NumericVector get_k_neighbour_2_1(NumericMatrix data, int k)
{
  NumericVector data_k_neighbour(data.nrow());
  for(int data_i = 0; data_i != data.nrow(); ++data_i)
  {
    std::vector<double> temp(data.nrow(), 0.0);
    for(int col_i = 0; col_i != data.ncol(); ++col_i){
      double Y = data(data_i,col_i);
      for(int row_i = 0; row_i != data.nrow(); ++row_i)
      {
        temp[row_i] = std::max(temp[row_i], std::abs(Y - data(row_i,col_i)));
      }
    }
    std::sort(temp.begin(), temp.end());
    data_k_neighbour[data_i] = temp[k];
  }
  return data_k_neighbour;
}
// [[Rcpp::export]]
std::vector<double> get_k_neighbour_3(NumericMatrix data, int k)
{
  /*
   * cols -- variables
   * rows -- parameters
   */
  std::vector<double> data_k_neighbour(data.ncol(), 0.0);
  for(int data_i = 0; data_i != data.ncol(); ++data_i)
  {
    std::vector<double> temp(data.ncol(), 0.0);
    for(int row_i = 0; row_i != data.nrow(); ++row_i){
      double Y = data(row_i, data_i);
      for(int col_i = 0; col_i != data.ncol(); ++col_i)
      {
        temp[col_i] = std::max(temp[col_i], std::abs(Y - data(row_i,col_i)));
      }
    }
    std::sort(temp.begin(), temp.end());
    data_k_neighbour[data_i] = temp[k];
  }
  return data_k_neighbour;
}

/*
 * Inne pomysły to wektory w różnych konfiguracjach
 * Sprawdzić Parallelność
 */

/*
 * Euclidean
 */

// [[Rcpp::export]]
std::vector<double> get_k_neighbour_euclidean(NumericMatrix data, int k)
{
  std::vector<double> data_k_neighbour(data.nrow(), 0.0);
  for(int data_i = 0; data_i != data.nrow(); ++data_i)
  {
      std::vector<double> temp(data.nrow(), 0.0);
      for(int row_i = 0; row_i != data.nrow(); ++row_i)
      {
        for(int col_i = 0; col_i != data.ncol(); ++col_i){
          temp[row_i] += std::abs(data(data_i,col_i)*data(data_i,col_i) - data(row_i,col_i)*data(row_i,col_i));
        }
      }
     std::sort(temp.begin(), temp.end());
     data_k_neighbour[data_i] = temp[k];
  }
  return data_k_neighbour;
}


/**
 * Check if equal 
 */
// [[Rcpp::export]]
int check_equality(NumericMatrix data, double delta)
{
  if(data.nrow() == 1)
  {
    return 0;
  }
  std::vector<double> data_k_neighbour(data.nrow(), 0.0);
  for(int data_i = 0; data_i != data.nrow() - 1; ++data_i)
  {
    std::vector<double> temp(data.nrow() - 1 - data_i, 0.0);
    for(int col_i = 0; col_i != data.ncol(); ++col_i){
      double Y = data(data_i,col_i);
      for(int row_i = data_i + 1; row_i != data.nrow(); ++row_i)
      {
        temp[row_i - 1 - data_i] = std::max(temp[row_i- 1 - data_i],
                                            std::abs(Y - data(row_i,col_i)));
      }
    }
    for(std::vector<double>::iterator it = temp.begin(); it != temp.end(); ++it)
    {
      if(*it < delta)
      {
        return 0;
      }
    }
  }
  return 1;
}

