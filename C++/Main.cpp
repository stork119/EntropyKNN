#include <vector>
#include <string>

#include <limits>
#include <algorithm>
#include <cmath>
#include <time.h>

#include <sstream>
#include <fstream>
#include <iostream>

using namespace std;


vector<double> get_k_neighbour_1(vector< vector< double > > data, int k)
{
   vector<double> data_k_neighbour(data.size(), 0.0);
   for(int data_i = 0; data_i != data.size(); ++data_i)
   {
   
       vector<double> temp(data.size(), 0.0);
       for(int col_i = 0; col_i != data[data_i].size(); ++col_i){
           double Y = data[data_i][col_i];
           for(int row_i = 0; row_i != data.size(); ++row_i)
           {
               temp[row_i] = max(temp[row_i], abs(Y - data[row_i][col_i]));
           }
       }
       
       std:sort(temp.begin(), temp.end());
       data_k_neighbour[data_i] = temp[k];
   }
   return data_k_neighbour;
}

vector<double> get_k_neighbour_2(double** data, int k, int data_nrow, int data_ncol)
{
   vector<double> data_k_neighbour(data_nrow, 0.0);
   for(int data_i = 0; data_i != data_nrow; ++data_i)
   {
       vector<double> temp(data_nrow, 0.0);
       for(int col_i = 0; col_i != data_ncol; ++col_i){
           double Y = data[data_i][ col_i];
           for(int row_i = 0; row_i != data_nrow; ++row_i)
           {
               temp[row_i] = max(temp[row_i], abs(Y - data[row_i][ col_i]));
           }
       }
       std:sort(temp.begin(), temp.end());
       data_k_neighbour[data_i] = temp[k];
   }
   return data_k_neighbour;
}



int main(int argc, char* argv[])
{
    int k = atoi(argv[1]);
    int i_limit = 0;
    if(argc > 2){
        i_limit = atoi(argv[2]);
    }
    vector< vector<double> > data;
    ifstream file("../../Resources/data1.csv");

    string line;
    string partial;

    int i = 0;
    while(getline(file, line) && (i < i_limit || i_limit == 0)) 
    {     // '\n' is the default delimiter
        istringstream iss(line);
        string token;
        vector<double> d;
        while(getline(iss, token, '\t')){
            // but we can specify a different one
            //
            d.push_back(stod(token));
        }
       data.push_back(d);
       ++i;
    }
    
   double** data_arr = new double*[data.size()];
   for(int data_i = 0; data_i != data.size(); ++data_i)
   {
       data_arr[data_i] = new double[data[0].size()];
       for(int data_j = 0; data_j != data[0].size(); ++data_j)
       {
           data_arr[data_i][data_j] = data[data_i][data_j];
       }
   }

   clock_t t;
   ofstream myfile, myfile2;

   /** first version **/
   cout << "Start computations 1" << '\t' << data.size() << endl;
   t = clock();
   vector<double> data_k_neighbour =  get_k_neighbour_1(data, k); /* First version */ 
   cout << "Finished computations 1" << endl; 
   double s = (double) (clock() - t)/CLOCKS_PER_SEC;
    
   myfile.open ("logs/times_1.log");
   myfile << s << endl;
   myfile.close();

   myfile2.open ("logs/results_1.out");
   for(int data_i = 0; data_i != data.size(); ++data_i)
   {
      myfile2 << data_k_neighbour[data_i] << endl;
   }  
   myfile2.close();

   /** second version **/
   cout << "Start computations 1" << '\t' << data.size() << endl;
   t = clock();
   vector<double> data_k_neighbour_2 =  get_k_neighbour_2(data_arr, k, data.size(), data[0].size());  
   cout << "Finished computations 2" << endl; 
   s = (double) (clock() - t)/CLOCKS_PER_SEC;
    
   myfile.open ("logs/times_2.log");
   myfile << s << endl;
   myfile.close();

   myfile2.open ("logs/results_2.out");
   for(int data_i = 0; data_i != data.size(); ++data_i)
   {
      myfile2 << data_k_neighbour_2[data_i] << endl;
   }  
   myfile2.close();


   return 0;

}

