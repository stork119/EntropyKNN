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

int main()
{
    int k = 6;
    vector< vector<double> > data;
    ifstream file("../../Resources/data1.csv");

    string line;
    string partial;

    vector<string> tokens;
    int i = 0;
    while(getline(file, line) && i < 5000) 
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
    
   vector<double> data_k_neighbour(data.size(), 0.0);

   
   cout << "Start computations" << '\t' << data.size() << endl;
   /* First version */ 
   clock_t t = clock();
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
      cout << "Finished computations" << endl; 
  double s = (double) (clock() - t)/CLOCKS_PER_SEC;
    ofstream myfile;
    myfile.open ("logs/times.log");
    myfile << s << endl;
    myfile.close();

    ofstream myfile2;  
    myfile2.open ("logs/results.out");
    for(int data_i = 0; data_i != data.size(); ++data_i)
    {
      myfile2 << data_k_neighbour[data_i] << endl;
    }  
   myfile2.close();



   return 0;

}

