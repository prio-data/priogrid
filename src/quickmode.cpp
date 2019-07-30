#include <Rcpp.h>
using namespace Rcpp;

// A version of the .getMode function from the CRAN "raster" package, that
// avoids casing both in R and CPP, making repeated calls to the function, such
// as when aggregating, a little bit quicker (around 33%).

// [[Rcpp::export]]
double quickmode(NumericVector values) {
	int n = values.length();
   IntegerVector counts(n);


   // Loop over values, counting them
   for (int i = 0; i < n; ++i) {
      counts[i] = 0;
      int j = 0;
      while ((j < i) && (values[i] != values[j])) {
         ++j;
      }
      ++(counts[j]);
   }

   int maxCount = 0;

   // Return value at index with max count (last)
   for(int i = 1; i < n; ++i){
      if(counts[i] > counts[maxCount]){
         maxCount = i;
      }
   }
   return values[maxCount];
}

