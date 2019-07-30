#include <Rcpp.h>
using namespace Rcpp;

// A version of the .getMode function from the CRAN "raster" package, that
// avoids casing both in R and CPP, making repeated calls to the function, such
// as when aggregating, a little bit quicker (around 33%).

// [[Rcpp::export]]
double quickmode(NumericVector values) {
	int n = values.length();
   IntegerVector counts(n);

   int tieCount = 1;
   int maxCount = 0;
   for (int i = 1; i < n; ++i) {
      if (counts[i] > counts[maxCount]) {
         maxCount = i;
         tieCount = 1;
      } else if (counts[i] == counts[maxCount]) {
         tieCount++;
         if (R::runif(0,1) < (1.0 / tieCount)) {
            maxCount = i;
         }			
      }
   }
   return values[maxCount];
}

