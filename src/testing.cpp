#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericVector getNum()
{
	NumericVector y = 2;
	return y ;
}
