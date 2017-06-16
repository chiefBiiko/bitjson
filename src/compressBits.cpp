#include <Rcpp.h>

//[[Rcpp::export]]
Rcpp::IntegerVector compressBits(const Rcpp::IntegerVector x) {

  // setup
  std::vector<int> y;                          // STL return vector
  y.reserve(x.size());                         // reserve too much
  int cnt = 0;                                 // bit count
  int prev = x[0];                             // lookbehind
  Rcpp::IntegerVector::const_iterator it;      // iterator

  // compress
  for (it = x.begin(); it != x.end(); ++it) {
    if (prev != *it) {                         // identify bit change
      if (cnt == 1) {                          // record bit run
        y.push_back(prev);
      } else {
        y.push_back(cnt);
        y.push_back(prev);
      }
      cnt = 0;                                 // reset bit count
    }
    cnt++;                                     // increment bit count
    prev = *it;                                // keep bit as lookbehind
  }

  // consume remainder
  if (cnt == 1) {
    y.push_back(prev);
  } else {
    y.push_back(cnt);
    y.push_back(prev);
  }

  // serve
  return Rcpp::wrap(y);
}
