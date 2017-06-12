#include <Rcpp.h>

//[[Rcpp::export]]
int marshal(Rcpp::IntegerVector x) {  // Integer or Numeric faster? -> Numeric

  // setup
  std::vector<int> y;                          // STL return vector
//y.reserve(x.size());  // reserve too much then shrink_to_fit ?
  int cnt = 0;                                 // bit count
  int prev = x[0];                             // lookbehind
  Rcpp::IntegerVector::const_iterator it;      // iterator

  // marshal
  for (it = x.begin(); it != x.end(); ++it) {  // it < x.end() ?
    // identify bit change
    if (prev != *it) {
      // record bit count
      if (cnt == 1) {
        y.push_back(prev);
      } else {
        y.push_back(cnt);
        y.push_back(prev);  // dry here
      }
      // reset bit count
      cnt = 0;
    }
    // increment bit count
    cnt++;
    // keep bit as lookbehind
    prev = *it;
  }

  // consume remainder
  if (cnt == 1) {
    y.push_back(x[x.size() - 2]);  // or - 1 ?
  } else {
    y.push_back(cnt);
    y.push_back(x[x.size() - 2]);  // dry here
  }

  // serve
  return y;
}
