#include <Rcpp.h>

//[[Rcpp::export]]
Rcpp::IntegerVector decompressBits(const Rcpp::IntegerVector y) {

  // setup
  std::vector<int> z;                      // STL return vector
  const int ARR[] = {0, 1};                // binary digits
  const std::vector<int> BITS(ARR, ARR + sizeof(ARR) / sizeof(ARR[0]));
  int prev = 0;                            // lookbehind
  Rcpp::IntegerVector::const_iterator it;  // iterator

  // decompress
  for (it = y.begin(); it != y.end(); ++it) {
    if (prev > 1 &&
        std::binary_search(BITS.begin(), BITS.end(), *it)) {
      // encoded bit run
      std::vector<int> run(prev, *it);
      z.insert(z.end(), run.begin(), run.end());
    } else if (std::binary_search(BITS.begin(), BITS.end(), *it)) {
      // unencoded literal bit
      z.push_back(*it);
    }
    prev = *it;  // keep bit as lookbehind
  }

  // serve
  return Rcpp::wrap(z);
}
