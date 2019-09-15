#include <range/v3/view.hpp>
#include <range/v3/numeric/inner_product.hpp>
#include <chrono>

using namespace ranges::view;

int main(int argc, char * argv[]) {
  size_t N = 3000;
  if(argc > 1) N = std::atol(argv[1]);

  auto matrix = [](size_t n) {
    return iota(0ul, n * n) | transform([n, tmp = 1. / n / n](auto x) {
      ssize_t i = x / n, j = x % n;
      return tmp * (i - j) * (i + j);
    });
  };

  auto a = matrix(N), b = matrix(N);

  auto column = [N](const auto & m, size_t n) {
    return slice(m, n, m.size()) | stride(N);
  };

  auto row = [N](const auto & m, size_t n) {
    return slice(m, n * N, n * N + N);
  };

  auto mul = [=](const auto & a, const auto & b) {
    return iota(0ul, N) | transform([=](auto i) {
      return iota(0ul, N) | transform([=](auto j) {
        return ranges::inner_product(row(a, i), column(b, j), 0.);
      });
    });
  };


  auto start = std::chrono::high_resolution_clock::now();
  auto c = mul(a, b);
  auto time = std::chrono::high_resolution_clock::now() - start;

  fprintf(stderr, "%luns\n", size_t(std::chrono::duration_cast<std::chrono::nanoseconds>(time).count()));
  fprintf(stderr, "%f\n", c[N/2][N/2]);
}
