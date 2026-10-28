/*
** Measure the per-launch cost of Kokkos::parallel_for on this machine so the
** OpenMP regression can be attributed either to dispatch overhead or to the
** kernel itself.  Built against the same Kokkos tree as the ROMS binaries.
**
**   bench_launch [points-per-launch] [reps]
**
** "empty" launches an empty lambda -- whatever it costs is pure dispatch.
** "work" does ~10 flops per point, a stand-in for a step2d point.
*/

#include <Kokkos_Core.hpp>

#include <chrono>
#include <cstdio>
#include <cstdlib>

template <class Exec>
void bench_empty(const char* tag, int n, int reps) {
  using policy = Kokkos::RangePolicy<Exec, Kokkos::IndexType<int>>;
  Kokkos::parallel_for("warm", policy(0, n), KOKKOS_LAMBDA(int) {});
  auto t0 = std::chrono::steady_clock::now();
  for (int k = 0; k < reps; ++k)
    Kokkos::parallel_for("empty", policy(0, n), KOKKOS_LAMBDA(int) {});
  auto t1 = std::chrono::steady_clock::now();
  double us = std::chrono::duration<double, std::micro>(t1 - t0).count() / reps;
  std::printf("%-7s empty  n=%-8d %9.2f us/launch\n", tag, n, us);
}

template <class Exec>
void bench_work(const char* tag, int n, int reps) {
  using policy = Kokkos::RangePolicy<Exec, Kokkos::IndexType<int>>;
  Kokkos::View<double*, Kokkos::HostSpace> a("a", n), b("b", n);
  for (int i = 0; i < n; ++i) {
    a(i) = 1.0e-9 * i;
    b(i) = 0.0;
  }
  Kokkos::parallel_for("warm", policy(0, n), KOKKOS_LAMBDA(int i) {
    double s = a(i);
    for (int m = 0; m < 8; ++m) s = s * 1.0000001 + 1.0e-12;
    b(i) = s;
  });
  auto t0 = std::chrono::steady_clock::now();
  for (int k = 0; k < reps; ++k)
    Kokkos::parallel_for("work", policy(0, n), KOKKOS_LAMBDA(int i) {
      double s = a(i);
      for (int m = 0; m < 8; ++m) s = s * 1.0000001 + 1.0e-12;
      b(i) = s;
    });
  auto t1 = std::chrono::steady_clock::now();
  double us = std::chrono::duration<double, std::micro>(t1 - t0).count() / reps;
  std::printf("%-7s work   n=%-8d %9.2f us/launch  %6.3f ns/point\n", tag, n,
              us, 1000.0 * us / n);
}

int main(int argc, char** argv) {
  Kokkos::initialize(argc, argv);
  {
    const int n = argc > 1 ? std::atoi(argv[1]) : 262144;
    const int reps = argc > 2 ? std::atoi(argv[2]) : 500;
    std::printf("OpenMP concurrency=%d  Serial concurrency=%d  n=%d reps=%d\n",
                Kokkos::OpenMP::concurrency(), Kokkos::Serial::concurrency(), n,
                reps);
    bench_empty<Kokkos::Serial>("Serial", n, reps);
    bench_empty<Kokkos::OpenMP>("OpenMP", n, reps);
    bench_work<Kokkos::Serial>("Serial", n, reps / 10 + 1);
    bench_work<Kokkos::OpenMP>("OpenMP", n, reps / 10 + 1);
  }
  Kokkos::finalize();
  return 0;
}
