/* Host scratch shared by the Kokkos kernel implementations.
** Each translation unit keeps its own thread_local instance. Allocation
** and teardown use no Kokkos services, so storage can outlive finalize(). */
#ifndef ROMS_KOKKOS_WORKSPACE_HPP
#define ROMS_KOKKOS_WORKSPACE_HPP

#ifndef __sw_slave__
#include <cstddef>
#include <memory>

namespace roms {

class HostWorkspace {
 public:
  double* reserve(std::size_t count) {
    if (count > capacity_) {
      storage_.reset(new double[count]);
      capacity_ = count;
    }
    return storage_.get();
  }

 private:
  std::unique_ptr<double[]> storage_;
  std::size_t capacity_ = 0;
};

}  // namespace roms
#endif  // !__sw_slave__

#endif  // ROMS_KOKKOS_WORKSPACE_HPP
