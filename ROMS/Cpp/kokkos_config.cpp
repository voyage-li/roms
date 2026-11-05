/*
** git $Id$
************************************************************************
** Copyright (c) 2002-2025 The ROMS Group                               **
**   Licensed under a MIT/X style license                               **
**   See License_ROMS.md                                                **
************************************************************************
*/

#include "kokkos_config.hpp"

#if defined ROMS_KOKKOS && !defined(__sw_slave__)

#include <chrono>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <mutex>
#include <iostream>

namespace roms {

namespace {
std::once_flag init_flag;
std::once_flag fini_flag;

/* Per-kernel timing table (see ROMS_KK_TIMER in kokkos_config.hpp). */
constexpr int kMaxSlots = 64;
struct TimerSlot {
  const char*   name = nullptr;
  unsigned long calls = 0;
  double        seconds = 0.0;
};
TimerSlot  g_slots[kMaxSlots];
int        g_nslots = 0;
std::mutex g_timer_mutex;

void report_timers() {
  if (!timer_enabled() || g_nslots == 0) return;
  double total = 0.0;
  std::printf("\nROMS Kokkos kernel timing (wall, %s)\n", exec_space::name());
  std::printf("  %-24s %10s %12s %12s\n", "kernel", "calls", "seconds",
              "us/call");
  for (int s = 0; s < g_nslots; ++s) {
    const TimerSlot& t = g_slots[s];
    if (t.calls == 0) continue;
    total += t.seconds;
    std::printf("  %-24s %10lu %12.6f %12.3f\n", t.name, t.calls, t.seconds,
                1.0e6 * t.seconds / double(t.calls));
  }
  std::printf("  %-24s %10s %12.6f\n", "sum", "", total);
  std::fflush(stdout);
}
}  // namespace

bool timer_enabled() {
  static const bool on = [] {
    const char* v = std::getenv("ROMS_KK_TIMING");
    return v != nullptr && *v != '\0' && std::strcmp(v, "0") != 0;
  }();
  return on;
}

double wall_seconds() {
  return std::chrono::duration<double>(
             std::chrono::steady_clock::now().time_since_epoch())
      .count();
}

int timer_slot(const char* name) {
  std::lock_guard<std::mutex> lock(g_timer_mutex);
  for (int s = 0; s < g_nslots; ++s)
    if (std::strcmp(g_slots[s].name, name) == 0) return s;
  if (g_nslots == kMaxSlots) return -1;
  g_slots[g_nslots].name = name;
  return g_nslots++;
}

void timer_add(int slot, double seconds) {
  if (slot < 0 || slot >= kMaxSlots) return;
  std::lock_guard<std::mutex> lock(g_timer_mutex);
  ++g_slots[slot].calls;
  g_slots[slot].seconds += seconds;
}

/*
** ROMS calls main3d inside an OpenMP parallel region, so several threads
** can reach this at the same time.  std::call_once makes the first one
** do the work and the rest wait.
*/

void initialize() {
  std::call_once(init_flag, []() {
    if (!Kokkos::is_initialized()) {
      Kokkos::initialize();
    }
    std::cout << "ROMS Kokkos execution space: " << exec_space::name()
              << std::endl;
    Kokkos::print_configuration(std::cout);
  });
}

void finalize() {
  std::call_once(fini_flag, []() {
    report_timers();
    if (Kokkos::is_initialized()) {
      Kokkos::finalize();
    }
  });
}

}  // namespace roms

/*
** C entry points for the Fortran driver (ROMS/Cpp/kokkos_step2d_mod.F).
*/

extern "C" {

void roms_kokkos_initialize() { roms::initialize(); }
void roms_kokkos_finalize()   { roms::finalize(); }

}  // extern "C"

#endif  // ROMS_KOKKOS
