#include "YAKL.h"
#include <bits/stdc++.h>

typedef double real;
typedef yakl::Array<real, 1, yakl::memHost, yakl::styleFortran> realHost1d;
typedef yakl::Array<real, 2, yakl::memHost, yakl::styleFortran> realHost2d;
typedef yakl::Array<real, 3, yakl::memHost, yakl::styleFortran> realHost3d;
typedef yakl::Array<real, 1, yakl::memDevice, yakl::styleFortran> real1d;
typedef yakl::Array<real, 2, yakl::memDevice, yakl::styleFortran> real2d;
typedef yakl::Array<real, 3, yakl::memDevice, yakl::styleFortran> real3d;
