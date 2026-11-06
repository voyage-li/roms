#!/usr/bin/env python3
"""Host CMake regression tests for ROMS' Kokkos target wiring."""

import json
import re
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[3]
CMAKE_LISTS = ROOT / "CMakeLists.txt"
CMAKE = shutil.which("cmake")
PUBLIC_LINK_PATTERN = re.compile(
    r"target_link_libraries\s*\(\s*(\$\{PROJECT_NAME\}|ROMS_shared|ROMS_static)"
    r"\s+PUBLIC\s+Kokkos::kokkos\s*\)",
    re.IGNORECASE,
)
OBJECT_LINK_PATTERN = re.compile(
    r"target_link_libraries\s*\(\s*Objects\s+Kokkos::kokkos\s*\)",
    re.IGNORECASE,
)
OPTIONS_FUNCTION_PATTERN = re.compile(
    r"^[ \t]*function\(\s*roms_kokkos_cxx_options\s+target\s*\).*?"
    r"^[ \t]*endfunction\s*\(\s*\)",
    re.IGNORECASE | re.MULTILINE | re.DOTALL,
)


def production_rules():
    """Return the link rules and SW64 option function from the real project."""
    source = CMAKE_LISTS.read_text(encoding="utf-8")
    links = {
        match.group(1): match.group(0)
        for match in PUBLIC_LINK_PATTERN.finditer(source)
    }
    object_link = OBJECT_LINK_PATTERN.search(source)
    options_function = OPTIONS_FUNCTION_PATTERN.search(source)
    if not object_link or not options_function:
        raise AssertionError("Could not find ROMS Kokkos CMake rules")
    return links, object_link.group(0), options_function.group(0)


class KokkosCMakeRegression(unittest.TestCase):
    def test_each_public_roms_library_has_the_kokkos_interface(self):
        links, _, _ = production_rules()
        self.assertEqual(
            set(links), {"${PROJECT_NAME}", "ROMS_shared", "ROMS_static"},
            "Kokkos must be attached publicly to ecbuild, shared, and static ROMS targets",
        )

    @unittest.skipUnless(CMAKE, "cmake is not installed")
    def test_library_only_consumers_inherit_kokkos(self):
        links, object_link, _ = production_rules()
        self.assertEqual(
            set(links), {"${PROJECT_NAME}", "ROMS_shared", "ROMS_static"},
            "Kokkos must be attached publicly to ecbuild, shared, and static ROMS targets",
        )

        with tempfile.TemporaryDirectory(prefix="roms-kokkos-link-") as temporary:
            source = Path(temporary) / "src"
            build = Path(temporary) / "build"
            source.mkdir()
            (source / "kokkos").mkdir()
            (source / "kokkos" / "kokkos_api.hpp").write_text(
                "#pragma once\nint kokkos_value();\n", encoding="utf-8"
            )
            (source / "kokkos_runtime.cpp").write_text(
                '#include "kokkos_api.hpp"\nint kokkos_value() { return 17; }\n',
                encoding="utf-8",
            )
            (source / "objects.cpp").write_text(
                '#include "kokkos_api.hpp"\n'
                "int roms_entry() { return kokkos_value(); }\n",
                encoding="utf-8",
            )
            (source / "consumer.cpp").write_text(
                '#include "kokkos_api.hpp"\n'
                "int roms_entry();\n"
                "int main() { return roms_entry() == kokkos_value() ? 0 : 1; }\n",
                encoding="utf-8",
            )

            public_links = "\n".join(links.values())
            project = f"""\
cmake_minimum_required(VERSION 3.13)
project(ROMS_ecbuild LANGUAGES CXX)

add_library(kokkos_runtime STATIC kokkos_runtime.cpp)
set_property(TARGET kokkos_runtime PROPERTY POSITION_INDEPENDENT_CODE ON)
target_include_directories(kokkos_runtime PRIVATE "${{CMAKE_CURRENT_SOURCE_DIR}}/kokkos")
add_library(kokkos_iface INTERFACE)
target_include_directories(kokkos_iface INTERFACE "${{CMAKE_CURRENT_SOURCE_DIR}}/kokkos")
target_link_libraries(kokkos_iface INTERFACE kokkos_runtime)
add_library(Kokkos::kokkos ALIAS kokkos_iface)

add_library(Objects OBJECT objects.cpp)
set_property(TARGET Objects PROPERTY POSITION_INDEPENDENT_CODE ON)
{object_link}

# Model the three production library creation branches. The target dependency
# statements below are taken directly from the ROMS top-level CMakeLists.txt.
add_library(${{PROJECT_NAME}} STATIC $<TARGET_OBJECTS:Objects>)
add_library(ROMS_shared SHARED $<TARGET_OBJECTS:Objects>)
add_library(ROMS_static STATIC $<TARGET_OBJECTS:Objects>)
{public_links}

add_executable(consumer_ecbuild consumer.cpp)
target_link_libraries(consumer_ecbuild PRIVATE ${{PROJECT_NAME}})
add_executable(consumer_shared consumer.cpp)
target_link_libraries(consumer_shared PRIVATE ROMS_shared)
add_executable(consumer_static consumer.cpp)
target_link_libraries(consumer_static PRIVATE ROMS_static)
"""
            (source / "CMakeLists.txt").write_text(project, encoding="utf-8")

            configured = subprocess.run(
                [CMAKE, "-S", str(source), "-B", str(build)],
                cwd=ROOT,
                text=True,
                capture_output=True,
                check=False,
            )
            self.assertEqual(
                configured.returncode, 0, configured.stdout + configured.stderr
            )
            built = subprocess.run(
                [
                    CMAKE,
                    "--build",
                    str(build),
                    "--target",
                    "consumer_ecbuild",
                    "consumer_shared",
                    "consumer_static",
                ],
                cwd=ROOT,
                text=True,
                capture_output=True,
                check=False,
            )
            self.assertEqual(built.returncode, 0, built.stdout + built.stderr)

    @unittest.skipUnless(CMAKE, "cmake is not installed")
    def test_sw64_compile_options_follow_fast_math_mode(self):
        _, _, options_function = production_rules()
        with tempfile.TemporaryDirectory(prefix="roms-kokkos-flags-") as temporary:
            source = Path(temporary) / "src"
            source.mkdir()
            (source / "probe.cpp").write_text("int probe() { return 0; }\n", encoding="utf-8")
            (source / "CMakeLists.txt").write_text(
                f"""\
cmake_minimum_required(VERSION 3.13)
project(ROMS_Kokkos_options LANGUAGES CXX)
set(_kk_backend sw64)
set(ROMS_FAST_MATH ${{ROMS_FAST_MATH}})
{options_function}
add_library(probe OBJECT probe.cpp)
roms_kokkos_cxx_options(probe)
""",
                encoding="utf-8",
            )

            for fast_math in ("OFF", "ON"):
                with self.subTest(ROMS_FAST_MATH=fast_math):
                    build = Path(temporary) / f"build-{fast_math.lower()}"
                    configured = subprocess.run(
                        [
                            CMAKE,
                            "-S",
                            str(source),
                            "-B",
                            str(build),
                            f"-DROMS_FAST_MATH={fast_math}",
                            "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON",
                        ],
                        cwd=ROOT,
                        text=True,
                        capture_output=True,
                        check=False,
                    )
                    self.assertEqual(
                        configured.returncode, 0, configured.stdout + configured.stderr
                    )
                    commands = json.loads(
                        (build / "compile_commands.json").read_text(encoding="utf-8")
                    )
                    self.assertEqual(len(commands), 1)
                    args = commands[0].get("arguments")
                    if args is None:
                        import shlex

                        args = shlex.split(commands[0]["command"])
                    self.assertIn("-mhybrid-coding", args)
                    self.assertIn("-fpermissive", args)
                    if fast_math == "ON":
                        self.assertIn("--param", args)
                        self.assertIn("vect-max-version-for-alias-checks=64", args)
                        self.assertNotIn("-fno-tree-vectorize", args)
                    else:
                        self.assertNotIn("--param", args)
                        self.assertNotIn("vect-max-version-for-alias-checks=64", args)
                        self.assertIn("-fno-tree-vectorize", args)


if __name__ == "__main__":
    unittest.main()
