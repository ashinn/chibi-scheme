
cmake_minimum_required(VERSION 3.12)

file(READ ${CMAKE_CURRENT_SOURCE_DIR}/VERSION version)
string(STRIP ${version} version)

file(READ ${CMAKE_CURRENT_SOURCE_DIR}/RELEASE release)
string(STRIP ${release} release)

project(chibi-scheme LANGUAGES C VERSION ${version}
    DESCRIPTION "Chibi-Scheme: minimal r7rs implementation, release: ${release}")

include(CheckIncludeFile)
include(CheckSymbolExists)
include(GNUInstallDirs)
include(CMakePackageConfigHelpers)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(CMAKE_BUILD_TYPE "${CMAKE_BUILD_TYPE}" CACHE STRING
    "Build type: None, Debug, Release, RelWithDebInfo, MinSizeRel, or Sanitizer." FORCE)

if (NOT EXISTS ${CMAKE_BINARY_DIR}/CMakeCache.txt AND NOT CMAKE_BUILD_TYPE)
    # CMake doesn't have a default build type, so set one manually
    set(CMAKE_BUILD_TYPE "RelWithDebInfo" CACHE STRING "" FORCE)
endif()

#
# Features
#

check_include_file(poll.h HAVE_POLL_H)
check_symbol_exists(ntp_gettime sys/timex.h HAVE_NTP_GETTIME)
check_symbol_exists(int_least8_t inttypes.h HAVE_STDINT_H)

if (WIN32 AND NOT CYGWIN)
    set(DEFAULT_SHARED_LIBS OFF)
else()
    set(DEFAULT_SHARED_LIBS ON)
endif()

option(BUILD_SHARED_LIBS "Build chibi-scheme as a shared library" ${DEFAULT_SHARED_LIBS})
option(SEXP_USE_BOEHM "Use Boehm garbage collection library" OFF)

if(SEXP_USE_BOEHM)
    find_library(BOEHMGC gc REQUIRED)
    find_path(BOEHMGC_INCLUDE NAMES gc/gc.h)
endif()

set(chibi-scheme-exclude-modules)
if(WIN32)
    set(chibi-scheme-exclude-modules
        # Following modules are not compatible with Win32
        lib/chibi/net.sld
        lib/chibi/process.sld
        lib/chibi/stty.sld
        lib/chibi/system.sld
        lib/chibi/time.sld
        lib/chibi/pty.sld)
endif()

#
# Default settings for all targets. We use an interface library here to not
# pollute/mutate global settings. Any configuration applied to this library
# is propagated to its client targets.
#

add_library(libchibi-common
    INTERFACE)

target_compile_definitions(libchibi-common
    INTERFACE
    SEXP_STATIC_LIBRARY=$<NOT:$<BOOL:${BUILD_SHARED_LIBS}>>
    SEXP_USE_DL=$<BOOL:${BUILD_SHARED_LIBS}>
    $<$<PLATFORM_ID:Windows>:BUILDING_DLL=$<BOOL:${BUILD_SHARED_LIBS}>>
    SEXP_USE_INTTYPES=$<BOOL:${HAVE_STDINT_H}>
    SEXP_USE_NTPGETTIME=$<BOOL:${HAVE_NTP_GETTIME}>
    $<$<NOT:$<BOOL:${HAVE_POLL_H}>>:SEXP_USE_GREEN_THREADS=0>
    $<$<PLATFORM_ID:Windows>:SEXP_USE_STRING_STREAMS=0>
    $<$<BOOL:${SEXP_USE_BOEHM}>:SEXP_USE_BOEHM=1>)

target_compile_options(libchibi-common
    INTERFACE
    $<$<C_COMPILER_ID:GNU>:-Wall>
    $<$<OR:$<C_COMPILER_ID:AppleClang>,$<C_COMPILER_ID:Clang>>:-Wall>
    $<$<CONFIG:SANITIZER>:-g
    -fsanitize=address,undefined,integer,float-divide-by-zero,float-cast-overflow,return
    -fno-omit-frame-pointer>)

target_include_directories(libchibi-common
    INTERFACE
    ${BOEHMGC_INCLUDE}
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include>
    $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>)

target_link_libraries(libchibi-common INTERFACE
    ${BOEHMGC}
    $<$<CONFIG:SANITIZER>:-fsanitize=address,undefined>
    $<$<PLATFORM_ID:Windows>:ws2_32>)

#
# Sources
#

set(chibi-scheme-srcs
    # SEXP
    gc.c
    sexp.c
    bignum.c
    gc_heap.c

    # Eval
    opcodes.c
    vm.c
    eval.c
    simplify.c)

#
# Bootstrap
#

add_executable(chibi-scheme-bootstrap
    EXCLUDE_FROM_ALL
    ${chibi-scheme-srcs}
    main.c)

target_link_libraries(chibi-scheme-bootstrap PRIVATE libchibi-common)


#
# Core library
#

add_library(libchibi-scheme
    ${chibi-scheme-srcs})

target_link_libraries(libchibi-scheme
    PUBLIC libchibi-common)

set_target_properties(libchibi-scheme
    PROPERTIES
    PREFIX "" # It's liblibchibi-scheme otherwise
    SOVERSION ${CMAKE_PROJECT_VERSION_MAJOR}
    VERSION ${CMAKE_PROJECT_VERSION})


#
# Generate modules
#

file(GLOB_RECURSE slds RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}
    CONFIGURE_DEPENDS lib/*.sld)
if (chibi-scheme-exclude-modules)
    # CMake doesn't complain anymore about an empty 2nd argument, but 3.12 does. When we require a
    # more recent version, the if-guard should go.
    list(REMOVE_ITEM slds ${chibi-scheme-exclude-modules})
endif()

set(chibi-ffi ${CMAKE_CURRENT_SOURCE_DIR}/tools/chibi-ffi)
set(chibi-genstatic ${CMAKE_CURRENT_SOURCE_DIR}/tools/chibi-genstatic)

add_custom_target(chibi-compiled-libs)

function(add_compiled_library cfile)
    if (NOT BUILD_SHARED_LIBS)
        return()
    endif()

    get_filename_component(basename ${cfile} NAME_WE)
    get_filename_component(libdir ${cfile} DIRECTORY)

    if(NOT IS_ABSOLUTE ${libdir})
        set(libdir ${CMAKE_CURRENT_BINARY_DIR}/${libdir})
    endif()

    file(RELATIVE_PATH libname ${CMAKE_CURRENT_BINARY_DIR} ${libdir}/${basename})
    string(REPLACE "/" "-" libname ${libname})

    add_library(${libname} ${cfile})
    target_link_libraries(${libname} PRIVATE libchibi-scheme)
    add_dependencies(chibi-compiled-libs ${libname})

    set_target_properties(${libname} PROPERTIES
        LIBRARY_OUTPUT_DIRECTORY ${libdir}
        LIBRARY_OUTPUT_NAME ${basename}
        PREFIX "")

    file(RELATIVE_PATH installsubdir ${CMAKE_CURRENT_BINARY_DIR}/lib ${libdir})
    install(TARGETS ${libname}
        LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/chibi/${installsubdir})
endfunction()

if(BUILD_SHARED_LIBS)
    # This makes sure we only use the separate bootstrap executable for static
    # builds. With dynamic linking, the default executable is fine. The dispatch
    # is not a generator expression within the actual custom command to process
    # the stubs, as older CMake versions fail to properly construct the dependency
    # on the bootstrap executable from the generator expression.
    set(bootstrap chibi-scheme)
else()
    set(bootstrap chibi-scheme-bootstrap)
endif()

function(add_stubs_library stub)
    get_filename_component(stubdir ${stub} PATH)
    get_filename_component(basename ${stub} NAME_WE)
    set(stubfile ${CMAKE_CURRENT_SOURCE_DIR}/${stub})
    set(stubdir ${CMAKE_CURRENT_BINARY_DIR}/${stubdir})
    set(stubout ${stubdir}/${basename}.c)
    set(stubouts ${stubouts} ${stubout} PARENT_SCOPE)

    file(MAKE_DIRECTORY ${stubdir})


    add_custom_command(OUTPUT ${stubout}
        COMMAND ${bootstrap} ${chibi-ffi} ${stubfile} ${stubout}
        DEPENDS ${stubfile} ${chibi-ffi}
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

    add_compiled_library(${stubout})
endfunction()

add_stubs_library(lib/chibi/crypto/crypto.stub)
add_stubs_library(lib/chibi/emscripten.stub)
add_stubs_library(lib/chibi/filesystem.stub)
add_stubs_library(lib/chibi/io/io.stub)
add_stubs_library(lib/scheme/bytevector.stub)
add_stubs_library(lib/srfi/144/math.stub)
add_stubs_library(lib/srfi/160/uvprims.stub)

if(NOT WIN32)
    add_stubs_library(lib/chibi/net.stub)
    add_stubs_library(lib/chibi/process.stub)
    add_stubs_library(lib/chibi/pty.stub)
    add_stubs_library(lib/chibi/stty.stub)
    add_stubs_library(lib/chibi/system.stub)
    add_stubs_library(lib/chibi/time.stub)
else()
    add_stubs_library(lib/chibi/win32/process-win32.stub)
endif()

add_custom_target(chibi-scheme-stubs DEPENDS ${stubouts})

if (NOT BUILD_SHARED_LIBS)
    add_dependencies(libchibi-scheme chibi-scheme-stubs)
endif()

add_compiled_library(lib/chibi/weak.c)
add_compiled_library(lib/chibi/heap-stats.c)
add_compiled_library(lib/chibi/disasm.c)
add_compiled_library(lib/chibi/ast.c)
add_compiled_library(lib/chibi/json.c)
add_compiled_library(lib/srfi/18/threads.c)
add_compiled_library(lib/chibi/optimize/rest.c)
add_compiled_library(lib/chibi/optimize/profile.c)
add_compiled_library(lib/srfi/27/rand.c)
add_compiled_library(lib/srfi/151/bit.c)
add_compiled_library(lib/srfi/39/param.c)
add_compiled_library(lib/srfi/69/hash.c)
add_compiled_library(lib/srfi/95/qsort.c)
add_compiled_library(lib/srfi/98/env.c)
add_compiled_library(lib/scheme/time.c)

#
# Generate clib.c for SEXP_USE_STATIC_LIBS
#

if (NOT BUILD_SHARED_LIBS)
    string(REPLACE ";" "\n" genstatic-input "${slds}")
    set(clibin ${CMAKE_CURRENT_BINARY_DIR}/clib-in.txt)
    set(clibout ${CMAKE_CURRENT_BINARY_DIR}/clib.c)
    set(genstatic-helper
        ${CMAKE_CURRENT_LIST_DIR}/contrib/chibi-genstatic-helper.cmake)
    file(WRITE ${clibin} "${genstatic-input}")

    add_custom_command(OUTPUT ${clibout}
        COMMAND
        ${CMAKE_COMMAND}
        -DEXEC=$<TARGET_FILE:chibi-scheme-bootstrap>
        -DGENSTATIC=${chibi-genstatic}
        -DSTUBS=${clibin}
        -DOUT=${clibout}
        -P ${genstatic-helper}
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        DEPENDS
        chibi-scheme-bootstrap
        ${chibi-genstatic}
        ${genstatic-helper}
        ${slds})

    # The generated file will #include both manually written files in
    # the source directory as well as files generated by chibi-ffi in
    # the build directory. The latter can be found without special flags,
    # as they are relative to the clib.c, but the preprocessor needs
    # help for the former. As only clib.c needs this flag, we set it
    # as locally as possible, i.e., not as a target property.
    set_source_files_properties(${clibout}
        PROPERTIES
        INCLUDE_DIRECTORIES
        ${CMAKE_CURRENT_SOURCE_DIR})

    target_compile_definitions(libchibi-scheme
        PUBLIC
        SEXP_USE_STATIC_LIBS=1)

    target_sources(libchibi-scheme
        PRIVATE
        ${clibout})
endif()

#
# Interpreter
#

add_executable(chibi-scheme
    main.c)

target_link_libraries(chibi-scheme
    PRIVATE libchibi-scheme)

#
# Generate "chibi/install.h"
#

if(WIN32)
    set(platform "windows")
elseif(CYGWIN)
    set(platform "cygwin")
elseif(APPLE)
    set(platform "macosx")
elseif(CMAKE_SYSTEM MATCHES "[Bb][Ss][Dd]")
    set(platform "bsd")
elseif(CMAKE_SYSTEM MATCHES "[Aa]ndroid")
    set(platform "android")
elseif(CMAKE_SYSTEM MATCHES "[Ss]un[Oo][Ss]")
    set(platform "solaris")
elseif (CMAKE_SYSTEM MATCHES "[Ll]inux")
    set(platform "linux")
else()
    set(platform "unix")
endif()

if(WIN32)
    # Leave this empty for now, as the default GNU install directories won't
    # help on Windows.
    set(default_module_path "")
else()
    string(JOIN ":" default_module_path
        ${CMAKE_INSTALL_FULL_DATAROOTDIR}/chibi
        ${CMAKE_INSTALL_FULL_LIBDIR}/chibi
        ${CMAKE_INSTALL_FULL_DATAROOTDIR}/snow
        ${CMAKE_INSTALL_FULL_LIBDIR}/snow)
endif()

configure_file(include/chibi/install.h.in include/chibi/install.h)

#
# Testing
#

enable_testing()

set(chibi-scheme-tests
    r7rs-tests
    division-tests
    syntax-tests
    unicode-tests)

foreach(e ${chibi-scheme-tests})
    add_test(NAME "${e}"
        COMMAND chibi-scheme -I ${CMAKE_CURRENT_BINARY_DIR}/lib tests/${e}.scm
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
endforeach()

add_test(NAME r5rs-test
    COMMAND chibi-scheme -I ${CMAKE_CURRENT_BINARY_DIR}/lib -xchibi tests/r5rs-tests.scm
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

file(GLOB_RECURSE srfi_tests RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}/lib
    CONFIGURE_DEPENDS lib/srfi/*/test.sld)

file(GLOB_RECURSE chibi_scheme_tests RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}/lib
    CONFIGURE_DEPENDS lib/chibi/*-test.sld)

set(win32testexcludes
    # Excluded tests
    chibi/filesystem-test
    chibi/memoize-test
    chibi/term/ansi-test
    chibi/weak-test

    # Not ported to Win32
    srfi/18/test # Threading
    chibi/doc-test # Depends (chibi time)
    chibi/log-test
    chibi/system-test
    chibi/tar-test # Depends (chibi system)
    chibi/process-test # Not applicable
    chibi/pty-test # Depends (chibi pty)
    )

foreach(e ${srfi_tests} ${chibi_scheme_tests})
    get_filename_component(pth ${e} PATH)
    get_filename_component(nam ${e} NAME_WE)
    list(APPEND testlibs ${pth}/${nam})
endforeach()

if(WIN32)
    list(REMOVE_ITEM testlibs ${win32testexcludes})
endif()

foreach(e ${testlibs})
    string(REGEX REPLACE "/" "_" testname ${e})
    string(REGEX REPLACE "/" " " form ${e})
    add_test(NAME "lib_${testname}"
        COMMAND chibi-scheme -I ${CMAKE_CURRENT_BINARY_DIR}/lib
        -e "(import (${form}))"
        -e "(run-tests)"
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
endforeach()

#
# Testing (embedding)
#

add_executable(test-foreign-apply-loop
    tests/foreign/apply-loop.c)

target_link_libraries(test-foreign-apply-loop
    PRIVATE libchibi-scheme)

add_test(NAME "foreign-apply-loop"
    COMMAND test-foreign-apply-loop
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

add_executable(test-foreign-typeid
    tests/foreign/typeid.c)

target_link_libraries(test-foreign-typeid
    PRIVATE libchibi-scheme)

add_test(NAME "foreign-typeid"
    COMMAND test-foreign-typeid
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})


#
# Image, pkgconfig and meta file generation
#

add_custom_command(OUTPUT chibi.img
    COMMAND chibi-scheme -I ${CMAKE_CURRENT_BINARY_DIR}/lib -mchibi.repl
    -d ${CMAKE_CURRENT_BINARY_DIR}/chibi.img
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
add_custom_command(OUTPUT red.img
    COMMAND chibi-scheme -I ${CMAKE_CURRENT_BINARY_DIR}/lib -xscheme.red -mchibi.repl
    -d ${CMAKE_CURRENT_BINARY_DIR}/red.img
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
add_custom_command(OUTPUT snow.img
    COMMAND chibi-scheme -I ${CMAKE_CURRENT_BINARY_DIR}/lib
        -mchibi.snow.commands -mchibi.snow.interface -mchibi.snow.package -mchibi.snow.utils
        -d ${CMAKE_CURRENT_BINARY_DIR}/snow.img
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

if(BUILD_SHARED_LIBS)
    # Currently, image dumps only work with shared library builds, which includes Windows
    add_custom_target(chibi-images ALL
        DEPENDS
        ${CMAKE_CURRENT_BINARY_DIR}/chibi.img
        ${CMAKE_CURRENT_BINARY_DIR}/red.img
        ${CMAKE_CURRENT_BINARY_DIR}/snow.img
        # The dependency on libchibi-scheme is crucial here:
        chibi-compiled-libs)
endif()

configure_file(contrib/chibi-scheme.pc.cmake.in chibi-scheme.pc @ONLY)

function(generate_package_list libdir output)
    add_custom_command(OUTPUT ${output}
        COMMAND
        ${CMAKE_COMMAND}
        -DEXEC=$<TARGET_FILE:chibi-scheme>
        -DLIBDIR=${libdir}
        -DGENMETA=tools/generate-install-meta.scm
        -DVERSION=${CMAKE_PROJECT_VERSION}
        -DOUT=${CMAKE_CURRENT_BINARY_DIR}/${output}
        -P contrib/chibi-generate-install-meta-helper.cmake
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        DEPENDS
        chibi-scheme
        tools/generate-install-meta.scm
        contrib/chibi-generate-install-meta-helper.cmake)
endfunction()

generate_package_list(lib/chibi .chibi.meta)
generate_package_list(lib/scheme .scheme.meta)
generate_package_list(lib/srfi .srfi.meta)

add_custom_target(chibi-meta-lists ALL
    DEPENDS
    ${CMAKE_CURRENT_BINARY_DIR}/.chibi.meta
    ${CMAKE_CURRENT_BINARY_DIR}/.scheme.meta
    ${CMAKE_CURRENT_BINARY_DIR}/.srfi.meta)


#
# Installation
#

install(DIRECTORY include/chibi
    DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
    PATTERN "sexp-*.[hc]" EXCLUDE
    PATTERN "*.h.in" EXCLUDE)

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/include/chibi/install.h
    DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/chibi)

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/chibi-scheme.pc
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/pkgconfig)

add_library(chibi::libchibi-scheme ALIAS libchibi-scheme)

install(TARGETS libchibi-scheme libchibi-common chibi-scheme
    EXPORT chibi-scheme-targets
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR})

install(FILES
    tools/chibi-ffi
    tools/chibi-doc
    tools/snow-chibi
    tools/snow-chibi.scm
    DESTINATION ${CMAKE_INSTALL_BINDIR})

install(FILES
    doc/chibi-scheme.1
    doc/chibi-ffi.1
    doc/chibi-doc.1
    DESTINATION ${CMAKE_INSTALL_MANDIR}/man1)

if(BUILD_SHARED_LIBS)
    install(FILES
        ${CMAKE_CURRENT_BINARY_DIR}/chibi.img
        ${CMAKE_CURRENT_BINARY_DIR}/red.img
        ${CMAKE_CURRENT_BINARY_DIR}/snow.img
        DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/chibi)
endif()

install(DIRECTORY
    lib/
    DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/chibi
    PATTERN "*win32" EXCLUDE
    PATTERN "*test.sld" EXCLUDE
    PATTERN "*.c" EXCLUDE
    PATTERN "*.stub" EXCLUDE)

# This is to revert the above exclusion pattern
install(FILES lib/chibi/test.sld
    DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/chibi/chibi)

if(WIN32)
    install(DIRECTORY
        lib/
        DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/chibi
        FILES_MATCHING
        PATTERN "*win32/*.scm"
        PATTERN "*win32/*.sld")
endif()

install(FILES
    ${CMAKE_CURRENT_BINARY_DIR}/.chibi.meta
    ${CMAKE_CURRENT_BINARY_DIR}/.scheme.meta
    ${CMAKE_CURRENT_BINARY_DIR}/.srfi.meta
    DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/chibi)

install(EXPORT chibi-scheme-targets
    FILE chibi-scheme-targets.cmake
    NAMESPACE chibi::
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/chibi)

write_basic_package_version_file(chibi-scheme-config-version.cmake
    VERSION ${CMAKE_PROJECT_VERSION}
    COMPATIBILITY ExactVersion)

install(FILES
    contrib/chibi-scheme-config.cmake
    ${CMAKE_CURRENT_BINARY_DIR}/chibi-scheme-config-version.cmake
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/chibi)
