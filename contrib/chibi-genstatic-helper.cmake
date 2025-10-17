#
# chibi-genstatic-helper.cmake
#
#  INPUT:
#   EXEC=<EXECUTABLE>
#   GENSTATIC=<FILE>
#   FEATURES=<STRING>
#   STUBS=<FILE>
#   OUT=<FILE>
if(NOT EXEC)
    message(FATAL_ERROR "huh?")
endif()

if(NOT OUT)
    message(FATAL_ERROR "huh?")
endif()

set(ENV{CHIBI_IGNORE_SYSTEM_PATH} 1)
set(ENV{CHIBI_MODULE_PATH} lib)

if(FEATURES)
    set(FEATURES_FLAG "--features")
endif()

execute_process(
    COMMAND ${EXEC} ${GENSTATIC} --no-inline ${FEATURES_FLAG} ${FEATURES}
    INPUT_FILE ${STUBS}
    OUTPUT_FILE ${OUT}
    RESULT_VARIABLE rr
    )

if(rr)
    message(FATAL_ERROR "Error: ${rr}")
endif()
