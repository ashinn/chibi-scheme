#
# chibi-genstatic-helper.cmake
#
#  INPUT:
#   ROOT=<DIR>
#   EXEC=<EXECUTABLE>
#   GENSTATIC=<FILE>
#   STUBS=<FILE>
#   OUT=<FILE>
if(NOT EXEC)
    message(FATAL_ERROR "huh?")
endif()

if(NOT OUT)
    message(FATAL_ERROR "huh?")
endif()

execute_process(
    COMMAND ${EXEC} ${GENSTATIC} --no-inline
    INPUT_FILE ${STUBS}
    OUTPUT_FILE ${OUT}
    RESULT_VARIABLE rr
    )

if(rr)
    message(FATAL_ERROR "Error: ${rr}")
endif()
