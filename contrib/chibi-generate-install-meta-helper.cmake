
execute_process(
    COMMAND find ${LIBDIR} -name "*.sld"
    COMMAND ${EXEC} ${GENMETA} ${VERSION}
    OUTPUT_FILE ${OUT}
    RESULT_VARIABLE error)

if(error)
    message(FATAL_ERROR "${error}")
endif()
