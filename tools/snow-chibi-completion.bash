#/usr/bin/env bash

_snow_chibi_completions() {
    if [ "${#COMP_WORDS[@]}" -gt "2" ]
    then
        COMPREPLY=($(compgen -f -- "${COMP_WORDS[COMP_CWORD]}"))
    else
        COMPREPLY=($(compgen -W "search show install upgrade remove status package gen-key reg-key sign verify upload index update implementations help" "${COMP_WORDS[COMP_CWORD]}"))
    fi
}

complete -o bashdefault -F _snow_chibi_completions snow-chibi
