#!/bin/bash
# https://gist.github.com/unhammer/c1ac7320141f09ac38e0

# Usage: merlin_init.sh | grep pkg >> .merlin

eval $(opam config env)

# Add PKG's:
ocamlfind list \
    | awk '{ print "PKG "$1 }'

# See https://github.com/the-lambda-church/merlin/wiki/Letting-merlin-locate-go-to-stuff-in-.opam
find "$OPAM_SWITCH_PREFIX" -name '*.cmt' -print0 \
    | xargs -0 -I{} dirname '{}' \
    | sort -u \
    | awk '{ print "S "$0"\nB "$0 }'
