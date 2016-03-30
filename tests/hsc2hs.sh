#!/bin/sh

# Very stupid fake hsc2hs specific to our tests

awk -v hs="${1%c}" '
/^#{/ {
    skip = 1
}
!skip && !/^#include/ {
    lines = lines $0"\n"
}
/}/ {
    skip = 0
}

/A_TYPO/ {
    print FILENAME":"NR":58: error: ‘A_TYPO’ undeclared (first use in this function)" >"/dev/stderr"
    lines=""
    exit(1)
}

END {
    if(lines) {
        lines = lines "rand_max  :: NUMBERS\n"
        lines = lines "rand_max  = NUMBERS 2147483647\n"
        print lines > hs
    }
}
' "$1"
