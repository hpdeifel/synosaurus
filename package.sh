#!/bin/bash

if [[ $# -ne 1 ]]; then
    echo "Usage: package.sh VERSION"
    exit 1
fi

git archive --prefix synosaurus-$1/ --output synosaurus-$1.tar HEAD^{tree}
