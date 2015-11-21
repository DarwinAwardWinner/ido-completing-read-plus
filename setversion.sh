#!/bin/bash

TARGET_VERSION="$1"
if [ -n "$TARGET_VERSION" ]; then
    echo "Updating version to $TARGET_VERSION";
    perl -i'orig_*' -lape "s/Version: [0-9.]+/Version: $TARGET_VERSION/g;" \
         -e "s/((?:defconst|defvar|setq).*-version\s+)\"[0-9.]+\"/\${1}\"$TARGET_VERSION\"/g;" \
         -e "s/(Package-Requires.*\(ido-completing-read\+\s+)\"[0-9.]+\"\)/\${1}\"${TARGET_VERSION}\")/g;" \
         *.el
fi
