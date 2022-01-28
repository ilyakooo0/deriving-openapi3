#!/bin/sh

set -e

EXTENSIONS="-XAllowAmbiguousTypes -XScopedTypeVariables -XKindSignatures -XDataKinds -XTypeOperators -XFlexibleInstances -XTypeApplications -XUndecidableInstances -XTypeFamilies -XPolyKinds"

DEPENDENCIES="--package doctest --package yaml"

stack exec $DEPENDENCIES -- find src -name '*.hs' -exec doctest $EXTENSIONS '{}' \;
