#!/bin/sh
set -eu

version=${ERGM_JS_VERSION:-0.4.0}
if ! printf '%s\n' "$version" | grep -Eq '^[0-9]+\.[0-9]+\.[0-9]+$'; then
  echo "ERGM_JS_VERSION must be a release version such as 0.4.0" >&2
  exit 1
fi

root=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT HUP INT TERM
base="https://raw.githubusercontent.com/gvegayon/ergm-js/v$version"

mkdir -p "$tmp/src" "$tmp/vendor"
for file in LICENSE.md src/ergm.js src/ergm-widget.js vendor/graphology.umd.min.js vendor/sigma.min.js vendor/README.md; do
  echo "Downloading ergm-js v$version: $file"
  curl -fsSL "$base/$file" -o "$tmp/$file"
  if [ ! -s "$tmp/$file" ]; then
    echo "Downloaded file is empty: $file" >&2
    exit 1
  fi
done

if ! grep -Fq "const VERSION = \"$version\";" "$tmp/src/ergm.js"; then
  echo "Downloaded ergm-js does not declare version $version" >&2
  exit 1
fi

for file in LICENSE.md src/ergm.js src/ergm-widget.js vendor/graphology.umd.min.js vendor/sigma.min.js vendor/README.md; do
  cp "$tmp/$file" "$root/ergm-js/$file"
done
echo "Updated ergm-js to v$version. Render and test the live demo before publishing."
