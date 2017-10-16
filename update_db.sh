#!/usr/bin/env bash

set -euo pipefail
IFS=$'\n\t'

TAG=$(git describe --tags)

echo "$TAG" || exit 1

jq --arg tag "$TAG"\
   --raw-output \
   'to_entries | map("('"'"'" + .key + "'"'"', '"'"'" + .value.repo + "'"'"', '"'"'" + .value.version + "'"'"', '"'"'" + $tag + "'"'"')") | join(",\n  ") | "INSERT INTO package (name, repo, version, package_set) VALUES\n  " + . + "\n;"'\
   packages.json \
   > package.sql

jq --arg tag "$TAG"\
   --raw-output \
   'to_entries | map("('"'"'" + .key + "'"'"', '"'"'" + .value.dependencies[] + "'"'"', '"'"'" + $tag + "'"'"')") | join(",\n  ") | "INSERT INTO dependencies (independent, dependent, package_set) VALUES\n  " + . + "\n;"'\
   packages.json \
   > dependencies.sql

sqlite3 package_sets.sqlite3 < package.sql
sqlite3 package_sets.sqlite3 < dependencies.sql
