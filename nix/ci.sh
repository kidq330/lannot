#!/usr/bin/env bash

set -euxo pipefail

DEFAULT=${DEFAULT:-master}
NIX_TARGET="lannotate"
#           <plugin>   /nix       /ci.sh
PLUGIN_DIR="$(dirname "$(dirname "$(readlink -f "$0")")")"

CURRENT_BRANCH="${CI_MERGE_REQUEST_SOURCE_BRANCH_NAME:-}"
: "${CURRENT_BRANCH:="${CI_COMMIT_REF_NAME:-}"}"
: "${CURRENT_BRANCH:="$(git branch --show-current)"}"

# prints
# - "$CURRENT_BRANCH" if it is a branch name in remote $1,
# - else "$DEFAULT" if it is set and $DEFAULT is a branch name in remote $1,
# - else master
get_matching_branch () {
  if   git ls-remote --quiet --exit-code "$1" "$CURRENT_BRANCH" >/dev/null 2>/dev/null;
  then echo "$CURRENT_BRANCH"
  elif git ls-remote --quiet --exit-code "$1" "$DEFAULT" >/dev/null 2>/dev/null;
  then echo "$DEFAULT"
  else echo master
  fi
}

TMP_DIR="$(mktemp -d)"

cleanup () {
  rm -rf $TMP_DIR
}

trap cleanup EXIT

mkdir -p $TMP_DIR/frama-ci
frama_ci_repo="$(readlink -f $TMP_DIR/frama-ci)"
frama_ci_url="git@git.frama-c.com:frama-c/Frama-CI.git"
frama_ci_branch="$(get_matching_branch "$frama_ci_url")"
echo "using branch $frama_ci_branch of Frama-CI repo at $frama_ci_repo"
git clone --depth=1 --branch="$frama_ci_branch" "$frama_ci_url" "$frama_ci_repo"

source $frama_ci_repo/common-plugin-ci.sh
