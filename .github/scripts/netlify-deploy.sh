#!/usr/bin/env bash
# Deploy a directory to Netlify using the file-digest API, without the Netlify
# CLI or any third-party GitHub Action (the repository allows local actions only).
#
# Usage: netlify-deploy.sh <dir> <production|draft>
#
# Needs NETLIFY_AUTH_TOKEN and NETLIFY_SITE_ID in the environment, plus curl,
# jq and sha1sum. A draft deploy gets its own preview URL and never changes the
# live site. Set DRY_RUN=1 to print the file manifest instead of deploying.
#
# API reference: https://docs.netlify.com/api/get-started/#file-digest-method
set -euo pipefail

dir=${1:?usage: netlify-deploy.sh <dir> <production|draft>}
mode=${2:?usage: netlify-deploy.sh <dir> <production|draft>}
case "$mode" in
  production) draft=false ;;
  draft)      draft=true ;;
  *) echo "mode must be 'production' or 'draft', got '$mode'" >&2; exit 2 ;;
esac

api="https://api.netlify.com/api/v1"
cd "$dir"

# Netlify file paths are sent unencoded in the upload URL, so refuse anything
# that would need URL-encoding rather than uploading it wrongly.
bad=$(find . -type f | sed 's|^\./||' | grep -vE '^[A-Za-z0-9._/+-]+$' || true)
if [ -n "$bad" ]; then
  echo "::error::File names that need URL-encoding are not supported:" >&2
  echo "$bad" >&2
  exit 1
fi

# Manifest: {"/path": "sha1", ...}
manifest=$(find . -type f | sed 's|^\./||' | sort | while IFS= read -r f; do
  printf '/%s\t%s\n' "$f" "$(sha1sum "$f" | cut -d' ' -f1)"
done | jq -Rn '[inputs | split("\t") | {(.[0]): .[1]}] | add')

echo "Files: $(jq 'length' <<<"$manifest") | mode: $mode"
if [ "${DRY_RUN:-0}" = "1" ]; then
  jq . <<<"$manifest"
  exit 0
fi

: "${NETLIFY_AUTH_TOKEN:?NETLIFY_AUTH_TOKEN secret must be set}"
: "${NETLIFY_SITE_ID:?NETLIFY_SITE_ID secret must be set}"
auth=(-H "Authorization: Bearer ${NETLIFY_AUTH_TOKEN}")

# 1. Create the deploy; Netlify replies with the SHA1s it does not already have.
deploy=$(jq -n --argjson files "$manifest" --argjson draft "$draft" \
           '{files: $files, draft: $draft}' |
         curl --fail-with-body -sS "${auth[@]}" -H "Content-Type: application/json" \
           --data-binary @- "${api}/sites/${NETLIFY_SITE_ID}/deploys")
deploy_id=$(jq -r '.id' <<<"$deploy")
echo "Created deploy ${deploy_id}"

# 2. Upload one file for each required SHA1.
jq -r '.required[]?' <<<"$deploy" | while IFS= read -r sha; do
  path=$(jq -r --arg sha "$sha" 'to_entries | map(select(.value == $sha)) | .[0].key' <<<"$manifest")
  curl --fail-with-body -sS -o /dev/null "${auth[@]}" -X PUT \
    -H "Content-Type: application/octet-stream" \
    --data-binary "@.${path}" "${api}/deploys/${deploy_id}/files${path}"
  echo "Uploaded ${path}"
done

# 3. Wait for Netlify to finish processing.
for _ in $(seq 1 60); do
  deploy=$(curl --fail-with-body -sS "${auth[@]}" "${api}/deploys/${deploy_id}")
  state=$(jq -r '.state' <<<"$deploy")
  case "$state" in
    ready) break ;;
    error) echo "::error::Netlify deploy failed: $(jq -r '.error_message' <<<"$deploy")"; exit 1 ;;
  esac
  sleep 5
done
if [ "$state" != "ready" ]; then
  echo "::error::Netlify deploy did not become ready (last state: ${state})"
  exit 1
fi

if [ "$draft" = true ]; then
  url=$(jq -r '.deploy_ssl_url' <<<"$deploy")
  label="Preview (draft deploy, live site unchanged)"
else
  url=$(jq -r '.ssl_url' <<<"$deploy")
  label="Production"
fi
echo "${label}: ${url}"
if [ -n "${GITHUB_STEP_SUMMARY:-}" ]; then
  printf '### pkgdown site\n\n%s: %s\n' "$label" "$url" >> "$GITHUB_STEP_SUMMARY"
fi
