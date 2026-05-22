#!/usr/bin/env bash
set -euo pipefail

DROPLET_IP="157.230.241.19"
SSH_USER="root"
SSH_KEY="$HOME/.ssh/workshop_deploy_key"
REMOTE_DIR="/scr-workshop"

REMOTE_HOST="${SSH_USER}@${DROPLET_IP}"

require_file() {
  local path="$1"
  local message="$2"
  if [[ ! -f "${path}" ]]; then
    echo "Error: ${message}" >&2
    exit 1
  fi
}

require_dir() {
  local path="$1"
  local message="$2"
  if [[ ! -d "${path}" ]]; then
    echo "Error: ${message}" >&2
    exit 1
  fi
}

echo "Checking deployment prerequisites..."
require_file "${SSH_KEY}" "SSH key not found at ${SSH_KEY}."
require_dir "course_materials" "Course materials directory not found at course_materials/."
require_dir "my_work" "Participant work directory not found at my_work/."
require_file "docker-compose.yml" "docker-compose.yml not found in the current directory."
require_file ".env.example" ".env.example not found in the current directory."

echo "Ensuring remote deployment directory exists at ${REMOTE_DIR}..."
ssh -i "${SSH_KEY}" "${REMOTE_HOST}" "mkdir -p '${REMOTE_DIR}' '${REMOTE_DIR}/course_materials' '${REMOTE_DIR}/my_work'"

echo "Uploading container configuration..."
deploy_files=(docker-compose.yml .env.example)

if [[ -f ".env" ]]; then
  echo "Uploading .env along with compose configuration..."
  deploy_files+=(.env)
else
  echo "No local .env found. Remote host must already have a .env with runtime secrets."
fi

rsync -avz -e "ssh -i ${SSH_KEY}" "${deploy_files[@]}" "${REMOTE_HOST}:${REMOTE_DIR}/"

echo "Refreshing container from published image..."
ssh -i "${SSH_KEY}" "${REMOTE_HOST}" "cd '${REMOTE_DIR}' && docker compose pull && docker compose up -d"

echo "Container deployment complete."
