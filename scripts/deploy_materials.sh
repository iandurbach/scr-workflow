#!/usr/bin/env bash
set -euo pipefail

DROPLET_IP="157.230.241.19"
SSH_USER="root"
SSH_KEY="$HOME/.ssh/workshop_deploy_key"
REMOTE_DIR="/scr-workshop"

LOCAL_COURSE_MATERIALS_DIR="course_materials"
REMOTE_COURSE_MATERIALS_DIR="${REMOTE_DIR}/course_materials"
REMOTE_MY_WORK_DIR="${REMOTE_DIR}/my_work"
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
require_dir "${LOCAL_COURSE_MATERIALS_DIR}" "Course materials directory not found at ${LOCAL_COURSE_MATERIALS_DIR}/."

echo "Ensuring remote workshop directories exist under ${REMOTE_DIR}..."
ssh -i "${SSH_KEY}" "${REMOTE_HOST}" "mkdir -p '${REMOTE_COURSE_MATERIALS_DIR}' '${REMOTE_MY_WORK_DIR}'"

echo "Removing generated RStudio metadata from remote course materials..."
ssh -i "${SSH_KEY}" "${REMOTE_HOST}" "rm -rf '${REMOTE_COURSE_MATERIALS_DIR}/.Rproj.user' && find '${REMOTE_COURSE_MATERIALS_DIR}' -name .DS_Store -delete"

echo "Uploading course materials from ${LOCAL_COURSE_MATERIALS_DIR}/..."
rsync -avz \
  --exclude ".Rproj.user/" \
  --exclude ".DS_Store" \
  -e "ssh -i ${SSH_KEY}" \
  "${LOCAL_COURSE_MATERIALS_DIR}/" \
  "${REMOTE_HOST}:${REMOTE_COURSE_MATERIALS_DIR}/"

echo "Restarting running container without rebuilding or pulling..."
ssh -i "${SSH_KEY}" "${REMOTE_HOST}" "cd '${REMOTE_DIR}' && docker compose restart"

echo "Course materials deployed."
