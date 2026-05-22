#!/usr/bin/env bash
set -euo pipefail

default_password="${WORKSHOP_PASSWORD:-workshop}"
users_csv="${WORKSHOP_USERS:-}"
user_count="${WORKSHOP_USER_COUNT:-0}"
user_prefix="${WORKSHOP_USERS_PREFIX:-participant}"
create_instructor="${WORKSHOP_CREATE_INSTRUCTOR:-false}"
instructor_user="${WORKSHOP_INSTRUCTOR_USER:-instructor}"
course_materials_dir="${COURSE_MATERIALS_DIR:-/home/rstudio/course_materials}"
participant_work_dir="${PARTICIPANT_WORK_DIR:-/home/rstudio/my_work}"

trim() {
  local value="$1"
  value="${value#"${value%%[![:space:]]*}"}"
  value="${value%"${value##*[![:space:]]}"}"
  printf '%s' "$value"
}

create_or_update_user() {
  local username="$1"
  local password="$2"
  local home_dir="/home/${username}"

  if ! id -u "${username}" >/dev/null 2>&1; then
    useradd -m -s /bin/bash -G staff "${username}"
  fi

  echo "${username}:${password}" | chpasswd

  mkdir -p "${course_materials_dir}" "${participant_work_dir}" "${participant_work_dir}/${username}"

  if [[ "${username}" != "rstudio" ]]; then
    ln -sfn "${course_materials_dir}" "${home_dir}/course_materials"
    ln -sfn "${participant_work_dir}/${username}" "${home_dir}/my_work"
  fi

  rm -f "${home_dir}/.RData" "${home_dir}/.Rhistory"
  chown -R "${username}:${username}" "${home_dir}" "${participant_work_dir}/${username}"
}

declare -a workshop_users=()

if [[ -n "${users_csv}" ]]; then
  IFS=',' read -r -a requested_users <<< "${users_csv}"
  for requested_user in "${requested_users[@]}"; do
    requested_user="$(trim "${requested_user}")"
    if [[ -n "${requested_user}" ]]; then
      workshop_users+=("${requested_user}")
    fi
  done
else
  if ! [[ "${user_count}" =~ ^[0-9]+$ ]] || [[ "${user_count}" -lt 0 ]]; then
    echo "Invalid WORKSHOP_USER_COUNT: ${user_count}" >&2
    exit 1
  fi

  if [[ "${user_count}" -gt 0 ]]; then
    width=2
    if [[ "${#user_count}" -gt "${width}" ]]; then
      width="${#user_count}"
    fi

    for index in $(seq 1 "${user_count}"); do
      printf -v username "%s%0*d" "${user_prefix}" "${width}" "${index}"
      workshop_users+=("${username}")
    done
  fi
fi

create_or_update_user "rstudio" "${PASSWORD:-${default_password}}"

chown -R rstudio:rstudio "${participant_work_dir}"
chmod 0775 "${participant_work_dir}"

for username in "${workshop_users[@]}"; do
  create_or_update_user "${username}" "${default_password}"
done

if [[ "${create_instructor}" == "true" ]]; then
  create_or_update_user "${instructor_user}" "${default_password}"
fi
