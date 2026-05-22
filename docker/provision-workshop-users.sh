#!/usr/bin/with-contenv bash
set -euo pipefail

course_materials_dir="${COURSE_MATERIALS_DIR:-/srv/workshop/course_materials}"
participant_work_dir="${PARTICIPANT_WORK_DIR:-/srv/workshop/my_work}"
workshop_password="${WORKSHOP_PASSWORD:?Set WORKSHOP_PASSWORD before starting the workshop container}"

mkdir -p "${course_materials_dir}" "${participant_work_dir}"
chmod 0775 "${participant_work_dir}"

for index in $(seq -w 1 50); do
  username="user${index}"
  home_dir="/home/${username}"

  if ! id -u "${username}" >/dev/null 2>&1; then
    echo "Expected build-time workshop user missing: ${username}" >&2
    exit 1
  fi

  mkdir -p "${participant_work_dir}/${username}"
  echo "${username}:${workshop_password}" | chpasswd
  ln -sfn "${course_materials_dir}" "${home_dir}/course_materials"
  ln -sfn "${participant_work_dir}/${username}" "${home_dir}/my_work"
  rm -f "${home_dir}/.RData" "${home_dir}/.Rhistory"
  chown -h "${username}:${username}" "${home_dir}/course_materials" "${home_dir}/my_work"
  chown "${username}:${username}" "${home_dir}"
  chown -R "${username}:${username}" "${participant_work_dir}/${username}"
done
