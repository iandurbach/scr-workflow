# syntax=docker/dockerfile:1.7

FROM rocker/geospatial:4.5.3

ARG QUARTO_VERSION=1.6.43
ARG TARGETARCH

ENV DEBIAN_FRONTEND=noninteractive \
    ROOT=false \
    COURSE_MATERIALS_DIR=/srv/workshop/course_materials \
    PARTICIPANT_WORK_DIR=/srv/workshop/my_work

RUN apt-get update \
 && apt-get install -y --no-install-recommends curl ca-certificates \
 && rm -rf /var/lib/apt/lists/*

RUN mkdir -p "${COURSE_MATERIALS_DIR}" "${PARTICIPANT_WORK_DIR}" \
 && for index in $(seq -w 1 50); do \
      username="user${index}"; \
      useradd -m -s /bin/bash -G staff "${username}"; \
      passwd -l "${username}"; \
      ln -sfn "${COURSE_MATERIALS_DIR}" "/home/${username}/course_materials"; \
      ln -sfn "${PARTICIPANT_WORK_DIR}/${username}" "/home/${username}/my_work"; \
      rm -f "/home/${username}/.RData" "/home/${username}/.Rhistory"; \
      chown -h "${username}:${username}" "/home/${username}/course_materials" "/home/${username}/my_work"; \
      chown "${username}:${username}" "/home/${username}"; \
    done

# Shared workshop accounts should start clean every time:
# avoid restoring someone else's workspace, avoid saving .RData on exit,
# and force participants to work from scripts copied into my_work.
RUN mkdir -p /etc/rstudio \
 && cat <<'EOF' >/etc/rstudio/rsession.conf
# Shared account workshop setting: do not save the last session state.
# This avoids restoring someone else's workspace on the next login.
session-save-action-default=no
EOF

# Global R defaults for a shared login:
# avoid loading or saving hidden workspace files and push participants
# toward script-based work in their own my_work folder.
RUN cat <<'EOF' >/usr/local/lib/R/etc/Rprofile.site
# Shared account workshop defaults:
# - avoid restoring someone else's workspace
# - avoid saving .RData on exit
# - force participants to work from scripts instead of hidden session state
options(
  save.workspace = FALSE,
  restoreWorkspace = FALSE
)
EOF

COPY docker/install-packages.R /tmp/install-packages.R
RUN Rscript /tmp/install-packages.R \
 && rm /tmp/install-packages.R

RUN case "${TARGETARCH}" in \
      amd64) quarto_arch="amd64" ;; \
      arm64) quarto_arch="arm64" ;; \
      *) echo "Unsupported TARGETARCH: ${TARGETARCH}" >&2; exit 1 ;; \
    esac \
 && if command -v quarto >/dev/null 2>&1 && [ "$(quarto --version)" = "${QUARTO_VERSION}" ]; then exit 0; fi \
 && curl -fsSL -o /tmp/quarto.tar.gz \
      "https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-linux-${quarto_arch}.tar.gz" \
 && rm -rf "/opt/quarto-${QUARTO_VERSION}" \
 && tar -C /opt -xzf /tmp/quarto.tar.gz \
 && ln -sfn "/opt/quarto-${QUARTO_VERSION}/bin/quarto" /usr/local/bin/quarto \
 && quarto check install \
 && rm /tmp/quarto.tar.gz

COPY docker/provision-workshop-users.sh /etc/cont-init.d/40-provision-workshop-users

RUN chmod +x /etc/cont-init.d/40-provision-workshop-users \
 && chmod 0775 "${PARTICIPANT_WORK_DIR}"

WORKDIR /home/user01

EXPOSE 8787

HEALTHCHECK --interval=30s --timeout=10s --start-period=60s --retries=5 \
  CMD curl -fsS http://127.0.0.1:8787/ >/dev/null || exit 1
