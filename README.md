# Workshop

This directory is intended to become the standalone root of the RStudio workshop project.

## Layout

```text
workshop/
  Dockerfile
  docker-compose.yml
  docker-compose.local.yml
  .env.example
  .gitignore
  .dockerignore
  docker/
  scripts/
  course_materials/
  my_work/
```

- `course_materials/` is mounted read-only into `/srv/workshop/course_materials`.
- `my_work/` is mounted read-write into `/srv/workshop/my_work`.
- Each workshop user has `~/course_materials` and `~/my_work` links in their home directory.
- Participants should copy the full `course_materials/` folder into their own `~/my_work/course_materials` workspace and work only from that copied workspace.

## Shared-login safeguards

The image is configured with real Linux users `user01` through `user50`:

- RStudio does not save or restore the last session.
- Global R startup disables `.RData` restore/save.
- Each user has an independent writable `~/my_work` area.

These settings reduce cross-participant contamination and force script-based work rather than hidden workspace state.

## Participant workflow

1. Log in as your assigned account, e.g. `user01`.
2. Open `~/course_materials`.
3. Copy the full `~/course_materials` folder into `~/my_work/`.
4. Open and run scripts only from `~/my_work/course_materials`.
5. Do not save files into `course_materials`, because it is read-only.

## Local testing

Copy the example environment file:

```bash
cp .env.example .env
```

Build and start locally:

```bash
docker compose -f docker-compose.yml -f docker-compose.local.yml up -d
```

Open `http://localhost:8787`.
Log in as `user01` through `user50`.

If you update the published image on a server, also copy the updated `docker-compose.yml` there before restarting the container. The image alone is not enough, because the canonical mount paths and default login user are defined in Compose.

## Material-only updates

Edit files under `course_materials/`, then run:

```bash
scripts/deploy_materials.sh
```

## Image or environment updates

Build and push the published image:

```bash
docker buildx build --platform linux/amd64 -t iandurbach/secr-workflow-rstudio:latest --push .
```

Then update the server:

```bash
ssh -i ~/.ssh/workshop_deploy_key root@157.230.241.19
cd /scr-workshop
docker compose pull
docker compose down
docker compose up -d
```
