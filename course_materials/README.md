# Course Materials Workspace

This folder is the canonical project root for workshop materials on both local Docker runs and the RStudio server.

- `course_materials/` is mounted read-only into the container.
- Participants should copy this full folder into `my_work/YOUR_NAME/course_materials`.
- All scripts and tutorial outputs should be run from the copied workspace, not from the mounted read-only source.
- The `.here` file and `course_materials.Rproj` make `here()` resolve inside the copied workspace.
