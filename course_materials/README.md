# Workshop Start Here

This `course_materials` folder is the master workshop copy and is read-only.

Do not work directly in this folder.

Instead, copy the whole folder into your own space under `my_work/YOUR_NAME`, then open the copied project and run scripts there.

## What to do

1. Open the Terminal tab in RStudio.

2. Run this command, replacing `YOUR_NAME` with your actual name:

```bash
mkdir -p /home/user11/my_work/YOUR_NAME && cp -R /home/user11/course_materials /home/user11/my_work/YOUR_NAME/
```

Example:

```bash
mkdir -p /home/user11/my_work/iandurbach && cp -R /home/user11/course_materials /home/user11/my_work/iandurbach/
```

3. In the Files pane, browse to:

```text
/home/user11/my_work/YOUR_NAME/course_materials
```

4. Open `scr_workshop.Rproj`.

5. When RStudio asks whether to switch to this project, click Yes.

6. Open this script first:

```text
namkha_basic/code/01_make_capthist.R
```

## Important

- Work only inside your copied folder under `my_work/YOUR_NAME/course_materials`.
- Do not save files into `/home/user11/course_materials`, because that folder is read-only.
- If you reopen the workshop later, reopen the project file in your own copied folder, not the read-only master copy.
