# Workshop Start Here

This `course_materials` folder is the master workshop copy and is read-only.

Do not work directly in this folder.

Instead, copy the whole folder into your own `my_work` space, then open the copied project and run scripts there.

## What to do

1. Open the Terminal tab in RStudio.

2. Run this command:

```bash
cp -R -L ~/course_materials ~/my_work/
```

This creates your own working copy here:

```text
~/my_work/course_materials
```

3. In the Files pane, browse to:

```text
~/my_work/course_materials
```

4. Open `scr_workshop.Rproj`.

5. When RStudio asks whether to switch to this project, click Yes.

6. Open this script first:

```text
namkha_basic/code/01_make_capthist.R
```

## Important

- Work only inside your copied folder under `~/my_work/course_materials`.
- Do not save files into `~/course_materials`, because that folder is read-only.
- If you reopen the workshop later, reopen the project file in your own copied folder, not the read-only master copy.
- To restore `~/my_work/course_materials` to its original state run these two commands in the terminal:

```bash
rm -rf ~/my_work/*
cp -rL ~/course_materials ~/my_work/
```
