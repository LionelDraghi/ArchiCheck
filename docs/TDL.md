ArchiCheck task and decision list <!-- omit from toc -->
=================================

This file records undecided feature ideas and pending work items
(cf. `docs/dev/development_workflow.md`, section 10). An entry is
removed when done, and user visible changes are recorded in
[the changelog](changelog.md). Known bugs and limitations are tracked
in [limitations.md](limitations.md).

## High priority

- **Allow specifying a component that contains all units from a folder**  
  Requested in
  [issue #4](https://github.com/LionelDraghi/ArchiCheck/issues/4):
  `Layer_A contains directory path/to/dir`, where the folder holds
  several Ada packages. Useful when the layering is already expressed
  by the file system hierarchy, saving rules file maintenance.
  The empty test suite `22_files_component.md` was reserving this
  slot: it has been removed, its scenarios being already covered by
  [03_dependency_list](features/03_dependency_list.md).

- **complete the globbing characters support**  
  The first implementation (0.6.0) only accepts a trailing `*`, as in
  `only Support_Layer may use Interfaces*`
  (cf. the [globbing test suite](../features/08_globbing_characters.md)).
  Generalizing the position of the `*` would allow rules such as
  `only *.Porting_Kit can use X`.

- **Ada: take `limited with` into account**  
  A `limited with` is currently processed as a plain `with`. Decide
  whether a limited view dependency should be excluded from the
  analysis, or reported as a warning.

- **Ada: process `pragma Import` as a dependency**  
  ```ada
  pragma Import (C, Get_File_Names_Case_Sensitive,
                 "__gnat_get_file_names_case_sensitive");
  ```

- **Ada: implement the `only X can use pragma import` rule**

- **Java: detect visibility through fully qualified names**  
  The `import` directive is not mandatory in Java to use a class: a
  fully qualified name is enough. Also tracked in
  [limitations.md](limitations.md) and
  [languages_concepts.md](languages_concepts.md).

## Low priority

- **Ada: `use type` processing**  
  A `use type` clause implies a dependency on the package of the
  type, on the same grounds as a `with` clause.

- **Migrate the test architecture diagrams to mermaid**  
  The diagrams embedded in the test suites (batik.png, cp1-4.png,
  gc1-2.png, gtk.png) are Dia PNG exports whose `.dia` sources are
  not in the repository anymore; the scenarios reference the
  generated copies in `docs/features/`. Mermaid diagrams would
  be rendered natively by GitHub
  (cf. [05_layer_rule](../features/05_layer_rule.md)).

- **Build a PDF documentation based on the md files**
