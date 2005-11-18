with Ada.Containers.Doubly_Linked_Lists;

private package Archicheck.Dependency_List is
  new Ada.Containers.Doubly_Linked_Lists (Dependency);
