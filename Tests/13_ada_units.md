## Feature: Ada units test suite

### Scenario: Ada compilation units unit test

procedure Sub.Test has several separate units :

  * procedure renaming
  * generic package renaming

  * package body
  * package specification
  * generic package

  * child procedure

  * separate procedure
  * separate private procedure
  * separate package
  * separate function
  * separate task
  * separate protected

> acc -ld -I 13_Ada_Units/src

Expected :

```
Enum_IO package spec depends on Ada.Text_IO.Enumeration_IO
New_Page procedure spec depends on Text_IO_New_Page
Rational_Io package spec depends on A4
Rational_Io package spec depends on Rational_Numbers.IO
Rational_Numbers.IO package spec depends on A2
Rational_Numbers package body depends on Rational_Numbers.Reduce
Rational_Numbers package spec depends on A5
Rational_Numbers.Reduce procedure body depends on A2
Rational_Numbers.Reduce procedure body depends on A3
Sub.Test.Get function body depends on A3
Sub.Test procedure body depends on New_Page
Sub.Test procedure body depends on Rational_IO
Sub.Test procedure body depends on Util.New_Page
Sub.Test.Put procedure body depends on A1
Sub.Test.Ressource protected body depends on A6
Sub.Test.Server task body depends on A7
Sub.Tools package spec depends on A2
Util.New_Page package spec depends on Interfaces.C
```

- Given I run `gnat make -q sub-test -I13_Ada_Units/src -D13_Ada_Units/src` Successfully
- Given there is a file `13_Ada_Units/expected_output.1`
- When I run `./acc -ld -I 13_Ada_Units/src` Successfully
- Then I get file (unordered) `13_Ada_Units/expected_output.1`
