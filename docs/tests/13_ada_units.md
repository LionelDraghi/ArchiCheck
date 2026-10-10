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

> acc -ld -I ../docs/tests/13_ada_units/src

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

- Given I run `gcc -c -gnatc -I../docs/tests/13_ada_units/src ../docs/tests/13_ada_units/src/rational_numbers.adb ../docs/tests/13_ada_units/src/rational_numbers-reduce.adb ../docs/tests/13_ada_units/src/sub-put.adb ../docs/tests/13_ada_units/src/sub-test.adb ../docs/tests/13_ada_units/src/sub-test-get.adb ../docs/tests/13_ada_units/src/sub-test-put.adb ../docs/tests/13_ada_units/src/sub-test-ressource.adb ../docs/tests/13_ada_units/src/sub-test-server.adb ../docs/tests/13_ada_units/src/text_io_new_page.adb` Successfully
- Given there is a file `../docs/tests/13_ada_units/expected_output.1`
- When I run `./acc -ld -I ../docs/tests/13_ada_units/src` Successfully
- Then I get file (unordered) `../docs/tests/13_ada_units/expected_output.1`
