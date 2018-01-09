Limitations
===========

- only the first compilation unit in Ada and Java files is taken into account.
  As an optimisation, the lexer exit as soon as a compilation unit and
  it's dependencies are identified.

- do not analyze C dependencies embedded in code, only "official" dependencies
  created by (depending on the language) "with", "use", "import", etc.

- `Unrecognized character '@'` :  
the Java lexer do not recognize @ character in code. It's Ok in comments, and so no problem with Javadoc tags, but it raises an `OpenToken.Syntax_Error` exception when met in code.  
So, annotations like `@Override` cause the analysis interruption.

Known Bugs
==========


