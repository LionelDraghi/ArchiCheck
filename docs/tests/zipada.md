
# ZipAda code test suite



##  ZipAda code test suite / -lf test

  > acc -lf -r -I zip-ada  

  Expected (102 files) :  

```  
zip-ada/demo/demo_csv_into_zip.adb
zip-ada/demo/demo_unzip.adb
zip-ada/demo/demo_zip.adb
...
zip-ada/zip_lib/zip.ads
zip-ada/zip_lib/zip_streams.adb
zip-ada/zip_lib/zip_streams.ads
```  


ZipAda code test suite / -lf test [Successful](zipada.md#zipada-code-test-suite---lf-test)

##  ZipAda code test suite / -ld test

  > acc -ld -r -I ./zip-ada  

  336 dependencies expected :  

```  
Ada_Directories_Extensions package body depends on Ada.Exceptions 
Ada_Directories_Extensions package body depends on Ada.Unchecked_Conversion 
Ada_Directories_Extensions package body depends on GNAT.Calendar 
Ada_Directories_Extensions package body depends on Interfaces.C.Strings 
Ada_Directories_Extensions package body depends on Win32.crt.Stat 
Ada_Directories_Extensions package body depends on Win32.crt.Time 
Ada_Directories_Extensions package body depends on Win32.crt.Utime 
Ada_Directories_Extensions package spec depends on Ada.Calendar 
Ada_Directories_Extensions package spec depends on Ada.Directories 
bunzip procedure body depends on Ada.Command_Line 
...
ZipTest procedure body depends on Ada.Strings.Unbounded 
ZipTest procedure body depends on Ada.Text_IO 
ZipTest procedure body depends on RW_File 
ZipTest procedure body depends on Zip.Compress 
ZipTest procedure body depends on Zip.Create 
ZipTest procedure body depends on Zip_Streams 
Zip_with_many_files procedure body depends on Ada.Strings.Fixed 
Zip_with_many_files procedure body depends on Zip.Compress 
Zip_with_many_files procedure body depends on Zip.Create 
Zip_with_many_files procedure body depends on Zip_Streams 
```  


ZipAda code test suite / -ld test [Successful](zipada.md#zipada-code-test-suite---ld-test)

##  ZipAda code test suite / rules test

  > acc zipadarules.txt -r -I ./zip-ada  

  Rules (not much to test...) :  

```  
Zip   may use Zip_Streams
UnZip may use Zip_Streams
```  

  No error expected  


ZipAda code test suite / rules test [Successful](zipada.md#zipada-code-test-suite--rules-test)
