
# ZipAda code test suite



##  ZipAda code test suite / -lf test

  > archicheck -lf -r -I ./zip-ada

  Expected :

```
zip-ada/extras/lzma_dec.adb
zip-ada/extras/dual_io.adb
zip-ada/extras/win32-crt-stat.ads
zip-ada/extras/lzh.adb
zip-ada/extras/random_data.adb
zip-ada/extras/lzma_enc.adb
zip-ada/extras/ada_directories_extensions.ads
zip-ada/extras/lzh.ads
zip-ada/extras/ada_directories_extensions.adb
zip-ada/extras/win32-crt-types.ads
zip-ada/extras/win32-crt-time.ads
zip-ada/extras/lz_scramble.adb
zip-ada/extras/bunzip.adb
zip-ada/extras/flexible_temp_files.ads
zip-ada/extras/lz.adb
zip-ada/extras/win32-crt.ads
zip-ada/extras/win32.adb
zip-ada/extras/win32.ads
zip-ada/extras/dual_io.ads
zip-ada/extras/lz_scramble.ads
zip-ada/extras/win32-crt-utime.ads
zip-ada/extras/flexible_temp_files.adb
zip-ada/tools/my_resolve_conflict.adb
zip-ada/tools/unzipada.adb
zip-ada/tools/comp_zip.adb
zip-ada/tools/my_get_password.adb
zip-ada/tools/rezip.adb
zip-ada/tools/rezip_lib.ads
zip-ada/tools/summary.ads
zip-ada/tools/rezip_lib.adb
zip-ada/tools/summary.adb
zip-ada/tools/my_dots.ads
zip-ada/tools/find_zip.adb
zip-ada/tools/my_tell_data.adb
zip-ada/tools/my_feedback.adb
zip-ada/tools/zipada.adb
zip-ada/tools/comp_zip_prc.adb
zip-ada/zip_lib/zip-compress.ads
zip-ada/zip_lib/zip-crc_crypto.adb
zip-ada/zip_lib/unzip-decompress-huffman.adb
zip-ada/zip_lib/zip.adb
zip-ada/zip_lib/zip-compress-reduce.ads
zip-ada/zip_lib/zip-compress-lzma_e.adb
zip-ada/zip_lib/zip-create.ads
zip-ada/zip_lib/lzma.ads
zip-ada/zip_lib/lzma-decoding.ads
zip-ada/zip_lib/zip-compress-shrink.ads
zip-ada/zip_lib/zip-crc_crypto.ads
zip-ada/zip_lib/zip-compress-deflate.adb
zip-ada/zip_lib/zip-compress-deflate.ads
zip-ada/zip_lib/unzip-decompress-huffman.ads
zip-ada/zip_lib/bzip2_decoding.ads
zip-ada/zip_lib/length_limited_huffman_code_lengths.adb
zip-ada/zip_lib/zip_streams.ads
zip-ada/zip_lib/unzip.adb
zip-ada/zip_lib/zip-headers.adb
zip-ada/zip_lib/zip_streams.adb
zip-ada/zip_lib/zip-compress.adb
zip-ada/zip_lib/zip-compress-reduce.adb
zip-ada/zip_lib/zip-compress-lzma_e.ads
zip-ada/zip_lib/zip.ads
zip-ada/zip_lib/unzip-decompress.adb
zip-ada/zip_lib/lz77.adb
zip-ada/zip_lib/unzip.ads
zip-ada/zip_lib/bzip2_decoding.adb
zip-ada/zip_lib/lz77.ads
zip-ada/zip_lib/zip-headers.ads
zip-ada/zip_lib/lzma-encoding.ads
zip-ada/zip_lib/zip-compress-shrink.adb
zip-ada/zip_lib/unzip-streams.ads
zip-ada/zip_lib/unzip-streams.adb
zip-ada/zip_lib/zip-create.adb
zip-ada/zip_lib/zip-compress__no_unsigned_64.adb
zip-ada/zip_lib/lzma-decoding.adb
zip-ada/zip_lib/lzma-encoding.adb
zip-ada/zip_lib/length_limited_huffman_code_lengths.ads
zip-ada/zip_lib/unzip-decompress.ads
zip-ada/test/test_stream_performance.adb
zip-ada/test/tb_wrap.adb
zip-ada/test/several_sizes.adb
zip-ada/test/test_unz_streams.adb
zip-ada/test/test_extract_tb.ads
zip-ada/test/test_lz_scramble.adb
zip-ada/test/uza_gnat.ads
zip-ada/test/test_extract.adb
zip-ada/test/test_lz77.adb
zip-ada/test/rw_file.adb
zip-ada/test/tb_wrap.ads
zip-ada/test/test_chunk.adb
zip-ada/test/fuzzip.adb
zip-ada/test/ziptest.adb
zip-ada/test/rw_file.ads
zip-ada/test/rz_gnat.ads
zip-ada/test/za_gnat.ads
zip-ada/test/lz77_stats.adb
zip-ada/test/test_llhc.adb
zip-ada/test/test_zip_info_timing.adb
zip-ada/test/demo_csv_gnat.ads
zip-ada/demo/demo_zip.adb
zip-ada/demo/demo_csv_into_zip.adb
zip-ada/demo/demo_unzip.adb
zip-ada/demo/zip_with_many_files.adb
```


 ZipAda code test suite / -lf test [Successful](tests_status.md#successful)

##  ZipAda code test suite / -ld test

  > archicheck -ld -r -I ./zip-ada

  Expected :

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
bunzip procedure body depends on Ada.Streams.Stream_IO 
bunzip procedure body depends on Ada.Text_IO 
bunzip procedure body depends on BZip2_Decoding 
bunzip procedure body depends on Interfaces 
BZip2_Decoding package body depends on Ada.Unchecked_Deallocation 
BZip2_Decoding package spec depends on Interfaces 
Comp_Zip_Prc procedure body depends on Ada.Characters.Handling 
Comp_Zip_Prc procedure body depends on Ada.Integer_Text_IO 
Comp_Zip_Prc procedure body depends on Ada.Text_IO 
Comp_Zip_Prc procedure body depends on UnZip.Streams 
Comp_Zip_Prc procedure body depends on Zip 
Comp_Zip procedure body depends on Ada.Command_Line 
Comp_Zip procedure body depends on Ada.Text_IO 
Comp_Zip procedure body depends on Comp_Zip_Prc 
Comp_Zip procedure body depends on UnZip 
Comp_Zip procedure body depends on Zip 
Demo_CSV_GNAT procedure body depends on Demo_csv_into_zip 
Demo_CSV_GNAT procedure body depends on TB_Wrap 
Demo_csv_into_zip procedure body depends on Ada.Characters.Handling 
Demo_csv_into_zip procedure body depends on Ada.Text_IO 
Demo_csv_into_zip procedure body depends on Zip.Create 
Demo_csv_into_zip procedure body depends on Zip_Streams 
Demo_UnZip procedure body depends on Ada.Text_IO 
Demo_UnZip procedure body depends on Demo_zip 
Demo_UnZip procedure body depends on UnZip.Streams 
Demo_zip procedure body depends on Zip.Create 
Demo_zip procedure body depends on Zip_Streams 
Dual_IO package spec depends on IO_Exceptions 
Dual_IO package spec depends on Text_IO 
Find_Zip procedure body depends on Ada.Characters.Handling 
Find_Zip procedure body depends on Ada.Command_Line 
Find_Zip procedure body depends on Ada.Integer_Text_IO 
Find_Zip procedure body depends on Ada.Streams 
Find_Zip procedure body depends on Ada.Text_IO 
Find_Zip procedure body depends on UnZip.Streams 
Find_Zip procedure body depends on Zip 
Flexible_temp_files package spec depends on Ada.Text_IO 
Fuzzip procedure body depends on Ada.Command_Line 
Fuzzip procedure body depends on Ada.Numerics.Float_Random 
Fuzzip procedure body depends on Ada.Strings.Unbounded 
Fuzzip procedure body depends on Ada.Text_IO 
Fuzzip procedure body depends on RW_File 
Fuzzip procedure body depends on UnZip.Streams 
Fuzzip procedure body depends on Zip 
Fuzzip procedure body depends on Zip.Compress 
Fuzzip procedure body depends on Zip.Create 
Fuzzip procedure body depends on Zip_Streams 
LZ77 package body depends on Ada.Unchecked_Deallocation 
LZ77 package body depends on Interfaces 
LZ77 package body depends on System 
LZ77 package spec depends on Interfaces 
LZ77_Stats procedure body depends on Ada.Integer_Text_IO 
LZ77_Stats procedure body depends on Ada.Text_IO 
LZH package spec depends on Interfaces 
LZMA.Decoding package body depends on Ada.Exceptions 
LZMA.Decoding package body depends on Ada.Unchecked_Deallocation 
LZMA.Decoding package spec depends on Ada.Direct_IO 
LZMA.Decoding package spec depends on Interfaces 
LZMA_Dec procedure body depends on Ada.Command_Line 
LZMA_Dec procedure body depends on Ada.Streams.Stream_IO 
LZMA_Dec procedure body depends on Ada.Text_IO 
LZMA_Dec procedure body depends on Interfaces 
LZMA_Dec procedure body depends on LZMA.Decoding 
LZMA.Encoding package body depends on Ada.Streams.Stream_IO 
LZMA.Encoding package body depends on Ada.Unchecked_Deallocation 
LZMA.Encoding package body depends on Interfaces 
LZMA.Encoding package body depends on LZ77 
LZMA.Encoding package spec depends on Interfaces 
LZMA_Enc procedure body depends on Ada.Command_Line 
LZMA_Enc procedure body depends on Ada.Streams.Stream_IO 
LZMA_Enc procedure body depends on Ada.Text_IO 
LZMA_Enc procedure body depends on Interfaces 
LZMA_Enc procedure body depends on LZMA.Encoding 
LZMA package spec depends on Interfaces 
LZMA package spec depends on System 
LZ procedure body depends on Ada.Calendar 
LZ procedure body depends on Ada.Command_Line 
LZ procedure body depends on Ada.Direct_IO 
LZ procedure body depends on Ada.Text_IO 
LZ procedure body depends on Interfaces 
LZ procedure body depends on LZH 
LZ_Scramble function body depends on Ada.Numerics.Discrete_Random 
LZ_Scramble function body depends on Ada.Numerics.Float_Random 
My_feedback procedure body depends on Ada.Text_IO 
My_feedback procedure body depends on My_dots 
My_get_password procedure body depends on Ada.Strings.Unbounded 
My_get_password procedure body depends on Ada.Text_IO 
My_resolve_conflict procedure body depends on Ada.Text_IO 
My_resolve_conflict procedure body depends on UnZip 
My_resolve_conflict procedure body depends on Zip 
My_tell_data procedure body depends on Ada.Text_IO 
My_tell_data procedure body depends on Interfaces 
My_tell_data procedure body depends on My_dots 
My_tell_data procedure body depends on Summary 
My_tell_data procedure body depends on UnZip 
Random_Data procedure body depends on Ada.Command_Line 
Random_Data procedure body depends on Ada.Numerics.Discrete_Random 
Random_Data procedure body depends on Ada.Sequential_IO 
Rezip_lib package body depends on Ada.Calendar 
Rezip_lib package body depends on Ada.Characters.Handling 
Rezip_lib package body depends on Ada.Directories 
Rezip_lib package body depends on Ada.Float_Text_IO 
Rezip_lib package body depends on Ada.Integer_Text_IO 
Rezip_lib package body depends on Ada.Numerics.Discrete_Random 
Rezip_lib package body depends on Ada.Streams.Stream_IO 
Rezip_lib package body depends on Ada.Strings.Fixed 
Rezip_lib package body depends on Ada.Strings.Unbounded 
Rezip_lib package body depends on Ada.Text_IO 
Rezip_lib package body depends on Ada.Unchecked_Deallocation 
Rezip_lib package body depends on Dual_IO 
Rezip_lib package body depends on Flexible_temp_files 
Rezip_lib package body depends on GNAT.OS_Lib 
Rezip_lib package body depends on Interfaces 
Rezip_lib package body depends on My_feedback 
Rezip_lib package body depends on UnZip 
Rezip_lib package body depends on Zip.Compress 
Rezip_lib package body depends on Zip.Create 
Rezip_lib package body depends on Zip.Headers 
Rezip_lib package body depends on Zip_Streams 
Rezip_lib package spec depends on Zip 
ReZip procedure body depends on Ada.Characters.Handling 
ReZip procedure body depends on Ada.Command_Line 
ReZip procedure body depends on Ada.Text_IO 
ReZip procedure body depends on Comp_Zip_Prc 
ReZip procedure body depends on Rezip_lib 
ReZip procedure body depends on Zip 
RW_File package body depends on Ada.Sequential_IO 
RW_File package spec depends on Ada.Strings.Unbounded 
RZ_GNAT procedure body depends on ReZip 
RZ_GNAT procedure body depends on TB_Wrap 
Several_sizes procedure body depends on Ada.Command_Line 
Several_sizes procedure body depends on Ada.Integer_Text_IO 
Several_sizes procedure body depends on Ada.Streams.Stream_IO 
Several_sizes procedure body depends on Ada.Text_IO 
Several_sizes procedure body depends on Interfaces 
Summary package body depends on Zip 
Summary package spec depends on UnZip 
TB_Wrap procedure body depends on Ada.Exceptions 
TB_Wrap procedure body depends on Ada.Text_IO 
TB_Wrap procedure body depends on GNAT.Traceback.Symbolic 
Test_Chunk procedure body depends on Ada.IO_Exceptions 
Test_Chunk procedure body depends on Ada.Streams.Stream_IO 
Test_Chunk procedure body depends on Ada.Text_IO 
Test_Chunk procedure body depends on UnZip.Streams 
Test_Extract procedure body depends on UnZip 
Test_Extract_TB procedure body depends on TB_Wrap 
Test_Extract_TB procedure body depends on Test_Extract 
Test_LLHC procedure body depends on Length_limited_Huffman_code_lengths 
Test_LLHC procedure body depends on Text_IO 
Test_LZ77 procedure body depends on Ada.Characters.Handling 
Test_LZ77 procedure body depends on Ada.Command_Line 
Test_LZ77 procedure body depends on Ada.Sequential_IO 
Test_LZ77 procedure body depends on Ada.Text_IO 
Test_LZ77 procedure body depends on LZ77 
Test_LZ_Scramble procedure body depends on Ada.Text_IO 
Test_LZ_Scramble procedure body depends on LZ_Scramble 
Test_Stream_Performance procedure body depends on Ada.Calendar 
Test_Stream_Performance procedure body depends on Ada.Command_Line 
Test_Stream_Performance procedure body depends on Ada.Streams.Stream_IO 
Test_Stream_Performance procedure body depends on Ada.Text_IO 
Test_Stream_Performance procedure body depends on Ada.Unchecked_Conversion 
Test_Stream_Performance procedure body depends on Interfaces 
Test_UnZ_Streams procedure body depends on Ada.Streams.Stream_IO 
Test_UnZ_Streams procedure body depends on Ada.Text_IO 
Test_UnZ_Streams procedure body depends on UnZip.Streams 
Test_UnZ_Streams procedure body depends on Zip 
Test_Zip_Info_timing procedure body depends on Ada.Calendar 
Test_Zip_Info_timing procedure body depends on Ada.Text_IO 
Test_Zip_Info_timing procedure body depends on Zip 
UnZipAda procedure body depends on Ada.Calendar 
UnZipAda procedure body depends on Ada.Characters.Handling 
UnZipAda procedure body depends on Ada.Command_Line 
UnZipAda procedure body depends on Ada.Directories 
UnZipAda procedure body depends on Ada_Directories_Extensions 
UnZipAda procedure body depends on Ada.Float_Text_IO 
UnZipAda procedure body depends on Ada.Numerics.Elementary_Functions 
UnZipAda procedure body depends on Ada.Text_IO 
UnZipAda procedure body depends on Interfaces 
UnZipAda procedure body depends on My_feedback 
UnZipAda procedure body depends on My_get_password 
UnZipAda procedure body depends on My_resolve_conflict 
UnZipAda procedure body depends on My_tell_data 
UnZipAda procedure body depends on Summary 
UnZipAda procedure body depends on UnZip 
UnZipAda procedure body depends on Zip 
UnZip.Decompress.Huffman package body depends on Ada.Text_IO 
UnZip.Decompress.Huffman package body depends on Ada.Unchecked_Deallocation 
UnZip.Decompress.Huffman package body depends on Interfaces 
UnZip.Decompress package body depends on Ada.Exceptions 
UnZip.Decompress package body depends on Ada.Streams.Stream_IO 
UnZip.Decompress package body depends on Ada.Text_IO 
UnZip.Decompress package body depends on BZip2_Decoding 
UnZip.Decompress package body depends on Interfaces 
UnZip.Decompress package body depends on LZMA.Decoding 
UnZip.Decompress package body depends on UnZip.Decompress.Huffman 
UnZip.Decompress package body depends on Zip.CRC_Crypto 
UnZip.Decompress package spec depends on Ada.Strings.Unbounded 
UnZip.Decompress package spec depends on Zip.Headers 
UnZip.Decompress package spec depends on Zip_Streams 
UnZip package body depends on Ada.Exceptions 
UnZip package body depends on Ada.IO_Exceptions 
UnZip package body depends on Interfaces 
UnZip package body depends on UnZip.Decompress 
UnZip package body depends on Zip.Headers 
UnZip package body depends on Zip_Streams 
UnZip package spec depends on Ada.Calendar 
UnZip package spec depends on Ada.Streams 
UnZip package spec depends on Ada.Strings.Unbounded 
UnZip package spec depends on Zip 
UnZip.Streams package body depends on Ada.Exceptions 
UnZip.Streams package body depends on Ada.Strings.Unbounded 
UnZip.Streams package body depends on Ada.Unchecked_Deallocation 
UnZip.Streams package body depends on Interfaces 
UnZip.Streams package body depends on UnZip.Decompress 
UnZip.Streams package body depends on Zip.Headers 
UnZip.Streams package spec depends on Ada.IO_Exceptions 
UnZip.Streams package spec depends on Ada.Streams 
UnZip.Streams package spec depends on Zip 
UnZip.Streams package spec depends on Zip_Streams 
UZA_GNAT procedure body depends on TB_Wrap 
UZA_GNAT procedure body depends on UnZipAda 
Win32.crt.Stat package spec depends on Win32.crt.Types 
Win32.crt.Time package spec depends on Win32.crt.Types 
Win32.crt.Types package spec depends on Win32 
Win32.crt.Utime package spec depends on Win32.crt.Types 
Win32 package spec depends on Ada.Unchecked_Conversion 
Win32 package spec depends on Interfaces 
Win32 package spec depends on Interfaces.C 
Win32 package spec depends on Interfaces.C.Strings 
Win32 package spec depends on System 
ZA_GNAT procedure body depends on TB_Wrap 
ZA_GNAT procedure body depends on ZipAda 
ZipAda procedure body depends on Ada.Calendar 
ZipAda procedure body depends on Ada.Characters.Handling 
ZipAda procedure body depends on Ada.Command_Line 
ZipAda procedure body depends on Ada.Directories 
ZipAda procedure body depends on Ada.Float_Text_IO 
ZipAda procedure body depends on Ada.Strings.Fixed 
ZipAda procedure body depends on Ada.Strings.Unbounded 
ZipAda procedure body depends on Ada.Text_IO 
ZipAda procedure body depends on My_feedback 
ZipAda procedure body depends on Zip 
ZipAda procedure body depends on Zip.Compress 
ZipAda procedure body depends on Zip.Create 
ZipAda procedure body depends on Zip_Streams 
Zip.Compress.Deflate procedure body depends on Ada.Integer_Text_IO 
Zip.Compress.Deflate procedure body depends on Ada.Text_IO 
Zip.Compress.Deflate procedure body depends on Ada.Unchecked_Deallocation 
Zip.Compress.Deflate procedure body depends on Interfaces 
Zip.Compress.Deflate procedure body depends on Length_limited_Huffman_code_lengths 
Zip.Compress.Deflate procedure body depends on LZ77 
Zip.Compress.Deflate procedure body depends on Zip.CRC_Crypto 
Zip.Compress.Deflate procedure body depends on Zip.CRC_Crypto 
Zip.Compress.Deflate procedure body depends on Zip_Streams 
Zip.Compress.LZMA_E procedure body depends on Ada.Unchecked_Deallocation 
Zip.Compress.LZMA_E procedure body depends on Interfaces 
Zip.Compress.LZMA_E procedure body depends on LZMA.Encoding 
Zip.Compress.LZMA_E procedure body depends on Zip.CRC_Crypto 
Zip.Compress.LZMA_E procedure body depends on Zip.CRC_Crypto 
Zip.Compress package body depends on Ada.Characters.Handling 
Zip.Compress package body depends on Ada.Characters.Handling 
Zip.Compress package body depends on Ada.Numerics.Discrete_Random 
Zip.Compress package body depends on Ada.Numerics.Discrete_Random 
Zip.Compress package body depends on Ada.Strings.Fixed 
Zip.Compress package body depends on Ada.Strings.Fixed 
Zip.Compress package body depends on Zip.Compress.Deflate 
Zip.Compress package body depends on Zip.Compress.Deflate 
Zip.Compress package body depends on Zip.Compress.LZMA_E 
Zip.Compress package body depends on Zip.Compress.Reduce 
Zip.Compress package body depends on Zip.Compress.Reduce 
Zip.Compress package body depends on Zip.Compress.Shrink 
Zip.Compress package body depends on Zip.Compress.Shrink 
Zip.Compress package body depends on Zip.CRC_Crypto 
Zip.Compress package body depends on Zip.CRC_Crypto 
Zip.Compress package spec depends on Zip_Streams 
Zip.Compress.Reduce procedure body depends on Ada.Text_IO 
Zip.Compress.Reduce procedure body depends on Ada.Unchecked_Deallocation 
Zip.Compress.Reduce procedure body depends on Interfaces 
Zip.Compress.Reduce procedure body depends on LZ77 
Zip.Compress.Reduce procedure body depends on Zip.CRC_Crypto 
Zip.Compress.Reduce procedure body depends on Zip.CRC_Crypto 
Zip.Compress.Reduce procedure body depends on Zip_Streams 
Zip.Compress.Shrink procedure body depends on Ada.Unchecked_Deallocation 
Zip.Compress.Shrink procedure body depends on Interfaces 
Zip.Compress.Shrink procedure body depends on Zip.CRC_Crypto 
Zip.Compress.Shrink procedure body depends on Zip.CRC_Crypto 
Zip.Create package body depends on Ada.Exceptions 
Zip.Create package body depends on Ada.Text_IO 
Zip.Create package body depends on Ada.Unchecked_Deallocation 
Zip.Create package body depends on Interfaces 
Zip.Create package spec depends on Ada.Strings.Unbounded 
Zip.Create package spec depends on Zip.Compress 
Zip.Create package spec depends on Zip.Headers 
Zip.Create package spec depends on Zip_Streams 
Zip.Headers package spec depends on Interfaces 
Zip.Headers package spec depends on Zip_Streams 
Zip package body depends on Ada.Characters.Handling 
Zip package body depends on Ada.Exceptions 
Zip package body depends on Ada.IO_Exceptions 
Zip package body depends on Ada.Strings.Fixed 
Zip package body depends on Ada.Strings.Unbounded 
Zip package body depends on Ada.Unchecked_Deallocation 
Zip package body depends on Zip.Headers 
Zip package spec depends on Ada.Calendar 
Zip package spec depends on Ada.Streams.Stream_IO 
Zip package spec depends on Ada.Text_IO 
Zip package spec depends on Interfaces 
Zip package spec depends on System 
Zip package spec depends on Zip_Streams 
Zip_Streams package spec depends on Ada.Calendar 
Zip_Streams package spec depends on Ada.Streams 
Zip_Streams package spec depends on Ada.Streams.Stream_IO 
Zip_Streams package spec depends on Ada.Strings.Unbounded 
Zip_Streams package spec depends on Interfaces 
Zip_Streams package spec depends on System 
ZipTest procedure body depends on Ada.IO_Exceptions 
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


 ZipAda code test suite / -ld test [Successful](tests_status.md#successful)
