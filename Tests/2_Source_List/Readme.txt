File: --list_files option test

This test check the file list extraction from simple Ada sources.

The command line is :
> ./archicheck -I SOURCES_DIRECTORY --list_files

The output will be of this kind :

(start code)

/home/lionel/Proj/ArchiCheck/Tests/Source_List/dir1/a.ads
/home/lionel/Proj/ArchiCheck/Tests/Source_List/dir1/a.adb
/home/lionel/Proj/ArchiCheck/Tests/Source_List/dir1/b.ads
/home/lionel/Proj/ArchiCheck/Tests/Source_List/dir1/c.ads
/home/lionel/Proj/ArchiCheck/Tests/Source_List/dir1/c-d.ads

(end)


