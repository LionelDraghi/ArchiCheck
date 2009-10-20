# common tasks; see subdirectories for Ada builds

ZIP_VERSION := 3.1a

.PHONY : zip force

source-clean ::
	-find ../ -name "*~" -print | xargs rm -v
	-find ../ -name ".#*" -print | xargs rm -v
	-find ../ -name "*,t" -print | xargs rm -v

linux-clean :
	make -C linux_release distclean
	make -C linux_debug distclean

zip : linux-clean source-clean gzipfile zipfile

gzipfile : force
	tar -zcf opentoken-$(ZIP_VERSION).tar.gz -C ../.. --exclude=_MTN --exclude=*.tar.gz --exclude=*.zip --no-anchor opentoken

zipfile : force
	cd ../..; zip -q -r opentoken/Build/opentoken-$(ZIP_VERSION).zip opentoken -x "opentoken/_MTN/*" -x "*.tar.gz" -x "*.zip"

tag :
	mtn tag -h:org.opentoken version_$(ZIP_VERSION)

# end of file
