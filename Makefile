backup-to-storagebox: backup-to-storagebox.o

backup-to-storagebox.c: backup-to-storagebox.sh
	echo "#include <stdio.h>" > $@
	echo "#include <stdlib.h>" >> $@
	echo "#include <errno.h>" >> $@
	echo "#include <unistd.h>" >> $@
	echo "" >> $@
	xxd -i $< >> $@
	echo "" >> $@
	echo "int main(int argc, char *argv[])" >> $@
	echo "{" >> $@
	echo "  int status;" >> $@
	echo "  if(setenv(\"BACKUP_TO_STORAGEBOX_CONFIG\", argv[1], 1) != 0) {" >> $@
	echo "    perror(\"Unable to export argument as environment variable\");" >> $@
	echo "    _exit(1);" >> $@
	echo "  }" >> $@
	echo "  status = system((char *)backup_to_storagebox_sh);" >> $@
	echo "  return status;" >> $@
	echo "}" >> $@

clean:
	-rm backup-to-storagebox.{c,o}

install: backup-to-storagebox
	cp $< $$HOME/.local/bin/

.PHONY: clean install
