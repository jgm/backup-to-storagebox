backup-to-storagebox: backup-to-storagebox.o

backup-to-storagebox.c: backup-to-storagebox.sh
	echo "#include <stdio.h>" > $@
	echo "#include <stdlib.h>" >> $@
	echo "" >> $@
	xxd -i $< >> $@
	echo "" >> $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  system((char *)backup_to_storagebox_sh);" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

clean:
	-rm backup-to-storagebox.{c,o}

install: backup-to-storagebox
	cp $< $$HOME/.local/bin/

.PHONY: clean install
