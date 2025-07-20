backup-to-storage-box: backup-to-storage-box.o

backup-to-storage-box.c: backup-to-storage-box.sh
	echo "#include <stdio.h>" > $@
	echo "#include <stdlib.h>" >> $@
	echo "" >> $@
	xxd -i $< >> $@
	echo "" >> $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  system((char *)backup_to_storage_box_sh);" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

clean:
	-rm backup-to-storage-box.{c,o}

install: backup-to-storage-box
	cp $< $$HOME/.local/bin/

.PHONY: clean install
