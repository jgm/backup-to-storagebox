backup: backup.o

backup.c: backup.sh
	echo "#include <stdio.h>" > $@
	echo "#include <stdlib.h>" >> $@
	echo "" >> $@
	xxd -i backup.sh >> $@
	echo "" >> $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  system((char *)backup_sh);" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

clean:
	-rm backup.c

install: backup
	cp $< $$HOME/.local/bin/

.PHONY: clean install
