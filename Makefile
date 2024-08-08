.PHONY: build
build:
	dune build

.PHONY: clean
clean:
	dune clean

.PHONY: install
install: build
	scp _build/default/bin/control.exe gw:/tmp/control.exe
	ssh gw chmod 755 /tmp/control.exe
	ssh gw mv /tmp/control.exe /var/www/dlink/cgi-bin/control.exe
	scp -O scripts/update.sh gw:/var/www/dlink/scripts/update.sh_tmp
	ssh gw mv /var/www/dlink/scripts/update.sh_tmp /var/www/dlink/scripts/update.sh
	scp icons/* gw:/var/www/dlink/icons
	#scp -O scripts/update.sh root@10.0.1.66:
