.PHONY: build
build:
	dune build

.PHONY: clean
clean:
	dune clean

.PHONY: install
install: build
	scp _build/default/bin/dlink_control.exe gw:/var/www/dlink/cgi-bin
	ssh gw chmod 755 /var/www/dlink/cgi-bin/dlink_control.exe
