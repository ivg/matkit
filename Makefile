



debug:
	TARGET=d.byte make -e -C src matkit


release:
	TARGET=native make -e -C src matkit

test:
	TARGET=d.byte make -e -C src test


clean:
	make -C src clean