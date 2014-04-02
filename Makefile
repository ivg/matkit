



debug:
	TARGET=d.byte make -e -C src matkit


release:
	TARGET=native make -e -C src matkit


clean:
	make -C src clean