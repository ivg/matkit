
JOBS=4
TARGET=d.byte



matkit:
	corebuild -j $(JOBS) matkit.$(TARGET)

test:
	corebuild -j $(JOBS) tests/test_parsing.$(TARGET) --

clean:
	corebuild -clean