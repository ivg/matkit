
JOBS=4
TARGET=d.byte



matkit:
	corebuild -j $(JOBS) matkit.$(TARGET)

test:
	corebuild -j $(JOBS) tests/test_parsing.$(TARGET) --

test_decl:
	corebuild -j $(JOBS) tests/test_decl.$(TARGET) --

test_exp:
	corebuild -j $(JOBS) tests/test_exp.$(TARGET) --

clean:
	corebuild -clean
