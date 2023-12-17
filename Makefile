LIBNAME = custom-css-module

.PHONY: clean

$(LIBNAME).so: $(LIBNAME).c Makefile
	$(CC) `pkg-config --cflags gtk+-3.0` -O2 -shared -o $@ -fPIC $<

clean:
	rm -rf $(LIBNAME).so
