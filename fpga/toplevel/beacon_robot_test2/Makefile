
PORT = /dev/ttyUSB0

toplevel.svf: ise/toplevel.bit
	impact -batch impact.cmd

header: ise/toplevel.bin
	python bin2c.py ise/toplevel.bin bitstream.hpp

program: toplevel.svf
	jtag urjtag.cmd

dataflash:
	java -jar ../../../tools/upload/upload.jar --port $(PORT) ise/toplevel.bin

.phony: program dataflash

