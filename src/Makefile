CC65_DIR = /usr/share/cc65
LOADER = $(CC65_DIR)/target/apple2/util/loader.system
DISK_DIR = ../disk
BUILD_DIR = ../build
TEMPLATE_DISK = $(DISK_DIR)/template.dsk
APPLE_COMMANDER =  java -jar ~/jar/AppleCommander.jar
CFG = start4000.cfg

DEP = defines.asm font7x8.asm inline_print.asm medit.asm tedit.asm

run: $(DISK_DIR)/gedit.dsk
	linapple

$(DISK_DIR)/%.dsk: $(BUILD_DIR)/%.apple2
	cp -f $(TEMPLATE_DISK) $@
	$(APPLE_COMMANDER) -p  $@ $(notdir $(basename $@)).system sys < $(LOADER)
	$(APPLE_COMMANDER) -as $@ $(notdir $(basename $@))        bin < $<


$(BUILD_DIR)/%.apple2: %.asm $(DEP) $(CFG)
	ca65 -t apple2               $<             -l $(BUILD_DIR)/$(basename $<).dis
	cl65 -t apple2 -u __EXEHDR__ $< apple2.lib  -o $(BUILD_DIR)/$(basename $<).apple2 -C $(CFG)

