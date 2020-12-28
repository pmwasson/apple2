cd ..\build
ca65 -I ..\src -t apple2 ..\src\gedit.asm -l gedit.dis
cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\gedit.asm apple2.lib  -o gedit.apple2 -C ..\src\start4000.cfg
copy ..\disk\template.dsk gedit.dsk
java -jar C:\jar\AppleCommander.jar -p  gedit.dsk gedit.system sys < C:\cc65\target\apple2\util\loader.system
java -jar C:\jar\AppleCommander.jar -as gedit.dsk gedit bin < gedit.apple2 
copy gedit.dsk ..\disk
C:\AppleWin\Applewin.exe -no-printscreen-dlg -d1 gedit.dsk

