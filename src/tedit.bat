cd ..\build
ca65 -I ..\src -t apple2 ..\src\tedit.asm -l tedit.dis
cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\tedit.asm apple2.lib  -o tedit.apple2 -C ..\src\start4000.cfg
copy ..\disk\template.dsk tedit.dsk
java -jar C:\jar\AppleCommander.jar -p  tedit.dsk tedit.system sys < C:\cc65\target\apple2\util\loader.system
java -jar C:\jar\AppleCommander.jar -as tedit.dsk tedit bin < tedit.apple2 
copy tedit.dsk ..\disk
C:\AppleWin\Applewin.exe -no-printscreen-dlg -d1 tedit.dsk

