cd ..\build
ca65 -I ..\src -t apple2 ..\src\hello.asm -l hello.txt
cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\hello.asm apple2.lib  -o hello.apple2 -C apple2-asm.cfg
copy ..\disk\template.dsk hello.dsk
java -jar C:\jar\AppleCommander.jar -p  hello.dsk hello.system sys < C:\cc65\target\apple2\util\loader.system
java -jar C:\jar\AppleCommander.jar -as hello.dsk hello bin < hello.apple2 
C:\AppleWin\Applewin.exe -no-printscreen-dlg -d1 hello.dsk 

