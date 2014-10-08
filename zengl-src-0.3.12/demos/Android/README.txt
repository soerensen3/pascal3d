RU:
ВНИМАНИЕ! Примеры не будут работать в эмуляторе с версией Android ниже 4.0, т.к. он не поддерживает архитектуру ARMv6

Примеры рассчитаны на Android NDK r8e и FreePascal собранный по этой инструкции:
http://zengl.org/wiki/doku.php?id=compilation:android

Для других версий Android NDK понадобится менять путь для файлов проекта в Lazarus(Project->Project Options->Other).

Порядок сборки:
- собрать библиотеку используя файл проекта для Lazarus'а, например "01 - Initialization/jni/demo01_linux.lpi"
- импортировать в Eclipse основной каталог проекта - "01 - Initialization"
- запустить дебаг :)

EN:
WARNING! Demos won't work in emulator with version of Android lower than 4.0 because it doesn't support ARMv6 architecture

Demos are written for Android NDK r8e and FreePascal built using this instruction:
http://zengl.org/wiki/doku.php?id=compilation:android

For other versions of Android NDK new path should be set for Lazarus project files(Project->Project Options->Other).

Build steps:
- compile library using project file for Lazarus, e.g. "01 - Initialization/jni/demo01_linux.lpi"
- import main directory of project into Eclipse
- run debug :)