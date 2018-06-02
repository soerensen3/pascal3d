#!/usr/bin/sh

lazbuild --add-package-link src/pascal3d.lpk
lazbuild --add-package-link src/pascal3d_ideintf.lpk
lazbuild --add-package-link src/pascal3d_ide.lpk
lazbuild --add-package-link src/pascal3d_ui.lpk
lazbuild --add-package-link external/libsdl2/libsdl2.lpk
