#!/bin/bash

PS3='Please select blender version, where the script should be installed to: '
paths=(~/.config/blender/*/)

select dir in "${paths[@]}"; do
  echo "installing to ${dir}"'!';
  mkdir -p "${dir}scripts/addons/";
  ln -sf "$(pwd)/blender3d_exporter/io_export_pascal3d/" "${dir}scripts/addons/";
  if [ -e "${dir}scripts/addons/io_export_pascal3d/" ]; then
    echo "Success!";
  else
    echo "Failed!";
  fi;
  break; 
done

