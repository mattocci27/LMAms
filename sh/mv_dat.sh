#!/bin/sh

GCE="${HOME}/Dropbox/gce"
OVERWRITE=true

usage() {
  name=`basename $0`
  cat <<EOF
Usage:
  $name [arguments] [command]
Commands:
  local_to_dropbox
  dropbox_to_local
Arguments:
  -h Print help
EOF
  exit 1
}

while getopts :fh opt; do
  case ${opt} in
    h)
      usage
      ;;
  esac
done
shift $((OPTIND - 1))

# Installation and settings
local_to_dropbox(){
  array=`ls data | grep \.rda`
  for f in $array
  do
    # Force remove a dotfile if it's already there
    if [ -f ${f} ] &&
      [ -n "${OVERWRITE}" -a -e ${GCE}/${f} ]; then
      rm -f ${GCE}/${f}
    fi
    if [ ! -e ${GCE}/${f} ]; then
      cp -rf data/${f} ${GCE}/${f}
    fi
  done

  echo $(tput setaf 2)R dat files moved to dropbox!. ✔︎$(tput sgr0)
}

dropbox_to_local(){
  array=`ls ${GCE} | grep \.rda`
  for f in $array
  do
    # Force remove a dotfile if it's already there
    if [ -f ${f} ] &&
      [ -n "${OVERWRITE}" -a -e data/${f} ]; then
      rm -f data/${f}
    fi
    if [ ! -e data/${f} ]; then
      cp -rf ${GCE}/${f} data/${f} 
    fi
  done

  echo $(tput setaf 2)R dat files moved to gce local!. ✔︎$(tput sgr0)
}

command=$1
[ $# -gt 0 ] && shift

case $command in
  local_to_dropbox*)
    local_to_dropbox
    ;;
  dropbox_to_local*)
    dropbox_to_local
    ;;
  *)
    usage
    ;;
esac
