# Common functions used by the different shell programs. Also contains the
# CSV header definitions for the downloaded Ping v2 CSV files.
#
# Daniel Weibel <daniel.weibel@unifr.ch> Mar. 2015
#------------------------------------------------------------------------------#

# File header definitions
#------------------------------------------------------------------------------#
# CellIdsService
H1="ts,cid,lac,mnc,mcc,rssi,nwCode,operator,connected,cdma"
# PingService
H2="ts,exec,sent,rec,imei,server,protocol,rttArr,rtt,wifiMob,nwType,ssidOp,macCid,handovers,rssiArr,rssi,cid,lac,mnc,mcc"

# Functions
#------------------------------------------------------------------------------#
# Print error message to stderr and exit
Abort() {
  echo -e "${0##*/}: $1" >&2
  exit 1
}

# Run R script and handle R errors
MyRscript() {
  local w=$(which ${0##*/})
  Prepend "source(\"$(dirname "${w:-$0}")/../lib/toolbox.r\")" "$1"
  local e=$(TmpFile)
  Rscript "$@" 2>"$e" 1>/dev/null || Abort "$(echo "R error:"; cat "$e")"
}

# Prepend string (arg 1) as a line to the beginning of a file (arg 2). 
Prepend() {
  local tmp=$(TmpFile)
  echo -e "$1"  >"$tmp"
  cat     "$2" >>"$tmp"
  mv      "$tmp" "$2"
}

# Convert date string of format YYYY-MM-DD to UNIX timestamp in seconds
Date2Ts() {
  if IsMac; then date -j -f "%Y-%m-%d %H%M%S" "$1 000000" "+%s" 2>/dev/null
  else           date -d                      "$1"        "+%s" 2>/dev/null; fi
}

# Create temporary file or directory
TmpFile() { mktemp    -t "${0##*/}.XXXX"; }
TmpDir()  { mktemp -d -t "${0##*/}.XXXX"; }

# Find out on which operating system the script is running
IsMac()   { [[ $(GetOS) = mac   ]]; }
IsLinux() { [[ $(GetOS) = linux ]]; }
GetOS()   {
  case "$OSTYPE" in
    *darwin*) echo mac   ;;
    *)        echo linux ;;
  esac
}

# Test if variable is unset/empty or set/non-empty
IsSet()      { [[ -n "$1" ]]; }
IsUnset()    { [[ -z "$1" ]]; }
IsEmpty()    { IsUnset  "$1"; }
IsNonEmpty() { IsSet    "$1"; }

# Check if file or directory exists
CheckFile() { [[ -f "$1" ]] || Abort "file does not exist: $1"; }
CheckDir()  { [[ -d "$1" ]] || Abort "directory does not exist: $1"; }

# Check if input file for a script is valid
CheckInput() {
  CheckFile "$1"
  [[ -s "$1" ]] || Abort "empty input"
}
