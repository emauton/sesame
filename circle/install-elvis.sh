# Install Inaka's Elvis tool. Note its dependency on Erlang/OTP 17.
set -ex

ERLHOME=${HOME}/erlang

if [ ! -e ${ERLHOME}/bin/elvis ]; then
  . ${ERLHOME}/activate
  git clone https://github.com/inaka/elvis.git
  cd elvis
  make escript
  cp elvis ${ERLHOME}/bin
fi
