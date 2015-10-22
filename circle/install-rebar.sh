# Install Rebar.
set -ex

ERLHOME=${HOME}/erlang

if [ ! -e ${ERLHOME}/bin/rebar ]; then
  git clone --branch 2.5.1 --depth 1 https://github.com/rebar/rebar.git
  cd rebar
  ./bootstrap
  cp rebar ${ERLHOME}/bin
fi
