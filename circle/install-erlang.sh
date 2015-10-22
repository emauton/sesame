# Install Erlang/OTP 17.
set -ex

get_dependencies() {
  sudo apt-get install build-essential libncurses5-dev openssl libssl-dev \
               fop xsltproc unixodbc-dev
}

VERSION=17.5
ERLROOT=${HOME}/erlang
if [ ! -e ${ERLROOT}/bin/erl ]; then
  get_dependencies
  curl -O https://raw.githubusercontent.com/spawngrid/kerl/master/kerl
  chmod a+x kerl

  # kerl is quiet, and CircleCI times quiet processes out after 3m.
  # To remedy that, we background kerl and watch; set -x creates output.
  ./kerl build ${VERSION} ${VERSION} &
  KERL_PID=$!
  while kill -0 ${KERL_PID} ; do
     sleep 5
  done

  ./kerl install ${VERSION} ${ERLROOT}
fi
