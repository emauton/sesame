# Copy coverage as artifacts.
set -ex

cd apps
mkdir ${CIRCLE_ARTIFACTS}/coverage

for app in * ; do
    cp -R ${app}/.eunit ${CIRCLE_ARTIFACTS}/coverage/${app}
done
