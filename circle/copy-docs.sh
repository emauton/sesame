# Copy all generated documentation as artifacts.
set -ex

cd apps
mkdir ${CIRCLE_ARTIFACTS}/docs

for app in * ; do
    cp -R ${app}/edoc ${CIRCLE_ARTIFACTS}/docs/${app}
done
