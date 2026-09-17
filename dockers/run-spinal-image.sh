#! /bin/bash
set -ex

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

DOCKER_ENGINE=${DOCKER_ENGINE:-podman}
IMAGE_NAME=ghcr.io/jdavidberger/spinalhdlextras/spinal-image:latest

if [[ ! -z $PULL_IMAGE ]]; then
  ${DOCKER_ENGINE} pull $IMAGE_NAME
else
  ${DOCKER_ENGINE} build $SCRIPT_DIR -f Dockerfile.spinalhdl -t $IMAGE_NAME --progress plain
fi

PWD=$(pwd)

mkdir -p "${PWD}/.${DOCKER_ENGINE}/.cache"
mkdir -p "${PWD}/.${DOCKER_ENGINE}/.sbt"
mkdir -p "${PWD}/.${DOCKER_ENGINE}/.ivy2/cache"
mkdir -p "${PWD}/.${DOCKER_ENGINE}/.ivy2/local"
mkdir -p "${PWD}/.${DOCKER_ENGINE}/target"
mkdir -p "${PWD}/.${DOCKER_ENGINE}/simulations"
mkdir -p "${PWD}/.${DOCKER_ENGINE}/simWorkspace"
mkdir -p "${PWD}/.${DOCKER_ENGINE}/project"

# Mount ivy2 under both /home/user and /root: this image's sbt often runs as
# root and publishLocal writes /root/.ivy2/local (not /home/user/...). Without
# the /root mounts, make publish-spinal appears to succeed but gen still sees
# the old SpinalHDL jar (e.g. missing PipelinedMemoryBusRsp.error).
${DOCKER_ENGINE} run --userns=host \
  -it --rm \
  -v "${PWD}/.${DOCKER_ENGINE}/.cache:/home/user/.cache" \
  -v "${PWD}:${PWD}" \
  -v "${PWD}/.${DOCKER_ENGINE}/project:${PWD}/project" \
  -v "${PWD}/.${DOCKER_ENGINE}/target:${PWD}/target" \
  -v "${PWD}/.${DOCKER_ENGINE}/.sbt:/home/user/.sbt" \
  -v "${PWD}/.${DOCKER_ENGINE}/.sbt:/root/.sbt" \
  -v "${PWD}/.${DOCKER_ENGINE}/simulations:${PWD}/simulations" \
  -v "${PWD}/.${DOCKER_ENGINE}/simWorkspace:${PWD}/simWorkspace" \
  -v "${PWD}/.${DOCKER_ENGINE}/.ivy2/cache:/home/user/.ivy2/cache" \
  -v "${PWD}/.${DOCKER_ENGINE}/.ivy2/cache:/root/.ivy2/cache" \
  -v "${PWD}/.${DOCKER_ENGINE}/.ivy2/local:/home/user/.ivy2/local" \
  -v "${PWD}/.${DOCKER_ENGINE}/.ivy2/local:/root/.ivy2/local" \
  --workdir "${PWD}" \
  $IMAGE_NAME "$@"
