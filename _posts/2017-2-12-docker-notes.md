---
layout: notes
title: Docker notes
---

The following are notes on how Docker works and is administrated.

Docker provides the following improvements to applications and their
life-cycles:

- Forcing applications to be more ephemeral as containers are quickly destroyed
	and recreated.
- Improving the run-time footprint of applications by making them more lean.
- Providing abstractions to developers so as to control application resources
	and dependencies without affecting the host system (and bothering the people
	that maintain them).
- The production specific instances themselves can be tested before being
	transferred to their production environments.

Common candidates for Docker-isation are services that are stateless, or store
all their state in some external data storage. Docker is built in Go and is an
application-level virtualization platform. 

Architecture
------------

Docker employs the following:

- Linux control groups, a.k.a. cgroups (process isolation).
- Union file systems (copy-on-write).
- Namespaces (resources jailing) that control process, network, IPC, mount, UTS
	usage, and etc.

Docker also uses a container format backend. The default is libcontainer, but
it can also use LXC.

Docker is made up of the following components:

- **Docker Registry** - storage for Docker Images, Docker Hub being the public
	one.
- **Docker Engine** (or Docker Daemon) - something that runs containers.
- **Docker Server** - runs on Docker daemons and receives requests from clients
	and interacts with the registry.
- **Docker Client** (or Docker Binary) - accepts user commands and interacts
	with servers.
- **Docker Container** - a running container instance, referenced with a
	container id or a container name.
- **Docker Image** - base from which a container is launched, consists of
	filesystem layers and metadata.
- **Dockerfile** - a set of instructions in a file that builds an image.

Dockerfiles create a Docker image which create a Docker Container which runs on
a Docker server.

Docker images are created into containers (docker create) and then started
(docker start). These operations are combined with docker run.

## Images

Images are read-only templates that serves as a base for new containers.
Creating images can be done in the following ways:

- Updating an existing base image and committing the results.
- Building from a Dockerfile.

Committing changes to an image involves running a container from the image, and
executing a `docker commit`.

Dockerfiles are configuration files that create images with `docker build`
commands.

- Build context refers to the current directory the build occurs in, the
  contents of which are uploaded to the image.
- Each instruction creates a new layer and commits the results.
- Containers may only contain a maximum of 127 layers.

Images can hold a tag for easy reference. Registries will hold images and can
be pulled and pushed to. The latest version value will always pull the latest
available image being referenced. Specific references can also be pulled.


	docker pull fedora:latest
	docker pull fedora@sha256:abcdefgh...

## Containers

Containers can use inheritable key/value metadata which are useful for
container filtering.

	docker run --detach --name test_labels --label="foo=bar" --label="fizz=buzz" centos:latest sleep 1000
	docker ps --filter="label=foo=bar"

Stopped containers will remain registered unless they are deleted or given the
`--rm` flag at runtime.

Containers can be given at runtime:

- Environment variables, `--env`.
- DNS, `--dns` and `--dns-search`.
- Host mounts, `--volume`.
- MAC address information, `--mac-address`.

Shared mounts can give an appended `:z` for multiple container shared space, or
`:Z` to guarantee single container access. Container local storage can be made
read-only. Combined with shared mounts, this can give fine-grain control over
where data is written out from a container. Local storage can be selectively
writeable using `--tmpfs` options.

Restart behavior can be 'disabled', 'on-failure', and 'always':

	> docker run --restart=on-failure:3 ... # will attempt to restart a badly exiting instance up to three times.

When a container is stopped, its inner processes exit. Docker containers
respond to signals like `SIGTERM` and `SIGKILL`.

	> docker stop --timeout 3. ...  # will attempt to SIGTERM until 30 seconds have passed, and then SIGKILL
	> docker kill --signal USR1 ... # will send a USR1 signal

Containers can also be paused. This keeps their runtime in memory, preserves
open handles, but stops the container from being CPU scheduled.

Docker containers can be deeply inspected for creation and runtime information
using docker inspect.

	> docker inspect abcdefg123...

Attaching to running containers can be done using an `exec` subcommand, or
tools that interactive with Linux namespaces, assuming that the container can
host the command you are attempting..

	> docker exec --tty --interactive acdefg123... /bin/bash

Note that, most of the time, Docker containers must run a process that stays in
the foreground.

Tooling
-------

The Docker binary is the CLI tool for interacting with containers. Docker
daemons also have remote APIs, which the Docker tool uses. Docker clients can
be made backwards compatible with older server versions by setting the
`DOCKER_API_VERSION` to the appropriate value.

Runtime configuration is often best passed in as environment which is managed
externally, rather than trying to manage it as persistent state.

Versioning
----------

Docker containers are version controlled through filesystem layers and image
tagging. Each addition to a container provisioned by a Dockerfile or performed
directly on a container creates a new filesystem later. Image tagging applies
at deployment time and is useful for macro-level versioning.

Docker registries act as a centralized HTTP transport for Docker images. There
are both public and private (for source or caching) registries available for
use.

Networking
----------

Docker network communication can use Unix or TCP sockets. Ports are typically
2375 or 2376 (encrypted).

Docker containers running on an engine can be considered as peers on a private
network maintained by the supporting server.

Internal requests to containers are, by default, brokered by the Docker proxy.
External requests meant from Docker containers must travel through the Docker
server's networking bridging, usually as an exposed interface. Either of these
can potentially become a network bottleneck.

Performance
-----------

By default, Docker containers will not jail their CPU, memory, or swap usage,
but will compete for resources like normal processes. These capabilities must
be compiled into the kernel before it's possible to use them.

- CPU shares are logical divisions of CPU time. A container can be assigned any
	part of a max of 1024 shares (default). This defines how time is sliced
	between containers. A container with twice the CPU shares as another will be
	scheduled to work twice as much. Can also employ CPU CFS.
- Containers can be pinned to specific CPU cores (using `--cpuset`).
- CPU resources are controlled through soft limits. The limit is actually only
	enforced if there is resource contention.
- Memory resources use hard limits. Specifying more memory than is available on
	the systems will allow swapping.
- Swap can be set separately, or disabled with `--memory-swap=-1`.
- Linux OOM will prevent containers from allocating more memory than they are
	allowed. This can be disabled.
- Block IO can explicitly be limited.

`docker update` can be used to update container resource settings in real-time.

Shared host filesystem storage is non-performing and should be avoided, where
possible. Ulimit settings can be applied to containers. There are defaults
which can be set on daemon start, and the `--ulimit` argument can apply
settings specific to containers.

`docker stats` and querying the Docker API are good ways to provide streaming
information about Docker container health. `docker events` will stream things
happening with the Docker server. `docker top` will show active processes
contained within a container (note: from the parent system, information showed
on top will be contextual to the container, like paths and UID/GIDs).

Examples
--------

- Run a new Centos 6 based container and have it execute a command.

      > docker run centos:6 /bin/echo "Oh hai"

- Launch with a interactive shell.

      > docker run --interactive --tty centos:6 /bin/bash

- View running Docker containers and all containers (created, running, stopped,
	etc.).

      > docker ps
      > docker ps --all

- View the standard output of a Docker container.

      > docker logs <container-id>

- Attach to a running Docker container.

      > docker attach <container-id>

- Delete all containers.

      > docker rm $(docker ps --all --quiet)

- Delete all images.

      > docker rmi $(docker images --quiet --filter "dangling=true")

- Show all files that have changed within the containers since build.

      > docker diff <container-id>

- Follow the logs of an actively running container.

			> docker logs --follow <container-id>
