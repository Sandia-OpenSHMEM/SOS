# Sandia OpenSHMEM Docker Image
Sandia OpenSHMEM (SOS) utlizes Docker images to provide users a means to create
containers that provide a configurable Ubuntu sandbox for running SOS
applications and benchmarks.

## Creating SOS Containers from the Prebuilt Docker Images
Coming Soon: Information regarding the SOS public Docker Hub repos and prebuilt
images.

## Creating SOS Containers from the Dockerfile 
This Dockerfile provides a configurable Ubuntu sandbox for running SOS
applications and benchmarks.

### Building the Image from the Dockerfile
To build the image from the Dockerfile, run the following command in the
directory that contains the Dockerfile:

```
$ docker build -t <name-of-image> .
```  

Where `<name-of-image>` represents the name being used to label the image.
The "docker build" command will use the Dockerfile to build the docker image
from which containers can be created.
To create an interactive container from the image run the following command:

```
$ docker run -it <name-of-image>
```

Where the `-it` option makes the container interactive and allows the user to
actually step into the container and execute commands in the new environment.

### Dockerfile Options
Throughout the Dockerfile there are lines that can be commented/uncommented to
configure a particular testing environment and SOS build.

#### Ubuntu Image
The first option this file provides is the type of Ubuntu image docker
should pull from.

By default the Dockerfile pulls a lightweight Ubuntu 20.04 image.
The other option (which is commnented out) is for OneAPI image which uses
Ubunutu 18.04. The idea for this image was to allow users to test/run SOS
applications with non-gcc compilers.

#### Building Libafabic
The next set of options are presented when it comes time to build Libfabric.

By default, a basic Libfabric configuration is built and installed to an
"install" directory (This option is currently uncommented).
The other option is to enable the GNI provider while building Libfabric. (For
this option uncomment the line of code "To build Libfabric to use GNI provider")

#### Building SOS
Another set of options are presented when Building SOS.

By default, SOS is built with a basic Libfabric provider.
The next few options are:
* Build SOS with the GNI provider
* Build SOS with the Libfabric provider and PMIx
* Build SOS with Portals 4
* Build SOS with UCX

As a reminder, the chosen SOS configuration option must be uncommented, while
the other options must be commented out.

#### Enabling the PRRTE Server
The last portion of code that is optional in this Dockerfile is that which
enables the PRRTE Server.
This is mostly used to run SOS tests when the GNI provider has been enabled in
the Libfabric build.
When testing on the GNI provider uncomment out the lines under "Start PRRTE
Server and use prun to run tests" to build a container which runs "make check"
on the PRRTE Server.
