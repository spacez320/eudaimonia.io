eudaimonia.io
=============

I built this with:

  - [Jekyll](https://jekyllrb.com)
  - [Skeleton](http://getskeleton.com/)
  - ~~[Vagrant](https://vagrantup.com)~~ [Docker](https://www.docker.com/)
  - [AWS S3](https://aws.amazon.com/s3)

To develop:

    docker-compose up dev

To build:

    docker-compose up build

To deploy:

    aws s3 sync _site/ s3://eudaimonia.io/
