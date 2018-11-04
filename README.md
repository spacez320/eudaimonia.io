eudaimonia.io
=============

I built this with:

  - [Jekyll](https://jekyllrb.com)
  - [Skeleton](http://getskeleton.com/)
  - ~~[Vagrant](https://vagrantup.com)~~ [Docker](https://www.docker.com/)
  - [AWS S3](https://aws.amazon.com/s3)

To build:

    docker build --tag 'jekyll:latest' .

To develop:

    docker run --tty --interactive --publish-all --rm \
      --volume `pwd`:/src/eudaimonia jekyll:latest /bin/bash
    jekyll serve --livereload

To deploy:

    aws s3 sync _site/ s3://eudaimonia.io/
