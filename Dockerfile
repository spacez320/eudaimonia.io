#
# Builds a container that can run Jekyll from local source.
#
# To build:
#
#     $ docker build --tag 'jekyll:latest' .
#
# To use:
#
#     $ docker run --tty --interactive --publish-all --rm \
#         --volume `pwd`:/src/eudaimonia jekyll:latest /bin/bash
#     $ jekyll serve --livereload
#

FROM fedora
VOLUME /src/eudaimonia
WORKDIR /src/eudaimonia
COPY Gemfile /tmp/
EXPOSE 8000
RUN dnf --assumeyes install gcc gcc-c++ libffi-devel redhat-rpm-config && \
dnf --assumeyes install ruby ruby-devel && \
gem install bundler && \
bundle install --gemfile=/tmp/Gemfile
