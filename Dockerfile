FROM fedora
MAINTAINER matthew@eudaimonia.io
RUN dnf --assumeyes install ruby ruby-devel
RUN gem install bundler
RUN bundle install
