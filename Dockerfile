FROM fedora
VOLUME /src/eudaimonia
WORKDIR /src/eudaimonia
COPY Gemfile /tmp/
EXPOSE 8000
RUN dnf --assumeyes install \
      gcc \
      gcc-c++ \
      libffi-devel \
      openssl \
      openssl-devel \
      redhat-rpm-config \
      ruby \
      ruby-devel && \
    gem install bundler && \
    bundle install --gemfile=/tmp/Gemfile
