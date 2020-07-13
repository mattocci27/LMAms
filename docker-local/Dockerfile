FROM mattocci/rstan:3.6.3

RUN  apt update --fix-missing && apt install -y \
  python-pip \
  # forwarding \
  openssh-client-ssh1 \
  x11-apps \
  libxt-dev \
  libopenmpi-dev \
  libmagick++-dev \
  curl \
  npm \
  software-properties-common \
  && curl -sL https://deb.nodesource.com/setup_12.x | sudo bash - \
  && apt install -y nodejs \
  && npm install request got nodemailer \
  && pip install request
