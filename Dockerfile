FROM mattocci/cmdstan-verse-zsh:4.3.2

ENV DEBIAN_FRONTEND noninteractive

ENV QUARTO_VERSION=1.5.56

USER root

RUN  wget -N --no-check-certificate -q -O install_quarto.sh \
  "https://raw.githubusercontent.com/rocker-org/rocker-versioned2/master/scripts/install_quarto.sh" \
  && chmod +x install_quarto.sh \
  && bash install_quarto.sh \
  && rm install_quarto.sh

USER rstudio
