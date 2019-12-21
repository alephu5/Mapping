FROM alpine@sha256:d371657a4f661a854ff050898003f4cb6c7f36d968a943c1d5cde0952bd93c80 as build
RUN export PATH=$PATH:$HOME/.local/bin
RUN apk add ghc musl-dev zlib-dev curl
RUN curl -sSL https://get.haskellstack.org/ | sh
RUN stack --system-ghc install Cabal alex happy hscolour
RUN mkdir /opt/build
COPY . /opt/build
WORKDIR /opt/build
RUN stack --system-ghc install .
RUN cp -r $(stack --system-ghc path --local-install-root) /opt/deploy
RUN mv /opt/deploy/bin/* /opt/deploy/bin/app
FROM alpine@sha256:d371657a4f661a854ff050898003f4cb6c7f36d968a943c1d5cde0952bd93c80 as deploy
COPY --from=build /opt/deploy /opt/deploy
CMD ["/opt/deploy/app"]
