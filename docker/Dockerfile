# Docker file for Why3find CI

FROM --platform=linux/amd64 ocaml/opam:alpine-ocaml-4.14
RUN sudo apk add typos
RUN sudo ln -s /usr/bin/opam-2.5 /usr/local/bin/opam
RUN opam init --reinit -ni
WORKDIR /home/why3find
COPY --from=why3find why3find.opam .
RUN opam update
RUN opam install ./why3find.opam --deps-only --with-test --with-doc
