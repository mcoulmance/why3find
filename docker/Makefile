# --------------------------------------------------------------------------
# --- GitLab Continuous Integration
# --------------------------------------------------------------------------

.PHONY: build

IMAGE=registry.frama-c.com/pub/why3find:v2

run:
	docker run --rm -it $(IMAGE)

build:
	dune build ../why3find.opam
	docker build -t $(IMAGE) -f Dockerfile --build-context=why3find=.. .

push:
	docker push $(IMAGE)
