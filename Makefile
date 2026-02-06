DOCKER_IMG = lisp-chat
APP_VERSION = $(shell git describe --tags --abbrev=0 2> /dev/null || printf 0.0.0)-$(shell date +%Y%m%d)
VERSION := latest
PUBLIC_IMG = ryukinix/$(DOCKER_IMG):$(VERSION)

client:
	./roswell/lisp-chat.ros

client-websockets:
	./roswell/lisp-chat.ros ws://localhost:5559/ws

client-online:
	APP_ENV=PROD ./roswell/lisp-chat.ros

server:
	./roswell/lisp-chat-server.ros

compile:
	APP_VERSION=$(APP_VERSION) APP_ENV=PROD ros build ./roswell/lisp-chat.ros

appimage: compile
	bash scripts/appimage.sh

docker-appimage: docker-build
	docker run --rm \
		--volume $(PWD):/lisp-chat \
		--env APPIMAGE_EXTRACT_AND_RUN=1 \
		--entrypoint=/bin/bash \
		$(DOCKER_IMG) \
		-c "make appimage APP_VERSION=$(APP_VERSION) && chown -R $(shell id -u):$(shell id -g) .appimage *.AppImage"

docker-build:
	docker build --build-arg APP_VERSION=$(APP_VERSION) -t $(DOCKER_IMG) .

docker-shell: docker-build
	docker run --rm -it --entrypoint=/bin/bash $(DOCKER_IMG)

docker-run: docker-build
	docker run --rm -it -p 5558:5558 -p 5559:5559 $(DOCKER_IMG)

docker-publish: docker-build
	docker tag $(DOCKER_IMG) $(PUBLIC_IMG)
	docker push $(PUBLIC_IMG)

deploy: docker-publish
	ssh starfox bash /home/lerax/Deploy/lisp-chat.sh

dep-tree:
	ros -s asdf-dependency-graph -e '(asdf-dependency-graph:generate "tree.png" "lisp-chat/client")'

.PHONY: check docker-build docs appimage docker-check docker-appimage

check:
	ros -s lisp-chat/tests -e '(asdf:test-system :lisp-chat)'

docker-check: docker-build
	docker run --rm --entrypoint=ros $(DOCKER_IMG) -s lisp-chat/tests -e '(asdf:test-system :lisp-chat)'
