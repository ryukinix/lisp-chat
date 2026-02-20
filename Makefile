DOCKER_IMG = lisp-chat
APP_VERSION = $(shell git describe --tags 2> /dev/null || printf 0.0.0)+$(shell date +%Y%m%d)
VERSION := latest
PUBLIC_IMG = ryukinix/$(DOCKER_IMG):$(VERSION)
ROS_TEST_FLAGS = -e "(sb-ext:disable-debugger)" -s lisp-chat/tests

lint:
	mallet --format line src

docker-lint:
	docker run --rm -t -v $(PWD):/src ryukinix/mallet:latest --format line src

version:
	@echo $(APP_VERSION)

install-deps:
	ros install qlot
	qlot install

client:
	qlot exec ./roswell/lisp-chat.ros

client-websockets:
	qlot exec ./roswell/lisp-chat.ros ws://localhost:5559/ws

client-online:
	APP_ENV=PROD qlot exec ./roswell/lisp-chat.ros

server:
	qlot exec ./roswell/lisp-chat-server.ros

compile:
	APP_VERSION=$(APP_VERSION) APP_ENV=PROD qlot exec ros build ./roswell/lisp-chat.ros

appimage: compile
	bash scripts/appimage.sh

docker-appimage: docker-build-tui
	docker run --rm \
		--volume $(PWD):/lisp-chat \
		--volume /lisp-chat/.qlot \
		--env APPIMAGE_EXTRACT_AND_RUN=1 \
		--entrypoint=/bin/bash \
		$(DOCKER_IMG)-tui \
		-c "make appimage APP_VERSION=$(APP_VERSION) && chown -R $(shell id -u):$(shell id -g) .appimage *.AppImage"

docker-build:
	docker build --build-arg APP_VERSION=$(APP_VERSION) -t $(DOCKER_IMG) .

docker-build-tui:
	docker build -f Dockerfile.tui --build-arg APP_VERSION=$(APP_VERSION) -t $(DOCKER_IMG)-tui .

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
	qlot exec ros -s asdf-dependency-graph -e '(asdf-dependency-graph:generate "tree.png" "lisp-chat")'

.PHONY: check docker-build docs appimage docker-check docker-appimage

check-integration:
	qlot exec ros -s lisp-chat/tests -e '(parachute:test (quote lisp-chat/tests::integration-tests))'

check-unit:
	qlot exec ros -s lisp-chat/tests -e '(parachute:test (quote lisp-chat/tests::unit-tests))'

check:
	qlot exec ros $(ROS_TEST_FLAGS) -e '(asdf:test-system :lisp-chat/tests)'

docker-check: docker-build
	docker run --rm --entrypoint=ros $(DOCKER_IMG) $(ROS_TEST_FLAGS) -e '(asdf:test-system :lisp-chat/tests)'
