DOCKER_IMG = lisp-chat
VERSION := latest
PUBLIC_IMG = ryukinix/$(DOCKER_IMG):$(VERSION)

client:
	./roswell/lisp-chat.ros

server:
	./roswell/lisp-chat-server.ros

docker-build:
	docker build -t $(DOCKER_IMG) .

docker-shell: docker-build
	docker run --rm -it --entrypoint=/bin/bash $(DOCKER_IMG)

docker-run: docker-build
	docker run --rm -it -p 5558:5558 -p 5559:5559 $(DOCKER_IMG)

docker-publish: docker-build
	docker tag $(DOCKER_IMG) $(PUBLIC_IMG)
	docker push $(PUBLIC_IMG)

deploy: docker-publish
	ssh starfox bash /home/lerax/Deploy/lisp-chat.sh


.PHONY: check docker-build docs appimage
