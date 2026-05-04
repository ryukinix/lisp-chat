FROM commonlispbr/roswell:latest
RUN apt update && apt install libev4 wget file make -y
WORKDIR /lisp-chat
RUN ln -s /lisp-chat /root/.roswell/local-projects/lisp-chat
COPY ./lisp-chat.asd lisp-chat.asd
COPY ./qlfile qlfile
COPY ./qlfile.lock qlfile.lock
RUN ros install qlot
RUN ~/.roswell/bin/qlot install

COPY ./src src
COPY ./tests tests
COPY ./roswell roswell
COPY ./scripts scripts
RUN bash ./scripts/bundle_minify.sh

ARG APP_VERSION
ENV APP_VERSION=$APP_VERSION
RUN ~/.roswell/bin/qlot exec ros build roswell/lisp-chat-server.ros
EXPOSE 5558
EXPOSE 5559
ENTRYPOINT ["/lisp-chat/roswell/lisp-chat-server"]
