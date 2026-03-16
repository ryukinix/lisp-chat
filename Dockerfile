FROM commonlispbr/roswell:latest
RUN apt update && apt install libev4 wget file make -y
WORKDIR /lisp-chat
RUN ln -s /lisp-chat /root/.roswell/local-projects/lisp-chat
COPY ./lisp-chat.asd lisp-chat.asd
COPY ./src src
COPY ./tests tests
COPY ./roswell roswell
COPY ./scripts scripts
RUN bash ./scripts/bump_static.sh

ARG APP_VERSION
ENV APP_VERSION=$APP_VERSION
RUN ros build roswell/lisp-chat-server.ros
EXPOSE 5558
EXPOSE 5559
ENTRYPOINT ["/lisp-chat/roswell/lisp-chat-server"]
