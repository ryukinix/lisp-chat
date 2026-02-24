FROM commonlispbr/roswell:latest
RUN apt update && apt install libreadline8 libev4 wget file make -y
# HACK: cl-tuition from quicklisp has problems
RUN ros install atgreen/cl-tuition
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
RUN ros install ./lisp-chat.asd
RUN ros run -s lisp-chat/server -q
EXPOSE 5558
EXPOSE 5559
ENTRYPOINT ["/root/.roswell/bin/lisp-chat-server"]
