FROM commonlispbr/roswell:latest
RUN apt update && apt install libreadline8 libev4 -y
WORKDIR /lisp-chat
COPY ./lisp-chat.asd lisp-chat.asd
COPY ./src src
COPY ./tests tests
COPY ./roswell roswell
RUN ros install ./
RUN ros run -s lisp-chat/server -q
EXPOSE 5558
EXPOSE 5559
ENTRYPOINT ["/root/.roswell/bin/lisp-chat-server"]
