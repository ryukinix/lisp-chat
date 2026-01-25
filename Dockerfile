FROM commonlispbr/roswell:latest
RUN apt update && apt install libreadline8 -y
WORKDIR /lisp-chat
COPY ./lisp-chat.asd lisp-chat.asd
COPY ./src src
COPY ./roswell roswell
RUN ros install ./
RUN ros run -s lisp-chat/server -q
EXPOSE 5558
EXPOSE 5559
ENTRYPOINT ["/root/.roswell/bin/lisp-chat-server"]
