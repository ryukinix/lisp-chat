FROM commonlispbr/roswell:latest
RUN apt update && apt install libreadline7 -y
WORKDIR /lisp-chat
COPY ./lisp-chat.asd lisp-chat.asd
COPY ./src src
COPY ./roswell roswell
RUN ros install ./
EXPOSE 5558
ENTRYPOINT ["/root/.roswell/bin/lisp-chat-server"]
