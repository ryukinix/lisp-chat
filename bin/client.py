#!/usr/bin/env python
# Author: Manoel Vilela

import socket
import threading
import readline

HOST = 'chat.lerax.me'
PORT = 5558

# set editing-mode on input
readline.parse_and_bind('set editing-mode vi')

s = socket.socket()
s.connect((HOST, PORT))
read_stream = s.makefile()
write_stream = s.makefile(mode='w')


def get_user_input():
    inp = input()
    print("\033[1A\033[2K", end='')
    return inp


def server_reader():
    """Fetch lines from server and print"""
    try:
        while not read_stream.closed:
            print(read_stream.readline(), end='')
    except ValueError:
        pass
        # after the read_stream is closed, in the case
        # the thread is running read_stream.readline()
        # is running this will throw a exception of
        # IO operation on closed file.


def send_message(message):
    write_stream.write(message + '\n')
    write_stream.flush()


def main():
    try:
        username = input(str(read_stream.readline().strip('\n')))
        print("Connected as {}@{}:{}".format(username, HOST, PORT))
        send_message(username)
        inp = ""
        t = threading.Thread(target=server_reader)
        t.start()
        while inp != "/quit":
            inp = get_user_input()
            send_message(inp)
    except KeyboardInterrupt:
        send_message("/quit")
    finally:
        read_stream.close()
        write_stream.close()
        s.close()
        t.join()


if __name__ == '__main__':
    main()
