from curses import wrapper
from ui import ChatUI
import socket
import threading

HOST = 'ryukinix.tk'
PORT = 5558

s = socket.socket()
s.connect((HOST, PORT))
read_stream = s.makefile()
write_stream = s.makefile(mode='w')


def writer():
    global chat
    while not read_stream.closed:
        chat.chatbuffer_add(read_stream.readline())


def main(stdscr):
    global chat, running
    stdscr.clear()
    chat = ChatUI(stdscr)
    try:
        username = chat.wait_input(str(read_stream.readline().strip('\n')))
        write_stream.write(username + '\n')
        write_stream.flush()
        inp = ""
        t = threading.Thread(target=writer)
        t.start()
        while inp != "/quit":
            inp = chat.wait_input()
            write_stream.write(inp + '\n')
            write_stream.flush()
    except KeyboardInterrupt:
        pass
    finally:
        read_stream.close()
        write_stream.close()
        s.close()
        t.join()

wrapper(main)
