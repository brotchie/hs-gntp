#!/usr/bin/env python

from gntp.notifier import mini

def main():
    print mini('Hello world', port=9090)

if __name__ == '__main__':
    main()
