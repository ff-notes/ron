# RON demo chat

## PoC. One god room

# Room

There must be a god object with id `chatroom` of type ORSet (`:set`).

If it does not exist, it must be created on first run.

Room references a message with op with payload starting with `message` keyword
followed by the message object id:

    message {object_id : UUID}

# Messages

Each message is an object of type ORSet (`:set`).

## Running

    # read me
    stack run -- chat --help

# Server

    # listening localhost:29737
    stack run -- chat node --listen=29737

# Client

    # default database, connect to localhost
    stack run -- chat ui Ronald --peer='ws://:29737' 2>log

    # view log in another terminal
    tail -f log

or, to test multiple instances

    # custom database, connect to custom server
    stack run --                        \
        chat ui Jeremiah                \
        --data=Jeremiah.sqlite          \
        --peer='ws://example.com:29737' \
    2>Jeremiah.log
