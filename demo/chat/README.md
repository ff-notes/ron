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
