Tumble - An Emacs mode for Tumblr
========================

Please visit [the project page](http://febuiles.github.com/tumble/) for in-depth installation and usage instructions.

Overview
--------

Tumble is a mode for interacting with Tumblr inside Emacs. It currently
supports the following types of posts:

* Text
* Quote
* Link
* Chat
* Photo
* Audio
* Video (only through embed)

You can start tumbling by using the following functions:

    tumble-text-from-region
    tumble-text-from-buffer

    tumble-quote-from-region

    tumble-link
    tumble-link-with-description

    tumble-chat-from-region
    tumble-chat-from-buffer

    tumble-photo-from-url
    tumble-photo-from-file

    tumble-audio

    tumble-video-from-url

Read tumble.el for more information about each function.

Installation
------------
Download Tumble to some directory:

    $ git clone git://github.com/febuiles/tumble.git

Add it to your load list and require it:

    (add-to-list 'load-path "~/some_directory/tumble")
    (require 'tumble)

At this point you can, either set your Tubmblr account info on your
.emacs file:

    tumble-email:    your account e-mail
    tumble-password: your account password
    tumble-url:      your account url (the main blog url)

Or just post without setting them, Tumble will prompt you for your
account details when you try to make a post.

If you want to post to a group or secondary blog, you should instead
set the `tumble-group` variable.

You can also customize the format of the post by modifying the
`tumble-format` variable. Tumble uses Markdown as default for posting.

Tumble uses HTTPS when posting. This is certainly more secure than
using HTTP, but also slower. If you want to send your passwords as
plain text over http you can just change the `tumble-api-url`
variable. It's `https://www.tumblr.com/api/write` by default, just
change the `https` part to `http`.


License
-------

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

Check tumble.el for more information.

Fork freely!


Acknowledgements
----------------

* Tumble now prompts you for login credentials thanks to
  Quildreen Motta <quildreen@gmail.com>.
