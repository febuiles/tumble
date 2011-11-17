Tumble - An Emacs mode for Tumblr
========================

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

Posts can either be published or saved as drafts, you'll be prompted
when you upload them. If you leave the 'State' field empty the post
will be published.

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

Tumble uses HTTP when posting. This is certainly less secure than
using HTTPS, but it's faster and the Tumblr API behaves in a weird way
with HTTPS. If you want to use HTTPS you can change the `tumble-api-url`
variable. It's `http://www.tumblr.com/api/write` by default, just
change the `http` part to `https`.


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
* Johan Persson <johan.z.persson@gmail.com> added support
  for saving posts as drafts.
