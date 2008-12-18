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

Read tumble.el for more information about each function.

Installation
------------
Download Tumble to some directory:

    $ git clone git://github.com/febuiles/tumble.git

Add it to your load list and require it:

    (add-to-list 'load-path "~/some_directory/tumble.el")
    (require 'tumble)

Open tumble.el and modify the following variables:

    (setq tumble-email "your_email@something.com")
    (setq tumble-password "your_password")
    (setq tumble-url "your_tumblelog.tumblr.com")

(optional) Tumble uses no group for posting and Markdown as the default 
format but you can change these:

    (setq tumble-group "your_group.tumblr.com")
    (setq tumble-format "html")


License
-------

Check tumble.el for more information.

Copyright (c) 2008 Federico Builes
Fork freely!
