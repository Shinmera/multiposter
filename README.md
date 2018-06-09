## About Multiposter
This is a protocol and small application to allow posting content to multiple providers at once.

## How To
Load the providers you want:

    (ql:quickload '(multiposter-twitter multiposter-mastodon multiposter-tumblr multiposter-git))

And set them up:

    (multiposter:setup)

This will prompt you for a bunch of information for each client. By default it will create a client for each type that you loaded. 

It's necessary to note that one client (potentially at random) will be picked as the "primary" client. This means that a post on every other client will contain a link to the post of the primary client. If you would like to set another client to be the primary, you can pass a different list:

    (multiposter:setup :clients '(multiposter-tumblr:client multiposter-twitter:client ..))

The first one in the list will be picked as the primary. If you do not want any primary at all, you can set it to NIL explicitly.

    (setf (multiposter:primary multiposter:*client*) NIL)

When you set the client up, it will automatically persist all the necessary login information to a configuration file at `*config-path*`. In future sessions, you can simply call `restore` to get the settings back.

To actually post to the configured services, simply call one of the related functions, `text` `link` `image` `video`:

    (multiposter:image #p"~/meat.jpg" :description "Some meat." :tags '("meat"))

And that's pretty much it. Note that for services that do not support tags as metadata, or enforce limits on the textual content, Multiposter will attempt to be as smart as possible and shorten or convert things in a sensible manner. It is for this reason too that using a primary client that does not restrict things is a good idea.

## Creating New Clients
If there's an additional service you would like to add support for, you can make use of Multiposter's posting protocol. The first step is to create a subclass of `client`.

    (defclass client (multiposter:client) ())

Next you will need a method on `make-load-form`. This is used to persist the client's settings.

    (defmethod make-load-form ((client client) &optional env)
      (declare (ignore env))
      `(make-instance 'client ...))

Another important part of the protocol is the `login` function, which takes care of the authentication during the initial `setup`. The function accepts arbitrary keywords, which you should use to allow the user to potentially provide default values. For anything that is not specified, use `prompt` to ask the user interactively.

    (defmethod multiposter:login ((client client) &key some-prop)
      (let ((some-prop (or some-prop (multiposter:prompt "Please enter some prop"))))
        (setf (some-prop client) some-prop)
        ...))

Finally you'll need methods on `post-text`, `post-link`, `post-image`, and `post-video` that perform the actual posting. You should take care to post as much of the provided information as possible and to only cut content if necessary.
