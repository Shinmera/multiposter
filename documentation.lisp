#|
 This file is a part of Multiposter
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.multiposter)

;; interface.lisp
(docs:define-docs
  (variable *client*
    "This variable keeps the current client instance for convenient posting.

See CLIENT
See RESTORE
See OFFLOAD
See SETUP
See TEXT
See LINK
See IMAGE
See VIDEO")
  
  (variable *config-path*
    "This variable holds the default path for the configuration file.

If XDG_CONFIG_HOME is set, it is:
  XDG_CONFIG_HOME/multiposter/client.lisp
If the system is Windows, it is:
  ~/AppData/Local/multiposter/client.lisp
Otherwise, it is:
  ~/.config/multiposter/client.lisp")
  
  (function restore
    "Load the client from the configuration file.

Simply LOADs the given path if it exists and returns *client*.

See *CLIENT*
See *CONFIG-PATH*")
  
  (function offload
    "Save the client to the configuration file.

Simply writes the client's make-load-form to the file.
Returns the client.

See *CLIENT*
See *CONFIG-PATH*")
  
  (function setup
    "Constructs a new multiposter client and performs a login.

On successful login, the client is OFFLOADed, the *CONFIG-PATH*
and *CLIENT* variables are set, and the client is returned.
The clients list can be a list of class names, class instances,
or CLIENT instances. The first client in the list is picked as
the multiposter client's primary client.

See CLIENT-TYPES
See LOGIN
See OFFLOAD
See *CLIENT*
See *CONFIG-PATH*")
  
  (function text
    "Shorthand to post a new text update.

See POST-TEXT
See *CLIENT*")
  
  (function link
    "Shorthand to post a new link.

See POST-LINK
See *CLIENT*")
  
  (function image
    "Shorthand to post a new image.

See POST-IMAGE
See *CLIENT*")
  
  (function video
    "Shorthand to post a new video.

See POST-VIDEO
See *CLIENT*"))

;; protocol.lisp
(docs:define-docs
  (variable *image-types*
    "Holds a list of known pathname types that designate image files.

By default:
  png jpg jpeg gif bmp svg")
  
  (variable *video-types*
    "Holds a list of known pathname types that designate video files:

By default:
  gifv apng mp4 webm mov mkv")
  
  (type client
    "Superclass for all clients that can post using this protocol.

Clients must implement methods for the following functions:
  LOGIN POST-TEXT POST-LINK POST-IMAGE POST-VIDEO

See CLIENT-TYPES
See LOGIN
See POST
See POST-TEXT
See POST-LINK
See POST-IMAGE
See POST-VIDEO")
  
  (function client-types
    "Returns a list of known subclasses of CLIENT.

See CLIENT
See SETUP")
  
  (function login
    "Performs the login and setup sequence for the given client.

The client may specify custom keyword arguments that can be used
to provide default settings. Any missing settings will be
interactively queries from the user.

Returns the client instance on success.
Must signal an error on failure.

See CLIENT")
  
  (function post
    "Post a thing using the given client.

By default this delegates to the specific functions based on a
heuristic analysis of the thing.

See POST-TEXT
See POST-LINK
See POST-IMAGE
See POST-VIDEO
See *IMAGE-TYPES*
See *VIDEO-TYPES*
See EXTRACT-LINKS")
  
  (function post-text
    "Posts a new textual update to the client.

Depending on the client the text may contain markup and links.
Depending on the client the text or tags might be cut short.
If LINK is provided, the update must include the full link.

See CLIENT")
  
  (function post-link
    "Posts a new link update to the client.

Depending on the client the text may contain markup and links.
Depending on the client the description or tags might be cut short.

See CLIENT")
  
  (function post-image
    "Posts a new image update to the client.

Depending on the client the text may contain markup and links.
Depending on the client the description or tags might be cut short.
If LINK is provided, the update must include the full link.

See CLIENT")
  
  (function post-video
    "Posts a new image update to the client.

Depending on the client the text may contain markup and links.
Depending on the client the description or tags might be cut short.
If LINK is provided, the update must include the full link.

See CLIENT")
  
  (type multiposter
    "Base class for the aggregation of multiple clients into one.

Simply delegates the posting functions to the respective clients and
makes sure that each client refers to the post of the primary client
if a primary client has been set.

See CLIENT
See PRIMARY
See CLIENTS")
  
  (function clients
    "Accessor to the list of client instances in the multiposter.

See MULTIPOSTER")
  
  (function primary
    "Accessor to the primary client of the multiposter.

See MULTIPOSTER"))

;; toolkit.lisp
(docs:define-docs
  (function prompt
    "Prompt the user for some information.

Writes a prompt for PROMPT to the given stream, defaulting to
*QUERY-IO*. It then reads a line from this same stream. If a
DEFAULT is given, and the line is empty, then the default is
returned. Otherwise, the CONVERTER is called with the line. If
the converter signals an error, the error is written to the stream
and the user is asked again. Otherwise, the value returned by the
converter is returned.

By default a converter is used that simply checks that the string
is not empty.")

  (function extract-links
    "Extracts all the links from the text.

This returns a list of the following structure:

  PARTS ::= PART*
  PART  ::= string | LINK
  LINK  ::= (:link string)

This is useful for systems where links are transformed independent
of the rest of the text. Only HTTP/S links are recognised, but the
protocol can be absent from a link.")

  (function limit-text-with-links
    "Truncate the length of a text intelligently.

This is useful for services where links are shortened and thus do
not constitute the full length to the text's length. The returned
string should not contain any truncated links. If a truncation
must happen, it will cut before the last character or newline if
this character is no farther than CUTOFF many characters from the
necessary end of the string.

This heuristic is not perfect, especially in a multilingual
context, but it should provide a good effort.

See EXTRACT-LINKS"))
