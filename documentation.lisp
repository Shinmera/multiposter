(in-package #:org.shirakumo.multiposter)

;; protocol.lisp
(docs:define-docs
  (variable *multiposter*
    "The default multiposter instance to use.

See MULTIPOSTER (type)
See LOAD-CONFIG
See SAVE-CONFIG")
  
  (function add-tag
    "Add a tag to the post.

The tag is not added if there's an existing tag that matches the new
tag regardless of whitespace and case.

See POST (type)")

  (function post
    "Post a post to one or more services.

If VERBOSE is non-NIL, extra status information may be printed for the
user.

See POST (type)
See CLIENT (type)
See MULTIPOSTER (type)
See PROFILE (type)
See VERBOSE")

  (function ready-p
    "Returns true if the CLIENT is ready for posting.

See CLIENT (type)")

  (function setup
    "Performs the necessary setup for the CLIENT to become ready.

If no ARGS are passed, and the client is not properly configured, an
interactive setup should be started to configure the client.
If ARGS are passed, the client should be reinitialised with those new
arguments.

If the configured client is not READY even after the setup, or the
configuration is somehow invalid, an error should be signalled.

See CLIENT (type)")

  (function undo
    "Undoes the post and deletes it if possible.

If this is not possible to do, an error is signalled.

See RESULT (type)")

  (function failed-p
    "Returns true if the result represents a failed post.

A failed result cannot be UNDOne.

See RESULT (type)")

  (function add-client
    "Add a new client to the multiposter.

A continuable error is signalled if a client with the same NAME exists
already.
A continuable error is signalled if the client cannot be readied.

See CLIENT (type)
See MULTIPOSTER (type)")

  (function add-profile
    "Add a new profile to the multiposter.

A continuable error is signalled if a client with the same NAME exists
already.

See PROFILE (type)
See MULTIPOSTER (type)")

  (function find-profile
    "Returns the profile of the given name, if any.

See PROFILE (type)
See MULTIPOSTER (type)")

  (function find-client
    "Returns the client of the given name, if any.

See PROFILE (type)
See MULTIPOSTER (type)")

  (type post
    "Superclass for all post types.

A post encapsulates all necessary information to push to a service in
a more structured way. Each client should piece together the
information according to the service's own distinct fields using
COMPOSE-POST.

See TITLE
See HEADER
See FOOTER
See DESCRIPTION
See CONTENT-WARNING
See TAGS
See COMPOSE-POST")

  (function title
    "Accesses the title of a post.

If the client does not support an explicit title field, the title
should be treated as a paragraph before the header.

See POST (type)")

  (function header
    "Accesses the header of a post.

The header should be a paragraph to be prepended before the
description.

See PROFILE (type)
See POST (type)")

  (function footer
    "Accesses the footer of a post.

The footer should be a paragraph to be appended after the
description.

See PROFILE (type)
See POST (type)")

  (function description
    "Accesses the main description content of a post.

See POST (type)")

  (function content-warning
    "Accesses the content-warning description of a post.

If set, this will be used as the explanation of why the content in the
post should include a warning disclaimer.

See POST (type)")

  (function tags
    "Accesses the tags list of a post.

The list of tags should be a set of strings of tags, each of which are
distinct from each other. You should add tags via ADD-TAG rather than
pushing onto this directly.

See ADD-TAG
See PROFILE (type)
See POST (type)")

  (function compose-post
    "Compose the post content text.

This should be used to compose the main post text, especially if the
client does not support specific fields for the title or tags.
You may pass all arguments permitted by COMPOSE-POST-TEXT. If
EXCLUDE-TITLE is true, the title of the post is not included in the
text. If EXCLUDE-TAGS is true, the tags list of the post is not
included in the text.

See COMPOSE-POST-TEXT
See POST (type)")

  (type image-post
    "Post representing a set of images.

The number of images allowed is client-specific. The client should
automatically trim down the set of files as needed, preferring earlier
files.

If a client does not support an image type, the client may signal an
error or drop the file from the set. If the set becomes empty, an
error should be signalled.

Each file may also have a description text. If the client does not
support file descriptions, the descriptions should be discarded.

See FILES
See FILE-DESCRIPTIONS
See POST (type)")

  (function files
    "Accesses the files of a post.

See FILE-DESCRIPTIONS
See IMAGE-POST (type)")

  (function file-descriptions
    "Accesses the file descriptions of a post.

This must be a list of the same length of FILES.

See FILES
See IMAGE-POST (type)")

  (type video-post
    "Post representing a video file.

If a client does not support a video type, the client should signal an
error.

See FILE
See POST (type)")

  (function file
    "Accesses the file of the post.

See VIDEO-POST (type)")

  (type link-post
    "Post representing a link to another web thing.

See URL
See POST (type)")

  (function url
    "Accesses the URL of the post.

See LINK-POST (type)")

  (type text-post
    "Post representing a text post.

The markup language supported is client dependent. The main post text
content is the DESCRIPTION field.

See DESCRIPTION
See POST (type)")

  (type client
    "Superclass for all clients.

See NAME
See POST-TAGS
See DEFINE-CLIENT
See POST
See READY-P
See SETUP")

  (function name
    "Returns the name of the client or profile.

See CLIENT (type)")

  (function post-tags
    "Accessor to the post tags of the client.

This should be a list where each element is a list composed of
the following elements:

  POST-TYPE TAG*

Where POST-TYPE corresponds to the type of post to which all of the
following tags should be added when posting to this client.

See CLIENT (type)")

  (function define-client
    "Define a new client type.

The syntax is identical to DEFCLASS. It will define the class and also
register it in the internal system so that it can be easily accessed
by the user.

See CLIENT (type)")

  (type result
    "Representation of a POST request result.

See CLIENT
See POST-OBJECT
See URL
See UNDO
See FAILED-P")

  (function client
    "Accesses the client object to which the POST was made.

See RESULT (type)
See CLIENT (type)")

  (function post-object
    "Accesses the post object that was posted.

Note that due to modifications made to fit the post to the client's
requirements, this instance may not be EQ to the POST instance that
was passed to the POST function initially.

See RESULT (type)
See POST (type)")

  (function url
    "Accesses the remote URL of the resulting post, if any.

This may be NIL if the result is a failure.
This may be an empty string if there is no canonical URL to access.

See RESULT (type)")

  (type profile
    "Representation of a posting profile.

A profile encapsulates certain standard tags to add to each post, as
well as an optional header and footer to append to each post.
You may also set a specific subset of clients to post to when using
the profile.

When POSTing to a profile, it creates a new post that is augmented
with the specified properties of the profile, and then posted to the
set of clients the profile is configured for.

See NAME
See CLIENTS
See TAGS
See HEADER
See FOOTER
See POST")

  (function clients
    "Accesses the clients associated with the object.

For a PROFILE this is a list of CLIENT instances.
For a MULTIPOSTER this is a hash table of names to cLIENT instances.

See CLIENT (type)
See PROFILE (type)
See MULTIPOSTER (type)")

  (type multiposter
    "Representation of a multiposter configuration.

See DEFAULT-PROFILE
See ADD-CLIENT
See ADD-PROFILE
See FIND-CLIENT
See FIND-PROFILE
See POST")

  (function default-profile
    "Accesses the default profile to use.

If this is NIL, posting to the multiposter will post to all
clients. Otherwise it will post to the default profile.

See POST
See PROFILE (type)
See MULTIPOSTER (type)"))

;; config.lisp
(docs:define-docs
  (function config-file
    "Returns the path to the configuration file.

If MULTIPOSTER_CONFIG is set, that path is used unquestioningly.
Otherwise, if APPDATA or XDG_CONFIG_HOME are set they are used as base
directory, and if not, the USER-HOMEDIR-PATHNAME is used as the base
directory, within which a directory named \"multiposter\" is created,
and within which a file called \"multiposter.lisp\" is created.")

  (function load-config
    "Loads the configuration file and updates the multiposter with it.

If MULTIPOSTER is NIL, a new multiposter instance is created and
returned. Note that this will not update the *MULTIPOSTER* variable.

See CONFIG-FILE
See MULTIPOSTER (type)")

  (function save-config
    "Saves the configuration file.

Returns the MULTIPOSTER instance.

See CONFIG-FILE
See MULTIPOSTER (type)"))

;; toolkit.lisp
(docs:define-docs
  (variable *image-types*
    "Variable used to identify pathname-types that correspond to images.

See FILE-TYPE-P")

  (variable *video-types*
    "Variable used to identify pathname-types that correspond to videos.

See FILE-TYPE-P")

  (function or*
    "Like OR except that empty strings are treated as NIL.")

  (function file-type-p
    "Checks THING against TYPEs and returns T if any match.

See *IMAGE-TYPES*
See *VIDEO-TYPES*")

  (function envvar
    "Returns the value of the given environment variable if it is not empty.")

  (function parse-tags
    "Parses the given tag or list of tags by splitting each by commas.")

  (function merge-paragraphs
    "Merge multiple paragraphs by inserting two newlines between each.

Paragraphs that are NIL or empty strings are ignored. If no paragraphs
are passed that aren't ignored, NIL is returned.")

  (function initargs
    "Return a list of initargs for the instance.

Users should add methods to this to append initargs that they need to
reconstruct the instance.")

  (function make-like
    "Create a copy of the object but with the given initargs set.")

  (function alphanumeric-p
    "Returns T if the character is from the ASCII alphanumeric set.")

  (function non-space-p
    "Returns T if the character isn't a space.")

  (function non-comma-p
    "Returns T if the character isn't a comma.")

  (function filter-tags
    "Ensures that all tags only contain allowed characters.")

  (function trim-text
    "Trims the text smartly to fit within the char limit.

If the limit is too low for the text to reasonably fit at all, NIL is
returned instead.")

  (function compose-post-text
    "Compose a single string from the given post parts.

Precedence is given to the post parts in the following order:
  1. Tags (only whole tags are dropped)
  2. Header (trimmed)
  3. Footer (trimmed)
  4. Body (trimmed)

Meaning if the limit is small enough, the post text may be reduced to
just tags, or just header and tags, etc.")

  (function query
    "Prompt the user for some information.

Returns the entered value.

If NULLABLE is T, NIL may be returned.
If DEFAULT is given, the DEFAULT is returned if the user does not
enter a value.
If COERCE is set, the user's value is first run through the COERCE
function to convert/parse its type.
If CHECK is set, the value is only considered valid if the CHECK
function returns true.")

  (function verbose
    "Prints a log message for informational purposes.")

  (function timestamp
    "Returns a \"YYYY.MM.DD hh-mm-ss\" formatted timestamp string.")

  (function path-url
    "Returns a file URL for the given pathname."))

;; clients
(docs:define-docs
  (type cohost
    "Client that posts to cohost.org

Tags are filtered to not contain any commas.

See CLIENT (type)")
  
  (type dummy
    "Dummy client that doesn't do anything.

See CLIENT (type)")

  (type file
    "File client that saves posts to a directory.

Each post gets its own txt file. Additional files like images and
videos are copied alongside and named similarly to the primary
file. The file name is chosen via timestamp and post title.

See CLIENT (type)")

  (type git
    "File client that commits to a git repository.

This acts like the FILE client, but also makes sure to pull the
repository before writing files, and instead of generating a txt file
puts the post metadata into the resulting commit instead. After
committing it will also push the repository.

See FILE (type)")

  (type lichat
    "Client that connects to a Lichat server and posts to a channel.

This will split the post content and optional payloads across multiple
updates. Tags are omitted from the messages.

See CLIENT (type)")

  (type mastodon
    "Client that posts to a mastodon server of choice.

The post content is truncated automatically to fit within the toot
message limit. Tags are filtered to be alphanumeric only.

See CLIENT (type)
See COMPOSE-POST-TEXT")

  (type reader
    "Client that posts to a Reader blog.

This only supports TEXT-POSTs.

See CLIENT (type)")

  (type studio
    "Client that uploads to a Studio gallery.

This only supports IMAGE-POSTs. If the title is unset, will use the
filename of the first file in the image post.

See CLIENT (type)")

  (type tumblr
    "Client that posts to Tumblr.

Tags are filtered to not contain any commas.

See CLIENT (type)")

  (type webdav
    "Client that posts to a WebDAV server.

This can use either an Authorization header, or HTTP Basic
authentication. It will upload the files using POST requests, and
remove them on failure using DELETE requests.

See CLIENT (type)"))
