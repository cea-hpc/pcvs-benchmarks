JCHRONOSS external dependencies
===============================

In order to ensure a high level of functionalities, JCHRONOSS requires some
dependencies to produce the best qualities and take advantage of the best from
plugins. JCHRONOSS embeds the following projects:

LibWebSockets
-------------

JCHRONOSS is based in part on the [LibWebSockets](https://libwebsockets.org/)
project. It is an open-source project distributed under LGPL license as a C
library providing lighweight Web Sockets interface to state C/C++ applications
as client/server (works in both directions). The version used here is v2.1
(2016-10-06). In order to ensure minimal space for JCHRONOSS archive, we strips
this dependency off two directories: doc/ and test-server/ (around 5Mio). Please
contact  
