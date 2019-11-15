#!/bin/bash

set -x
curl http://localhost:8888/resty/hello
curl http://localhost:8888/resty/hello/
curl http://localhost:8888/resty/java
curl http://localhost:8888/resty/java/
curl http://localhost:8888/resty/java/scala
curl http://localhost:8888/resty/scala
curl http://localhost:8888/resty/scala/
curl http://localhost:8888/resty/scala/java
curl http://localhost:8888/resty/scala/user/steve/
set +x
curl http://localhost:8888/resty/scala/user/1
echo
curl http://localhost:8888/resty/scala/user/2
echo
curl -H "Accept: application/xml" http://localhost:8888/resty/scala/user/1
echo
curl -H "Accept: application/xml" http://localhost:8888/resty/scala/user/2
echo
