#!/usr/bin/env bash

docker run \
  --rm -ti \
  -p 8080:8080 \
  -v $PWD/contrib/google-access-token.php:/router.php \
  --env-file .env \
  php -S 0.0.0.0:8080 /router.php
