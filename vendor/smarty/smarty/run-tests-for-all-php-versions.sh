#!/bin/bash

# Runs tests for all supported PHP versions
# Usage examples:
# - ./run-tests-for-all-php-versions.sh --group 20221124
# - ./run-tests-for-all-php-versions.sh --exclude-group slow

COMPOSE_CMD="mutagen-compose"

$COMPOSE_CMD run --rm php71 ./run-tests.sh $@ && \
$COMPOSE_CMD run --rm php72 ./run-tests.sh $@ && \
$COMPOSE_CMD run --rm php73 ./run-tests.sh $@ && \
$COMPOSE_CMD run --rm php74 ./run-tests.sh $@ && \
$COMPOSE_CMD run --rm php80 ./run-tests.sh $@ && \
$COMPOSE_CMD run --rm php81 ./run-tests.sh $@ && \
$COMPOSE_CMD run --rm php82 ./run-tests.sh $@ && \
$COMPOSE_CMD run --rm php83 ./run-tests.sh $@ && \
$COMPOSE_CMD run --rm php84 ./run-tests.sh $@
