#!/bin/sh

# Runs composer update, echoes php version and runs PHPUnit
# Usage examples:
# - ./run-tests.sh --group 20221124
# - ./run-tests.sh --exclude-group slow

composer update --quiet
#php -r 'echo "\nPHP version " . phpversion() . ". ";'
php ./vendor/phpunit/phpunit/phpunit $@
