#!/bin/bash
set -e

echo "=== SuiteCRM Docker ==="

# Wait for MySQL
echo "Waiting for MySQL..."
until mysqladmin ping -h "${DATABASE_HOST:-mysql}" -u root -p"${DATABASE_ROOT_PASSWORD:-root}" --silent 2>/dev/null; do sleep 2; done

# Wait for Elasticsearch  
echo "Waiting for Elasticsearch..."
until curl -s "http://elasticsearch:9200/_cluster/health" >/dev/null 2>&1; do sleep 2; done

# Check if already installed
if [ -f config.php ] && grep -q "db_host_name" config.php 2>/dev/null; then
    echo "Already installed. Starting Apache..."
    exec apache2-foreground
fi

# Run silent installer
echo "Running silent installer..."

# Create config_si.php with env vars substituted (handled by the PHP file using getenv)
cp /var/www/html/docker/config_si.php /var/www/html/config_si.php

php -r '
define("sugarEntry", true);
$_REQUEST = ["goto" => "SilentInstall", "cli" => true];
$_SERVER["SERVER_SOFTWARE"] = "Apache";
$_SERVER["SERVER_NAME"] = "localhost";
$_SERVER["REQUEST_URI"] = "/install.php";
$_SERVER["HTTP_HOST"] = "localhost";
$_SERVER["REQUEST_METHOD"] = "GET";
chdir("/var/www/html");
include "install.php";
'

# Verify
if [ -f config.php ] && grep -q "db_host_name" config.php 2>/dev/null; then
    echo "Installation successful!"
    rm -f config_si.php
else
    echo "WARNING: Installation may have failed. Check logs."
fi

# Fix permissions
chown -R www-data:www-data /var/www/html 2>/dev/null || true
chmod -R 775 cache custom modules upload 2>/dev/null || true

echo ""
echo "====================================================="
echo " SuiteCRM ready: ${SITE_URL:-http://localhost:8080}"
echo " Login: ${ADMIN_USER:-admin} / ${ADMIN_PASSWORD:-admin1}"
echo "====================================================="

exec apache2-foreground
