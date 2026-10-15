<?php
/**
 * Router for PHP's built-in development server.
 * Mimics Apache mod_rewrite behavior: serves static files directly,
 * routes API requests through the Slim API entry point,
 * and everything else through SuiteCRM's front controller.
 *
 * Usage: php -S localhost:80 build/router.php
 */

$uri = urldecode(parse_url($_SERVER['REQUEST_URI'], PHP_URL_PATH));

// Serve existing static files and PHP files directly
if ($uri !== '/' && file_exists(__DIR__ . '/../' . $uri)) {
    return false;
}

// Route API requests through the Slim API entry point
if (str_starts_with($uri, '/api/') || str_starts_with($uri, '/Api/')) {
    $_SERVER['SCRIPT_NAME'] = '/Api/index.php';
    $_SERVER['SCRIPT_FILENAME'] = __DIR__ . '/../Api/index.php';
    chdir(__DIR__ . '/..');
    require __DIR__ . '/../Api/index.php';
    return;
}

// Route everything else through index.php
$_SERVER['SCRIPT_NAME'] = '/index.php';
$_SERVER['SCRIPT_FILENAME'] = __DIR__ . '/../index.php';
chdir(__DIR__ . '/..');
require __DIR__ . '/../index.php';
