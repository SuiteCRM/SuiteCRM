<?php
/*
 * This document has been generated with
 * https://mlocati.github.io/php-cs-fixer-configurator/#version:3.0.0-rc.1|configurator
 * you can change this configuration by importing this file.
 *
 */

$config = include 'PhpCsFixer.php';

return $config->setFinder(PhpCsFixer\Finder::create()
    ->exclude('vendor')
    ->in(__DIR__.'\src')
    ->in(__DIR__.'\tests')
    );
