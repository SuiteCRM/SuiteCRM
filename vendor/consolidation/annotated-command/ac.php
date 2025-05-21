<?php

/**
 * A script for ad hoc testing of commands defined in this project.
 */

require 'vendor/autoload.php';

// Only add the Attributes commands since Annotation commands have same name and would conflict.
$myCommandClassInstance = new \Consolidation\TestUtils\ExampleAttributesCommandFile();
$commandFactory = new \Consolidation\AnnotatedCommand\AnnotatedCommandFactory();
$commandFactory->setIncludeAllPublicMethods(true);
$commandFactory->commandProcessor()->setFormatterManager(new \Consolidation\OutputFormatters\FormatterManager());
$commandList = $commandFactory->createCommandsFromClass($myCommandClassInstance);
$application = new \Symfony\Component\Console\Application('ac');
foreach ($commandList as $command) {
    $application->add($command);
}
$application->run();
