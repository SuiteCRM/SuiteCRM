<?php

namespace SuiteCRM\ModuleInstall;

use \LoggerManager;

/**
 * Class ExtensionManager
 */
#[\AllowDynamicProperties]
class ExtensionManager
{
    /**
     * @var array
     */
    protected static $moduleList = [];

    /**
     * @var
     */
    protected static $logger;

    /**
     * Checks if authenticated and dies if not.
     *
     * @return void
     */
    protected static function handleAuth()
    {
        if (!defined('sugarEntry') || !sugarEntry) {
            die('Not A Valid Entry Point');
        }
    }

    /**
     * Sets the static module and extension lists.
     *
     * @return void
     */
    protected static function initialise()
    {
        static::$moduleList = get_module_dir_list();
        static::$logger = LoggerManager::getLogger();
    }

    /**
     * Compiles source files for given extension to targeted file applying any given filters.
     *
     * @param string $extension Name of Extension i.e. 'Language'
     * @param string $targetFileName Name of target file
     * @param string $filter To filter file names such as language prefixes
     * @param bool $applicationOnly Whether or not to only compile application extensions
     * @return void
     */
    public static function compileExtensionFiles(
        $extension,
        $targetFileName,
        $filter = '',
        $applicationOnly = false
    ) {
        static::handleAuth();
        static::initialise();

        if ($extension === 'Language' && strpos($targetFileName, $filter) !== 0) {
            $targetFileName = $filter . $targetFileName;
        }

        if (!$applicationOnly) {
            static::compileModuleExtensions($extension, $targetFileName, $filter);
        }

        static::compileApplicationExtensions($extension, $targetFileName, $filter);
    }

    /**
     * @param $extension
     * @param $targetFileName
     * @param string $filter
     * @return void
     */
    protected static function compileApplicationExtensions(
        $extension,
        $targetFileName,
        $filter = ''
    ) {
        static::$logger->{'debug'}("Merging application files for $targetFileName in $extension");

        $extensionContents = '<?php' . PHP_EOL . '// WARNING: The contents of this file are auto-generated' . PHP_EOL;
        $extPath = "application/Ext/$extension";
        $moduleInstall  = "custom/Extension/$extPath";
        $shouldSave = false;

        if (is_dir($moduleInstall)) {
            $dir = dir($moduleInstall);

            while ($entry = $dir->read()) {
                if (static::shouldSkipFile($entry, $moduleInstall, $filter)) {
                    continue;
                }
                $shouldSave = true;
                $file = file_get_contents("$moduleInstall/$entry");
                $extensionContents .= PHP_EOL . static::removePhpTagsFromString($file);
            }
        }

        if ($shouldSave) {
            static::saveFile($extPath, $targetFileName, $extensionContents);
            return;
        }

        static::unlinkFile($extPath, $targetFileName);
    }

    /**
     * @param $extension
     * @param $targetFileName
     * @param string $filter
     * @return void
     */
    protected static function compileModuleExtensions(
        $extension,
        $targetFileName,
        $filter = ''
    ) {
        static::$logger->{'debug'}(
            self::class . "::compileExtensionFiles() : Merging module files in " .
            "custom/Extension/modules/<module>/$extension to custom/modules/<module>/$extension/$targetFileName"
        );

        foreach (static::$moduleList as $module) {
            $extensionContents = '<?php'
                . PHP_EOL
                . '// WARNING: The contents of this file are auto-generated'
                . PHP_EOL;

            $extPath = "modules/$module/Ext/$extension";
            $moduleInstall  = "custom/Extension/$extPath";
            $shouldSave = false;

            if (is_dir($moduleInstall)) {
                $dir = dir($moduleInstall);
                $shouldSave = true;
                $override = [];

                while ($entry = $dir->read()) {
                    if (static::shouldSkipFile($entry, $moduleInstall, $filter)) {
                        continue;
                    }

                    if (strpos($entry, '_override') === 0) {
                        $override[] = $entry;
                        continue;
                    }

                    $file = file_get_contents("$moduleInstall/$entry");
                    static::$logger->{'debug'}(
                        self::class . "->compileExtensionFiles(): found {$moduleInstall}{$entry}"
                    );

                    $extensionContents .= PHP_EOL . static::removePhpTagsFromString($file);
                }

                foreach ($override as $entry) {
                    $file = file_get_contents("$moduleInstall/$entry");
                    $extensionContents .= PHP_EOL . static::removePhpTagsFromString($file);
                }
            }

            if ($shouldSave) {
                static::saveFile($extPath, $targetFileName, $extensionContents);
                continue;
            }

            static::unlinkFile($extPath, $targetFileName);
        }
    }

    /**
     * @param string $string
     * @return string
     */
    protected static function removePhpTagsFromString($string)
    {
        return str_replace(
            ['<?php', '?>', '<?PHP', '<?'],
            '',
            $string
        );
    }

    /**
     * @param $entry
     * @param $moduleInstall
     * @param $filter
     * @return bool
     */
    protected static function shouldSkipFile(
        $entry,
        $moduleInstall,
        $filter
    ) {
        if ($entry === '.' || $entry === '..' || strtolower(substr((string) $entry, -4)) !== '.php') {
            return true;
        }

        if (!is_file("$moduleInstall/$entry")) {
            return true;
        }

        if (!empty($filter) && substr_count((string) $entry, (string) $filter) <= 0) {
            return true;
        }

        return false;
    }

    /**
     * @param string $extPath
     * @param string $targetFileName
     * @param string $extensionContents
     * @return void
     */
    protected static function saveFile(
        $extPath,
        $targetFileName,
        $extensionContents
    ) {
        if (!file_exists("custom/$extPath")) {
            mkdir_recursive("custom/$extPath", true);
        }

        $extensionContents .= PHP_EOL;

        $out = sugar_fopen("custom/$extPath/$targetFileName", 'wb');
        fwrite($out, $extensionContents);
        fclose($out);
    }

    /**
     * @param $extPath
     * @param $targetFileName
     * @return void
     */
    protected static function unlinkFile($extPath, $targetFileName)
    {
        if (file_exists("custom/$extPath/$targetFileName")) {
            unlink("custom/$extPath/$targetFileName");
        }
    }
}
