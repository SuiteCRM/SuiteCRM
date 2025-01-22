<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUGARCRM, SUGARCRM DISCLAIMS THE WARRANTY
 * OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License along with
 * this program; if not, see http://www.gnu.org/licenses or write to the Free
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 *
 * You can contact SugarCRM, Inc. headquarters at 10050 North Wolfe Road,
 * SW2-130, Cupertino, CA 95014, USA. or at email address contact@sugarcrm.com.
 *
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo and "Supercharged by SuiteCRM" logo. If the display of the logos is not
 * reasonably feasible for technical reasons, the Appropriate Legal Notices must
 * display the words "Powered by SugarCRM" and "Supercharged by SuiteCRM".
 */
namespace SuiteCRM\Robo\Plugin\Commands;

use SuiteCRM\Utility\OperatingSystem;
use SuiteCRM\Utility\Paths;

#[\AllowDynamicProperties]
class TestEnvironmentCommands extends \Robo\Tasks
{
    use \SuiteCRM\Robo\Traits\RoboTrait;

    /**
     * Configure environment for testing
     * @see https://docs.suitecrm.com/developer/appendix-c---automated-testing/#_environment_variables
     * @param array $opts optional command line arguments
     */
    public function configureTests(
        array $opts = [
            'database_driver' => '',
            'database_name' => '',
            'database_host' => '',
            'database_user' => '',
            'database_password' => '',
            'instance_url' => '',
            'instance_admin_user' => '',
            'instance_admin_password' => '',
            'instance_client_id' => '',
            'instance_client_secret' => '',
        ]
    ) {
        $this->say('Configure Test Environment');

        // Database
        $default_db_driver = strtoupper($this->chooseConfigOrDefault('dbconfig.db_type', 'MYSQL'));
        $this->askDefaultOptionWhenEmpty('Database Driver:', $default_db_driver, $opts['database_driver']);

        $default_db_host = $this->chooseConfigOrDefault('dbconfig.db_host_name', 'localhost');
        $this->askDefaultOptionWhenEmpty('Database Host:', $default_db_host, $opts['database_host']);

        $default_db_user = $this->chooseConfigOrDefault('dbconfig.db_user_name', 'suitecrm_tests');
        $this->askDefaultOptionWhenEmpty('Database Username:', $default_db_user, $opts['database_user']);

        $default_db_password = $this->chooseConfigOrDefault('dbconfig.db_password', 'suitecrm_tests');
        $this->askDefaultOptionWhenEmpty('Database User password:', $default_db_password, $opts['database_password']);

        $default_db_name = $this->chooseConfigOrDefault('dbconfig.db_name', 'suitecrm_tests');
        $this->askDefaultOptionWhenEmpty('Database Name:', $default_db_name, $opts['database_name']);

        // SuiteCRM Instance
        $default_instance_url = $this->chooseConfigOrDefault('site_url', 'http://localhost');
        $this->askDefaultOptionWhenEmpty('Instance URL:', $default_instance_url, $opts['instance_url']);
        $this->askDefaultOptionWhenEmpty('Instance Admin Username:', 'admin', $opts['instance_admin_user']);
        $this->askDefaultOptionWhenEmpty('Instance Admin Password:', 'admin1', $opts['instance_admin_password']);
        $this->askDefaultOptionWhenEmpty('Instance OAuth2 Client ID:', 'suitecrm_client', $opts['instance_client_id']);
        $this->askDefaultOptionWhenEmpty('Instance OAuth2 Client Secret:', 'secret', $opts['instance_client_secret']);

        $os = new OperatingSystem();
        if ($os->isOsWindows()) {
            $this->say('Windows detected');
            $this->installWindowsEnvironmentVariables($opts);
        } elseif ($os->isOsLinux()) {
            $this->say('Linux detected');
            $this->installUnixEnvironmentVariables($opts);
        } elseif ($os->isOsMacOSX()) {
            $this->say('macOS detected');
            $this->installUnixEnvironmentVariables($opts);
        } elseif ($os->isOsBSD()) {
            $this->say('BSD detected');
            $this->installUnixEnvironmentVariables($opts);
        } elseif ($os->isOsSolaris()) {
            $this->say('Solaris detected');
            $this->installUnixEnvironmentVariables($opts);
        } elseif ($os->isOsUnknown()) {
            throw new \DomainException('Unknown Operating system');
        } else {
            throw new \DomainException('Unable to detect Operating system');
        }

        $this->say('Configure Test Environment Complete');
    }

    /**
     * Download and install ChromeDriver.
     * @command chromedriver:install
     * @param array $opts
     * @option bool $reinstall Forces the Chrome WebDriver executable to be reinstalled, can be used to get a newer version.
     * @usage chromedriver:install --reinstall
     */
    public function chromeDriverInstall($opts = ['reinstall' => false])
    {
        $this->say('Installing ChromeDriver...');
        $os = new OperatingSystem();
        $paths = new Paths();
        $url = $this->getChromeWebDriverUrl();
        $basePath = $os->toOsPath($paths->getProjectPath() . '/build/tmp');

        if (!file_exists($basePath)) {
            if (!mkdir($basePath, 0777, true) && !is_dir($basePath)) {
                throw new \RuntimeException('Unable to create file structure ' . $basePath);
            }
        } elseif ($opts['reinstall']) {
            $this->_deleteDir($basePath);
            if (!mkdir($basePath, 0777, true) && !is_dir($basePath)) {
                throw new \RuntimeException('Unable to create file structure ' . $basePath);
            }
        }

        $zipPath = $basePath . DIRECTORY_SEPARATOR . 'webdriver.zip';
        $unzippedPath = $basePath . DIRECTORY_SEPARATOR . 'webdriver';

        if (!file_exists($unzippedPath)) {
            $this->say('Downloading ChromeDriver.');
            $this->download($url, $zipPath);
            $this->unzip($zipPath, $unzippedPath);
            $this->say('ChromeDriver install completed.');
        } else {
            $this->say('ChromeDriver has already been downloaded.');
        }
    }

    /**
     * Run ChromeDriver.
     * @command chromedriver:run
     * @param array $opts
     * @option string $url_base The base URL from which the WebDriver will be run.
     */
    public function chromeDriverRun($opts = ['url_base' => '/wd/hub'])
    {
        $this->say('Running ChromeDriver...');
        $os = new OperatingSystem();
        $paths = new Paths();
        $basePath = $os->toOsPath($paths->getProjectPath() . '/build/tmp/');

        $unzippedPath = $basePath . DIRECTORY_SEPARATOR . 'webdriver';

        if (!file_exists($unzippedPath)) {
            throw new \RuntimeException('ChromeDriver is not installed in ' . $unzippedPath);
        }

        $this->runChromeWebDriver($unzippedPath, $opts['url_base']);
    }

    /**
     * Configures local environment to look like travis
     * @param array $opts
     */
    public function fakeTravis(
        array $opts = [
            'travis' => true,
            'travis_commit_range' => '',
            'travis_pull_request' => true,
        ]
    ) {
        $this->say('Fake Travis Environment');

        $this->askDefaultOptionWhenEmpty('Is Travis Environment:', true, $opts['travis']);
        $this->askDefaultOptionWhenEmpty('Travis commit range:', 'master..develop', $opts['travis_commit_range']);
        $opts['travis_commit_range'] = '\''. $opts['travis_commit_range'] .'\'';
        $this->askDefaultOptionWhenEmpty('Is Pull request:', true, $opts['travis_pull_request']);

        $os = new OperatingSystem();
        if ($os->isOsWindows()) {
            $this->say('Windows detected');
            $this->installWindowsEnvironmentVariables($opts);
        } elseif ($os->isOsLinux()) {
            $this->say('Linux detected');
            $this->installUnixEnvironmentVariables($opts);
        } elseif ($os->isOsMacOSX()) {
            $this->say('macOS detected');
            $this->installUnixEnvironmentVariables($opts);
        } elseif ($os->isOsBSD()) {
            $this->say('BSD detected');
            $this->installUnixEnvironmentVariables($opts);
        } elseif ($os->isOsSolaris()) {
            $this->say('Solaris detected');
            $this->installUnixEnvironmentVariables($opts);
        } elseif ($os->isOsUnknown()) {
            throw new \DomainException('Unknown operating system');
        } else {
            throw new \DomainException('Unable to detect operating system');
        }

        $this->say('Fake Travis Environment Complete');
    }
    /**
     * Install unix environment variables for the testing framework
     * @param array $opts optional command line arguments
     */
    private function installUnixEnvironmentVariables(array $opts)
    {
        $environment_string_unix = $this->toUnixEnvironmentVariables($opts);

        $homePath = getenv("HOME");
        $bashAliasesPath = $homePath
            . DIRECTORY_SEPARATOR
            . '.bash_aliases';

        // create .bash_aliases file?
        if (!file_exists($bashAliasesPath)) {
            $this->say('Creating ' . $bashAliasesPath);
            file_put_contents($bashAliasesPath, '');
        }

        $this->say('Get File Contents ' . $bashAliasesPath);
        $bashAliasesFile = file_get_contents($bashAliasesPath);
        $bashAliasesLines = explode(PHP_EOL, $bashAliasesFile);


        // Delete existing variables
        $self = $this;
        foreach ($opts as $optionKey => $optionValue) {
            // find option key
            $optionKeyReplaced = str_ireplace('-', '_', $optionKey);

            $bashAliasesLines = array_map(function ($line) use ($self, $optionKeyReplaced) {
                // clear line
                if (stristr($line, $optionKeyReplaced) !== false) {
                    $self->say('Removed: ' . $optionKeyReplaced);
                    return '';
                }
                return $line;
            }, $bashAliasesLines);
        }

        $this->writeln('Generate a new .bash_aliases file');
        $newBashAliasesFile = '';

        // Only add lines which are not empty to the new file
        foreach ($bashAliasesLines as $line) {
            if (!empty($line)) {
                $newBashAliasesFile .= $line;
            }
        }

        $newBashAliasesFile .= PHP_EOL . $environment_string_unix;
        $this->writeln($newBashAliasesFile);

        if ($this->confirm('May I overwrite ' . $bashAliasesPath . '?')) {

            // write current file to backup file
            $this->say('Saving existing copy of .bash_aliases to ' . $bashAliasesPath . '~');
            file_put_contents($bashAliasesPath . '~', $bashAliasesFile);

            // write new file to .bash_aliases
            $this->say('Exporting variables to ' . $bashAliasesPath);
            file_put_contents($bashAliasesPath, $newBashAliasesFile);
            $this->writeln('Please restart your terminal or run `bash`');
        } else {
            $this->say('Skipping overwrite' . $bashAliasesPath);
        }
    }

    /**
     * Install windows environment variables for the testing framework
     * @param array $opts optional command line arguments
     */
    private function installWindowsEnvironmentVariables(array $opts)
    {
        $windows_environment_variables = $this->toWindowsEnvironmentVariables($opts);

        $this->writeln("Generate Script");
        $this->writeln($windows_environment_variables);
        if ($this->confirm('May I overwrite the environment variables?')) {
            $this->say('Overwriting environment variables');
            $environment_variables = explode(PHP_EOL, $windows_environment_variables);

            foreach ($environment_variables as $command) {
                $this->_exec($command);
            }

            $this->writeln('Please restart your command prompt or powershell');
        } else {
            $this->say('Skipping overwrite');
        }
    }

    /**
     * @param array $opts <key,value
     * @param string $format sprintf format
     * @return string environment variables script
     */
    private function toEnvironmentVariables(array $opts, $format)
    {
        $script = '';
        foreach ($opts as $optionKey => $optionValue) {
            $optionKeyReplaced = str_ireplace('-', '_', $optionKey);
            if (!empty($optionValue)) {
                $script .= sprintf($format, strtoupper($optionKeyReplaced), $optionValue);
            }
        }
        return $script;
    }

    /**
     * @param array $opts optional command line arguments
     * @return string environment variables script
     */
    private function toWindowsEnvironmentVariables(array $opts)
    {
        return $this->toEnvironmentVariables($opts, 'setx %s %s' . PHP_EOL);
    }

    /**
     * @param array $opts optional command line arguments
     * @return string environment variables script
     */
    private function toUnixEnvironmentVariables(array $opts)
    {
        return $this->toEnvironmentVariables($opts, 'export %s=%s;' . PHP_EOL);
    }


    /**
     * Gets the URL for installing the latest version of ChromeDriver.
     * @return string url
     */
    private function getChromeWebDriverUrl()
    {
        $os = new OperatingSystem();
        $latestRelease = file_get_contents('https://chromedriver.storage.googleapis.com/LATEST_RELEASE', false);

        if ($os->isOsWindows()) {
            $this->say('Windows detected');
            return 'https://chromedriver.storage.googleapis.com/' . $latestRelease . '/chromedriver_win32.zip';
        } elseif ($os->isOsLinux()) {
            $this->say('Linux detected');
            return 'https://chromedriver.storage.googleapis.com/' . $latestRelease . '/chromedriver_linux64.zip';
        } elseif ($os->isOsMacOSX()) {
            $this->say('macOS detected');
            return 'https://chromedriver.storage.googleapis.com/' . $latestRelease . '/chromedriver_mac64.zip';
        } elseif ($os->isOsBSD()) {
            $this->say('BSD detected');
            throw new \DomainException('Unsupported operating system');
        } elseif ($os->isOsSolaris()) {
            $this->say('Solaris detected');
            throw new \DomainException('Unsupported operating system');
        } elseif ($os->isOsUnknown()) {
            throw new \DomainException('Unknown operating system');
        } else {
            throw new \DomainException('Unable to detect operating system');
        }
    }

    /**
     * @param $url to download
     * @param $toPath path to download file to (save as)
     */
    private function download($url, $toPath)
    {
        $contents = file_get_contents($url, false);
        if ($contents === false) {
            throw new \RuntimeException('Unable to download ' . $url);
        }
        if (file_put_contents($toPath, $contents) === false) {
            throw new \RuntimeException('Unable to write to ' . $toPath);
        }
    }

    /**
     * @param $zipPath
     * @param $unzippedPath
     * @return bool
     */
    private function unzip($zipPath, $unzippedPath)
    {
        $this->say("Unzipping {$zipPath}.");
        $zip = new \ZipArchive();
        $res = $zip->open($zipPath);
        if ($res === true) {
            $zip->extractTo($unzippedPath);
            $zip->close();
            return true;
        }
        return false;
    }

    /**
     * @param $basePath directory where driver is kept
     * @param string $urlBase the url chrome should respond to
     */
    private function runChromeWebDriver($basePath, $urlBase = '/wd/hub')
    {
        $os = new OperatingSystem();
        if ($os->isOsWindows()) {
            $this->say('Windows detected');
            $binPath = $basePath
                . DIRECTORY_SEPARATOR
                . 'chromedriver.exe';
        } elseif ($os->isOsLinux()) {
            $this->say('Linux detected');
            $binPath = $basePath
                . DIRECTORY_SEPARATOR
                . 'chromedriver';
            chmod($binPath, 100);
        } elseif ($os->isOsMacOSX()) {
            $this->say('macOS detected');
            $binPath = $basePath
                . DIRECTORY_SEPARATOR
                . 'chromedriver';
            chmod($binPath, 100);
        } elseif ($os->isOsBSD()) {
            $this->say('BSD detected');
            throw new \DomainException('Unsupported operating system');
        } elseif ($os->isOsSolaris()) {
            $this->say('Solaris detected');
            throw new \DomainException('Unsupported operating system');
        } elseif ($os->isOsUnknown()) {
            throw new \DomainException('Unknown operating system');
        } else {
            throw new \DomainException('Unable to detect operating system');
        }

        if (!file_exists($binPath)) {
            throw new \RuntimeException('Unable to find ChromeDriver ' . $binPath);
        }

        $this->say('Hint: open terminal and run `'.$os->toOsPath('./vendor/bin/codecept').' run [test suite] --env custom`');
        $this->say('Starting ChromeDriver');
        $this->_exec(
            $binPath
            . ' --url-base='
            . $urlBase
        );
    }
}
