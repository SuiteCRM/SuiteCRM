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

use Robo\Tasks;
use RuntimeException;
use SuiteCRM\Utility\OperatingSystem;
use SuiteCRM\Robo\Traits\RoboTrait;

#[\AllowDynamicProperties]
class BuildCommands extends Tasks
{
    use RoboTrait;

    // define public methods as commands


    /**
     * Compile a theme (SASS) based in SuiteP
     * @param array $opts optional command line arguments
     * theme - The name of the theme you want to compile css
     * color-scheme - set which color scheme you wish to build
     * @throws RuntimeException
     */
    public function buildTheme(array $opts = ['theme' => '', 'color-scheme' => ''])
    {
        if (empty($opts['theme'])) {
            $this->say("Please specify the name of the theme you want to compile with '--theme=SuiteP'");
            return;
        }
        $this->say("Compile {$opts['theme']} Theme (SASS)");
        if (empty($opts['color-scheme'])) {
            /** Look for Subthemes in the {$opts['theme']} theme Dir **/
            $std = "themes/{$opts['theme']}/css/";
            $this->locateSubTheme($std);
            /** Look for Subthemes in the custom/theme Dir **/
            // Good opportunity to refactor here.
            // Does the same as above just looks in the custom directory.
            $ctd = "custom/themes/{$opts['theme']}/css/";
            $this->locateSubTheme($ctd);
            return;
        }

        $location = "themes/{$opts['theme']}/css/";

        if (is_array($opts['color-scheme'])) {
            foreach ($opts['color-scheme'] as $colorScheme) {
                $this->buildColorScheme($colorScheme, $location);
            }

            return;
        }

        $this->buildColorScheme($opts['color-scheme'], $location);
        $this->say("Compile {$opts['theme']} Theme (SASS) Complete");
    }


    /**
     * Build SuiteP theme
     * @param array $opts optional command line arguments
     * color-scheme - set which color scheme you wish to build
     * @throws RuntimeException
     */
    public function buildSuiteP(array $opts = ['color-scheme' => ''])
    {
        $this->buildTheme(['theme' => 'SuiteP', 'color-scheme' => $opts['color-scheme']]);
    }

    /**
     * @param string $colorScheme eg Dawn
     * @param string $location eg Directory to work from
     * @throws RuntimeException
     */
    private function buildColorScheme($colorScheme, $location)
    {
        $os = new OperatingSystem();
        $command =
            $os->toOsPath('./vendor/bin/pscss')
            . ' -s compressed '
            . $os->toOsPath("{$location}{$colorScheme}/style.scss")
            . ' > '
            . $os->toOsPath("{$location}{$colorScheme}/style.css");
        $this->_exec($command);
    }

    /**
     * @param string $directory
     */
    private function locateSubTheme($directory)
    {
        if (is_dir($directory) && $dir = opendir($directory)) {
            while (false !== ($file = readdir($dir))) {
                if (filetype($directory . $file) === 'dir' && file_exists($directory . $file . '/style.scss')) {
                    if (file_exists($directory . $file . '/style.css')) {
                        $this->say("Found style.css for {$file}, Removing");
                        unlink($directory . $file . '/style.css');
                    }
                    $this->say("Found style.scss for {$file}, Compiling");
                    $this->buildColorScheme($file, $directory);
                }
            }
        } else {
            $this->say("The folder {$directory} does not exists or it's not possible to open it.");
        }
    }
}
