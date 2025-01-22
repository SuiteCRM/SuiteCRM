<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2019 SalesAgility Ltd.
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

use SuiteCRM\Robo\Traits\CliRunnerTrait;
use Symfony\Component\Finder\Finder;
use Symfony\Component\Finder\SplFileInfo;

/**
 * Class CleanCacheCommands
 *
 * @category RoboTasks
 * @package  SuiteCRM\Robo\Plugin\Commands
 * @author   Jose C. Massón <jose@gcoop.coop>
 * @license  GNU GPLv3
 * @link     CleanCacheCommands
 */

#[\AllowDynamicProperties]
class CleanCacheCommands extends \Robo\Tasks
{
    use CliRunnerTrait;

    /**
     * Clean 'cache/' directory

     * @param array $opts
     * @throws \RuntimeException
     * @return void
     *
     * @command cache:clean
     * @aliases clean:cache
     * @option  force Force clean directories without confirmation
     */
    public function cleanCache($opts = ['force' => false])
    {
        global $sugar_config;
        $this->bootstrap();
        $cacheDir = isset($sugar_config) && isset($sugar_config['cache_dir']) ? $sugar_config['cache_dir'] : 'cache';

        $toDelete = array();
        $doNotDelete = array('Emails', 'emails', '.', '..');
        $cachesToDelete = array(
                               'Relationships',
                               'csv',
                               'dashlets',
                               'diagnostics',
                               'dynamic_fields',
                               'feeds',
                               'import',
                               'include/javascript',
                               'jsLanguage',
                               'pdf',
                               'themes',
                               'xml',
        );

        // Calculate sub-caches to clear
        $subCachesToDelete = new Finder();
        $subCachesToDelete
            ->directories()
            ->in($cacheDir)
            ->filter(function (SplFileInfo $directory) use ($cachesToDelete) {
                return in_array($directory->getRelativePathname(), $cachesToDelete);
            });

        $this->say("Found Sub-Cache Directories to Clean: ");
        $this->io()->listing(iterator_to_array($subCachesToDelete));
        $toDelete = array_merge($toDelete, iterator_to_array($subCachesToDelete));

        // Calculate cached modules to clear
        $moduleCachesToDelete = new Finder();
        $moduleCachesToDelete
            ->directories()
            ->depth(' == 0')
            ->in($cacheDir . 'modules')
            ->exclude($doNotDelete);

        $this->say("Found Module-Cache Directories to Clean: ");
        $this->io()->listing(iterator_to_array($moduleCachesToDelete));
        $toDelete = array_merge($toDelete, iterator_to_array($moduleCachesToDelete));

        // Confirm and clean cache directories
        $confirm = $opts['force'] || $this->confirm('Would you like to clean the above caches?');
        if ($confirm) {
            $this->_cleanDir($toDelete);
        }
    }
}
