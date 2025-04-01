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

use Symfony\Component\Console\Input\InputOption;

#[\AllowDynamicProperties]
class TestRunCommands extends \Robo\Tasks
{
    use \SuiteCRM\Robo\Traits\RoboTrait;

    /**
     * Run install test suite with the custom env.
     * 
     * @param string $fileOrDirectory Provide a path to a file or directory to
     *   run a specific test/specific directory of tests.
     * @param array $opts
     * @option debug Whether to have the test suite output extra information.
     * @option fail-fast Stop after first failure.
     * @usage tests:install
     * @usage tests:install ./tests/install/UserWizardCest.php
     * @usage tests:install --debug ./tests/install/UserWizardCest.php
     */
    public function TestsInstall($fileOrDirectory = null, $opts = ['debug' => false, 'fail-fast' => false]) {
      $this->say('Running Codeception Install Test Suite.');

      $command = "./vendor/bin/codecept run install --env custom {$fileOrDirectory}";

      if ($opts['debug']) {
        $command .= ' -vvv -d';
      }
      if ($opts['fail-fast']) {
        $command .= ' -f';
      }
      
      return $this->_exec($command);
    }

    /**
     * Run API test suite.
     * 
     * @param string $fileOrDirectory Provide a path to a file or directory to
     *   run a specific test/specific directory of tests.
     * @param array $opts
     * @option debug Whether to have the test suite output extra information.
     * @option fail-fast Stop after first failure.
     * @usage tests:api
     * @usage tests:api ./tests/api/V8/GetFieldsMetaCest.php
     * @usage tests:api ./tests/api/V8/
     * @usage tests:api --debug ./tests/api/V8/GetFieldsMetaCest.php
     */
    public function TestsAPI($fileOrDirectory = null, $opts = ['debug' => false, 'fail-fast' => false]) {
      $this->say('Running Codeception API Test Suite.');

      $command = "./vendor/bin/codecept run api {$fileOrDirectory}";

      if ($opts['debug']) {
        $command .= ' -vvv -d';
      }
      if ($opts['fail-fast']) {
        $command .= ' -f';
      }
      
      return $this->_exec($command);
    }

    /**
     * Run acceptance test suite with the custom env.
     * 
     * @param string $fileOrDirectory Provide a path to a file or directory to
     *   run a specific test/specific directory of tests.
     * @param array $opts
     * @option debug Whether to have the test suite output extra information.
     * @option fail-fast Stop after first failure.
     * @usage tests:acceptance
     * @usage tests:acceptance ./tests/acceptance/modules/Calendar/CalendarCest.php
     * @usage tests:acceptance ./tests/acceptance/modules/
     * @usage tests:acceptance --debug ./tests/acceptance/modules/Calendar/CalendarCest.php
     */
    public function TestsAcceptance($fileOrDirectory = null, $opts = ['debug' => false, 'fail-fast' => false]) {
      $this->say('Running Codeception Acceptance Test Suite.');

      $command = "./vendor/bin/codecept run acceptance --env custom {$fileOrDirectory}";

      if ($opts['debug']) {
        $command .= ' -vvv -d';
      }
      if ($opts['fail-fast']) {
        $command .= ' -f';
      }
      
      return $this->_exec($command);
    }

    /**
     * Run PHPUnit unit test suite.
     * 
     * @param string $fileOrDirectory Provide a path to a file or directory to
     *   run a specific test/specific directory of tests.
     * @param array $opts
     * @option debug Whether to have the test suite output extra information.
     * @option fail-fast Stop after first failure.
     * @usage tests:unit
     * @usage tests:unit ./tests/unit/phpunit/modules/Favorites/FavoritesTest.php
     * @usage tests:unit ./tests/unit/phpunit/modules/
     * @usage tests:unit --filter testdeleteFavorite ./tests/unit/phpunit/modules/Favorites/FavoritesTest.php
     * @usage tests:unit --debug ./tests/unit/phpunit/modules/Favorites/FavoritesTest.php
     */
    public function TestsUnit($fileOrDirectory = './tests/unit/phpunit', $opts = ['debug' => false, 'fail-fast' => false, 'filter' => InputOption::VALUE_REQUIRED]) {
      $this->say('Running PHPUnit Unit Test Suite.');

      $command = "./vendor/bin/phpunit --colors --configuration ./tests/phpunit.xml.dist {$fileOrDirectory}";
      
      if ($opts['debug']) {
        $command .= ' -v --debug';
      }
      if ($opts['fail-fast']) {
        $command .= ' --stop-on-error --stop-on-failure';
      }
      if ($opts['filter']) {
          $command .= ' --filter ' . $opts['filter'];
      }

      return $this->_exec($command);
    }
}
