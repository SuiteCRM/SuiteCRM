<?php
/**
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2021 SalesAgility Ltd.
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

namespace Helper;

use Codeception\Exception\ModuleException;
use Codeception\Module;
use SuiteCRM\Enumerator\DatabaseDriver;

/**
 * Class PhpBrowserDriverHelper
 * @package Helper
 * Helps to get configuration / environment variables for the PhpBrowser Driver
 */
#[\AllowDynamicProperties]
class PhpBrowserDriverHelper extends Module
{

    /**
     * @return array|mixed|null
     * @throws ModuleException
     */
    public function getConfig()
    {
        return $this->moduleContainer->getModule('PhpBrowser')->_getConfig();
    }

    /**
     * Gets the 'INSTANCE_URL' environment variable or 'url' in a yaml file.
     * @return string the test instance url.
     * @throws ModuleException
     */
    public function getInstanceURL()
    {
        return $this->getEnvironmentVariableOrDefault(
            'INSTANCE_URL',
            'http://localhost'
        );
    }

    /**
     * Gets the 'DATABASE_DRIVER' environment variable or 'database_driver' in a yaml file.
     * @return string
     * @throws ModuleException
     * @see DatabaseDriver
     */
    public function getDatabaseDriver()
    {
        return $this->getEnvironmentVariableOrDefault(
            'DATABASE_DRIVER',
            DatabaseDriver::MYSQL
        );
    }

    /**
     * Gets the 'DATABASE_NAME' environment variable or 'database_name' in a yaml file.
     * @return string
     * @throws ModuleException
     */
    public function getDatabaseName()
    {
        return $this->getEnvironmentVariableOrDefault(
            'DATABASE_NAME',
            'automated_tests'
        );
    }


    /**
     * Gets the 'DATABASE_HOST' environment variable or 'database_host' in a yaml file.
     * @return string
     * @throws ModuleException
     */
    public function getDatabaseHost()
    {
        return $this->getEnvironmentVariableOrDefault(
            'DATABASE_HOST',
            'database_host'
        );
    }

    /**
     * Gets the 'DATABASE_USER' environment variable or 'database_user' in a yaml file.
     * @return string the test instance url.
     * @throws ModuleException
     */
    public function getDatabaseUser()
    {
        return $this->getEnvironmentVariableOrDefault(
            'DATABASE_USER',
            'automated_tests'
        );
    }

    /**
     * Gets the 'DATABASE_PASSWORD' environment variable or 'database_password' in a yaml file.
     * @return string
     * @throws ModuleException
     */
    public function getDatabasePassword()
    {
        return $this->getEnvironmentVariableOrDefault(
            'DATABASE_PASSWORD',
            'automated_tests'
        );
    }


    /**
     * Gets the 'INSTANCE_ADMIN_USER' environment variable or 'instance_admin_user' in a yaml file.
     *
     * @return string
     * @throws ModuleException
     */
    public function getAdminUser()
    {
        return $this->getEnvironmentVariableOrDefault(
            'INSTANCE_ADMIN_USER',
            'admin'
        );
    }

    /**
     * Gets the 'INSTANCE_ADMIN_PASSWORD' environment variable or 'instance_admin_password' in a yaml file.
     * @return string
     * @throws ModuleException
     */
    public function getAdminPassword()
    {
        return $this->getEnvironmentVariableOrDefault(
            'INSTANCE_ADMIN_PASSWORD',
            'admin'
        );
    }

    /**
     * @return array|false|string
     * @throws ModuleException
     */
    public function getPasswordGrantClientId()
    {
        return $this->getEnvironmentVariableOrDefault(
            'INSTANCE_CLIENT_ID',
            'API-4c59-f678-cecc-6594-5a8d9c704473'
        );
    }

    /**
     * @return string
     * @throws ModuleException
     */
    public function getPasswordGrantClientSecret()
    {
        return $this->getEnvironmentVariableOrDefault(
            'INSTANCE_CLIENT_SECRET',
            'secret'
        );
    }

    /**
     * @return array|false|string
     * @throws ModuleException
     */
    public function getClientCredentialsGrantClientId()
    {
        return $this->getEnvironmentVariableOrDefault(
            'INSTANCE_CREDENTIALS_CLIENT_ID',
            'API-ea74-c352-badd-c2be-5a8d9c9d4351'
        );
    }

    /**
     * @return string
     * @throws ModuleException
     */
    public function getClientCredentialsGrantClientSecret()
    {
        return $this->getEnvironmentVariableOrDefault(
            'INSTANCE_CREDENTIALS_CLIENT_SECRET',
            'secret'
        );
    }

    /**
     * @param string $variable
     * @param string $default
     * @return string
     * @throws ModuleException
     */
    private function getEnvironmentVariableOrDefault($variable, $default)
    {
        $upperCase = strtoupper($variable);
        $lowerCase = strtoupper($variable);

        $env = getenv($upperCase);
        if ($env === false) {
            $config = $this->moduleContainer->getModule('PhpBrowser')->_getConfig();
            if (empty($config[$upperCase])) {
                return $default;
            }

            return $config[$lowerCase];
        }

        return $env;
    }
}
