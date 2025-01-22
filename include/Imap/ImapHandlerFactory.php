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

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once __DIR__ . '/ImapHandler.php';
require_once __DIR__ . '/Imap2Handler.php';
require_once __DIR__ . '/ImapHandlerException.php';

/**
 * ImapHandlerFactory
 * Retrieves an ImapHandlerInterface. It could be ImapHandler or ImapHandlerFake.
 * Use `$sugar_config['imap_test'] = true` in config_override.php to set test mode on.
 *
 * @author gyula
 */
#[\AllowDynamicProperties]
class ImapHandlerFactory
{
    public const SETTINGS_KEY_FILE = '/ImapTestSettings.txt';
    public const DEFAULT_SETTINGS_KEY = 'testSettingsOk';

    /**
     *
     * @var ImapHandlerInterface
     */
    protected $interfaceObject = null;

    /**
     *
     * @var array
     */
    protected $imapHandlerTestInterface = [
        'file' => 'include/Imap/ImapHandlerFake.php',
        'class' => 'ImapHandlerFake',
        'calls' => 'include/Imap/ImapHandlerFakeCalls.php',
    ];

    /**
     *
     * @param ImapHandlerInterface $interfaceObject
     */
    protected function setInterfaceObject(ImapHandlerInterface $interfaceObject)
    {
        $class = get_class($interfaceObject);
        LoggerManager::getLogger()->debug('ImapHandlerFactory will using a ' . $class);
        $this->interfaceObject = $interfaceObject;
    }

    /**
     *
     */
    protected function includeFakeInterface()
    {
        if (!class_exists($this->imapHandlerTestInterface['class'])) {
            require_once $this->imapHandlerTestInterface['file'];
        }
    }

    /**
     *
     * @global array $sugar_config
     * @param string $testSettings
     * @throws ImapHandlerException
     */
    protected function loadTestSettings($testSettings = null)
    {
        if (!$testSettings) {
            $testSettings = $this->getTestSettings();
        }
        $this->includeFakeInterface();
        $interfaceClass = $this->imapHandlerTestInterface['class'];
        $interfaceCallsSettings = include $this->imapHandlerTestInterface['calls'];

        if (!isset($interfaceCallsSettings[$testSettings])) {
            $info = "[debug testSettings] " . var_dump($testSettings, true);
            $info .= "\n[debug info this] " . var_dump($this, true);
            $info .= "\n[debug info callset] " . var_dump($interfaceCallsSettings, true);
            LoggerManager::getLogger()->debug('Imap test setting failure: ' . $info);
            throw new ImapHandlerException(
                "Test settings does not exists: $testSettings",
                ImapHandlerException::ERR_TEST_SET_NOT_EXISTS
            );
        }

        $interfaceCalls = $interfaceCallsSettings[$testSettings];
        $interfaceFakeData = new ImapHandlerFakeData();
        $interfaceFakeData->retrieve($interfaceCalls);
        $this->setInterfaceObject(new $interfaceClass($interfaceFakeData));
    }

    /**
     *
     * @return string
     */
    protected function getTestSettings()
    {
        $testSettings = null;
        if (file_exists(__DIR__ . self::SETTINGS_KEY_FILE)) {
            $testSettings = file_get_contents(__DIR__ . self::SETTINGS_KEY_FILE);
        }
        if (!$testSettings) {
            LoggerManager::getLogger()->warn("Test settings not set, create one with default key");
            $testSettings = self::DEFAULT_SETTINGS_KEY;
            $this->saveTestSettingsKey($testSettings);
        }

        return $testSettings;
    }

    /**
     * Delete's test settings file.
     * @return void
     */
    public function deleteTestSettings()
    {
        if (file_exists(__DIR__ . self::SETTINGS_KEY_FILE)) {
            unlink(__DIR__ . self::SETTINGS_KEY_FILE);
        }
    }

    /**
     *
     * @param string $key
     * @return boolean
     * @throws ImapHandlerException
     */
    public function saveTestSettingsKey($key)
    {
        if (!is_string($key) || !$key) {
            $type = gettype($key);
            throw new InvalidArgumentException('Key should be a non-empty string, ' . ($type == 'string' ? 'empty string' : $type) . ' given.');
        }

        $calls = include $this->imapHandlerTestInterface['calls'];

        if (!isset($calls[$key])) {
            throw new ImapHandlerException(
                'Key not found: ' . $key,
                ImapHandlerException::ERR_KEY_NOT_FOUND
            );
        } else {
            if (!file_put_contents(__DIR__ . self::SETTINGS_KEY_FILE, $key)) {
                throw new ImapHandlerException('Key saving error', ImapHandlerException::ERR_KEY_SAVE_ERROR);
            } else {
                return true;
            }
        }
        return false;
    }

    /**
     * Get Handler
     * @param string|null $testSettings
     * @return ImapHandlerInterface
     * @throws ImapHandlerException
     */
    public function getImapHandler(string $testSettings = null, string $handlerType = 'native' )
    {
        if (null === $this->interfaceObject) {
            global $sugar_config, $log;

            $test = (isset($sugar_config['imap_test']) && $sugar_config['imap_test']) || $testSettings;
            $charset = (isset($sugar_config['default_email_charset'])) ? $sugar_config['default_email_charset'] : null;

            if (inDeveloperMode()) {
                $logErrors = true;
                $logCalls = true;
            } else {
                $logErrors = true;
                $logCalls = false;
            }

            $interfaceClass = $this->getHandlerClass($handlerType);
            $log->debug('Using imap handler class: ' . $interfaceClass);

            if ($test) {
                $this->loadTestSettings($testSettings);
            } else {
                $this->setInterfaceObject(new $interfaceClass($logErrors, $logCalls, $charset));
            }
        }
        return $this->interfaceObject;
    }

    /**
     * Check if all handlers are available
     * @return bool
     */
    public function areAllHandlersAvailable(): bool {
        foreach ($this->getHandlers() as $handlerClass) {
            $available = (new $handlerClass())->isAvailable();
            if ($available === false) {
                return false;
            }
        }

        return true;
    }

    /**
     * Get Handler
     * @param string $handlerType
     * @return string
     */
    protected function getHandlerClass(string $handlerType): string
    {
        $handlers = $this->getHandlers();

        if (!empty($handlers[$handlerType])) {
            return $handlers[$handlerType];
        }

        return ImapHandler::class;
    }

    /**
     * @return ImapHandlerInterface[]
     */
    protected function getHandlers(): array
    {
        return [
            'native' => ImapHandler::class,
            'imap2' => Imap2Handler::class,
        ];
    }
}
