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

use SuiteCRM\LangText;

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

include_once __DIR__ . '/ImapHandlerFactory.php';
require_once __DIR__ . '/ImapHandlerException.php';

/**
 * ImapTestSettingsEntryHandler
 *
 * @author gyula
 */
#[\AllowDynamicProperties]
class ImapTestSettingsEntryHandler
{

    /**
     *
     * @param array $sugarConfig
     * @param array $request
     * @return string entry point output as string
     * @throws InvalidArgumentException
     */
    public function handleEntryPointRequest($sugarConfig, $request)
    {
        if (!isset($sugarConfig['imap_test']) || !$sugarConfig['imap_test']) {
            throw new InvalidArgumentException('IMAP test config is required, use $sugar_config[\'imap_test\'].');
        }
        
        $var = 'imap_test_settings';
        $key = isset($request[$var]) && $request[$var] ? $request[$var] : null;
        if (null === $key) {
            throw new InvalidArgumentException("Invalid request variable at '$var'.");
        }
        
        $error = $this->doSaveTestSettingsKey($key, $var);
        
        if (!empty($error)) {
            $output = LangText::get('IMAP_HANDLER_ERROR', ['error' => $error, 'key' => $key]);
        } else {
            $output = LangText::get('IMAP_HANDLER_SUCCESS', ['key' => $key]);
        }
        
        return $output;
    }
    
    /**
     *
     * @param string $key
     * @param string $var
     * @return string|null return a translated error message if error occurred
     */
    protected function doSaveTestSettingsKey($key, $var)
    {
        if (!$key) {
            $error = LangText::get('IMAP_HANDLER_ERROR_INVALID_REQUEST', ['var' => $var]);
        } else {
            $imapHandlerFactory = new ImapHandlerFactory();
            try {
                $error = $this->getSaveTestSettingsKey($imapHandlerFactory, $key);
            } catch (ImapHandlerException $e) {
                $error = $this->handleImapHandlerException($e);
            }
        }
        return $error;
    }
    
    /**
     *
     * @param ImapHandlerFactory $imapHandlerFactory
     * @param string $key
     * @return string|null return a translated error message if error occurred
     */
    protected function getSaveTestSettingsKey(ImapHandlerFactory $imapHandlerFactory, $key)
    {
        $error = null;
        if (!$imapHandlerFactory->saveTestSettingsKey($key)) {
            $error = LangText::get('IMAP_HANDLER_ERROR_UNKNOWN_BY_KEY', ['key' => $key]);
        }
        return $error;
    }
    
    /**
     *
     * @param ImapHandlerException $e
     * @return string return a translated error message
     */
    protected function handleImapHandlerException(ImapHandlerException $e)
    {
        $code = $e->getCode();
        LoggerManager::getLogger()->error('ImapHandlerException detected: ' . $e->getMessage() . ' #' . $code);
                
        switch ($code) {
            case ImapHandlerException::ERR_TEST_SET_NOT_EXISTS:
                $error = LangText::get('IMAP_HANDLER_ERROR_NO_TEST_SET');
                break;

            case ImapHandlerException::ERR_KEY_NOT_FOUND:
                $error = LangText::get('IMAP_HANDLER_ERROR_NO_KEY');
                break;

            case ImapHandlerException::ERR_KEY_SAVE_ERROR:
                $error = LangText::get('IMAP_HANDLER_ERROR_KEY_SAVE');
                break;

            default:
                $error = LangText::get('IMAP_HANDLER_ERROR_UNKNOWN');
                break;
        }
        return $error;
    }
}
