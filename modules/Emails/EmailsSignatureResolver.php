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

/**
 * EmailsSignatureResolver
 *
 * @author gyula
 */
#[\AllowDynamicProperties]
class EmailsSignatureResolver
{
    public const ERR_HTML_AMBIGUOUS = 301;
    public const ERR_HTML_NONE = 302;
    public const ERR_PLAINTEXT_AMBIGUOUS = 303;
    public const ERR_PLAINTEXT_NONE = 304;
    
    /**
     *
     * @var array
     */
    protected $signatureArray;
    
    /**
     *
     * @var string
     */
    protected $html;
    
    /**
     *
     * @var string
     */
    protected $plaintext;
    
    /**
     *
     * @var array
     */
    protected $errors;
    
    /**
     *
     * @var bool
     */
    protected $noDefaultAvailable;
    
    /**
     *
     * @param array $signatureArray
     * @return array errors
     */
    public function setSignatureArray($signatureArray)
    {
        $this->signatureArray = $signatureArray;
        $this->errors = [];
        $this->html = $this->resolveHtml();
        $this->plaintext = $this->resolvePlaintext();
        $this->noDefaultAvailable = false;
        if (in_array(self::ERR_HTML_NONE, $this->errors) && in_array(self::ERR_PLAINTEXT_NONE, $this->errors)) {
            $this->noDefaultAvailable = true;
        }
        return $this->errors;
    }
    
    /**
     *
     * @return string|null this function returns null and/or set errors variable if error(s) occured
     */
    protected function resolveHtml()
    {
        if (isset($this->signatureArray['html']) && $this->signatureArray['html']) {
            if (isset($this->signatureArray['signature_html']) && $this->signatureArray['signature_html'] &&
                    $this->signatureArray['signature_html'] != $this->signatureArray['html']) {
                $this->errors[] = self::ERR_HTML_AMBIGUOUS;
                LoggerManager::getLogger()->error('Ambiguous signature html found!');
            }
            return $this->signatureArray['html'];
        }
        if (isset($this->signatureArray['signature_html']) && $this->signatureArray['signature_html']) {
            return $this->signatureArray['signature_html'];
        }
        $this->errors[] = self::ERR_HTML_NONE;
        LoggerManager::getLogger()->error('Signature html not found!');
        return null;
    }
    
    /**
     *
     * @return string|null this function returns null and/or set errors variable if error(s) occured
     */
    protected function resolvePlaintext()
    {
        if (isset($this->signatureArray['plain']) && $this->signatureArray['plain']) {
            if (isset($this->signatureArray['signature']) && $this->signatureArray['signature'] &&
                    $this->signatureArray['signature'] != $this->signatureArray['plain']) {
                $this->errors[] = self::ERR_PLAINTEXT_AMBIGUOUS;
                LoggerManager::getLogger()->error('Ambiguous signature plain text found!');
            }
            return $this->signatureArray['plain'];
        }
        if (isset($this->signatureArray['signature']) && $this->signatureArray['signature']) {
            return $this->signatureArray['signature'];
        }
        $this->errors[] = self::ERR_PLAINTEXT_NONE;
        LoggerManager::getLogger()->error('Signature plain text not found!');
        return null;
    }
    
    /**
     *
     * @return string
     */
    public function getHtml()
    {
        return $this->html;
    }
    
    /**
     *
     * @return string
     */
    public function getPlaintext()
    {
        return $this->plaintext;
    }
    
    /**
     *
     * @return bool
     */
    public function isNoDefaultAvailable()
    {
        return $this->noDefaultAvailable;
    }
}
