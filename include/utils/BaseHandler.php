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

#[\AllowDynamicProperties]
class BaseHandler
{
    /**
     *
     * @var string
     */
    protected $tplPath = '';

    /**
     *
     * @var Sugar_Smarty
     */
    protected $ss = null;

    /**
     *
     * @var array
     */
    protected $request = null;

    /**
     *
     * @var array
     */
    protected $modStrings = null;

    /**
     * Set up the object
     *
     * @param Sugar_Smarty $sugar_smarty
     * @param array        $request
     * @param array        $mod_strings
     */
    public function __construct(Sugar_Smarty $sugar_smarty, $request, $mod_strings)
    {
        $this->ss          = $sugar_smarty;
        $this->request     = $request;
        $this->modStrings  = $mod_strings;

        $this->getLanguage();
        $this->getJavascipt();
    }

    /**
     * Get Languages
     *
     * @return void
     */
    protected function getLanguage()
    {
        $this->ss->assign('LANGUAGES', $this->protectedLanguage());
    }

    /**
     * Get Javascript
     *
     * @return void
     */
    protected function getJavascipt()
    {
        $this->ss->assign("JAVASCRIPT", $this->protectedJavascript());
    }

    /**
     * protected function the languages
     *
     * @return array
     */
    protected function protectedLanguage()
    {
        return get_languages();
    }

    /**
     * protected function for javascript
     *
     * @return array
     */
    protected function protectedJavascript()
    {
        return get_set_focus_js();
    }

    /**
     * protected function for SugarApplication::redirect() so test mock can override it
     *
     * @param string $url
     */
    protected function redirect($url)
    {
        SugarApplication::redirect($url);
    }

    /**
     * protected function for exit so test mock can override it
     *
     * @return void
     */
    protected function protectedExit()
    {
        exit;
    }

    /**
     * protected function for die() so test mock can override it
     *
     * @param string $exitstring
     */
    protected function protectedDie($exitstring)
    {
        die($exitstring);
    }
}
