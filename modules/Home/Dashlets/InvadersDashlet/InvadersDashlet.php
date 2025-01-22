<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}
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


require_once('include/Dashlets/Dashlet.php');


#[\AllowDynamicProperties]
class InvadersDashlet extends Dashlet
{
    public $savedText; // users's saved text
    public $height = '100'; // height of the pad

    /**
     * Constructor
     *
     * @global string current language
     * @param guid $id id for the current dashlet (assigned from Home module)
     * @param array $def options saved for this dashlet
     */
    public function __construct($id, $def)
    {
        $this->loadLanguage('InvadersDashlet'); // load the language strings here

        if (!empty($def['height'])) { // set a default height if none is set
            $this->height = $def['height'];
        }

        parent::__construct($id); // call parent constructor

        $this->isConfigurable = false; // dashlet is configurable
        $this->hasScript = true;  // dashlet has javascript attached to it

        // if no custom title, use default
        if (empty($def['title'])) {
            $this->title = $this->dashletStrings['LBL_TITLE'];
        } else {
            $this->title = $def['title'];
        }
    }

    /**
     * Displays the dashlet
     *
     * @return string html to display dashlet
     */
    public function display()
    {
        $ss = new Sugar_Smarty();
        $ss->assign('id', $this->id);
        $ss->assign('height', $this->height);
        $ss->assign('dashletStrings', $this->dashletStrings);

        $str = $ss->fetch('modules/Home/Dashlets/InvadersDashlet/InvadersDashlet.tpl');
        return parent::display($this->dashletStrings['LBL_DBLCLICK_HELP']) . $str . '<br />'; // return parent::display for title and such
    }

    /**
     * Displays the javascript for the dashlet
     *
     * @return string javascript to use with this dashlet
     */
    public function displayScript()
    {
        $ss = new Sugar_Smarty();
        $ss->assign('id', $this->id);
        $ss->assign('dashletStrings', $this->dashletStrings);

        $str = $ss->fetch('modules/Home/Dashlets/InvadersDashlet/InvadersDashletScript.tpl');
        return $str; // return parent::display for title and such
    }

    /**
     * Displays the configuration form for the dashlet
     *
     * @return string html to display form
     */
    public function displayOptions()
    {
        global $app_strings;

        $ss = new Sugar_Smarty();
        $ss->assign('titleLbl', $this->dashletStrings['LBL_CONFIGURE_TITLE']);
        $ss->assign('heightLbl', $this->dashletStrings['LBL_CONFIGURE_HEIGHT']);
        $ss->assign('saveLbl', $app_strings['LBL_SAVE_BUTTON_LABEL']);
        $ss->assign('clearLbl', $app_strings['LBL_CLEAR_BUTTON_LABEL']);
        $ss->assign('title', $this->title);
        $ss->assign('height', $this->height);
        $ss->assign('id', $this->id);

        return parent::displayOptions() . $ss->fetch('modules/Home/Dashlets/InvadersDashlet/InvadersOptions.tpl');
    }

    /**
     * called to filter out $_REQUEST object when the user submits the configure dropdown
     *
     * @param array $req $_REQUEST
     * @return array filtered options to save
     */
    public function saveOptions($req)
    {
        global $sugar_config, $timedate, $current_user, $theme;
        $options = array();
        $options['title'] = $_REQUEST['title'];
        if (is_numeric($_REQUEST['height'])) {
            if ($_REQUEST['height'] > 0 && $_REQUEST['height'] <= 300) {
                $options['height'] = $_REQUEST['height'];
            } elseif ($_REQUEST['height'] > 300) {
                $options['height'] = '300';
            } else {
                $options['height'] = '100';
            }
        }

//        $options['savedText'] = br2nl($this->savedText);
        $options['savedText'] = $this->savedText;

        return $options;
    }

    /**
     * Used to save text on textarea blur. Accessed via Home/CallMethodDashlet.php
     * This is an example of how to to call a custom method via ajax
     */
    public function saveText()
    {
        $optionsArray = [];
        if (isset($_REQUEST['savedText'])) {
            $optionsArray = $this->loadOptions();
            $optionsArray['savedText'] = nl2br($_REQUEST['savedText']);
            $this->storeOptions($optionsArray);
        } else {
            $optionsArray['savedText'] = '';
        }
        $json = getJSONobj();
        echo 'result = ' . $json->encode(array('id' => $_REQUEST['id'],
                                       'savedText' => $optionsArray['savedText']));
    }
}
