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
require_once 'include/Dashlets/Dashlet.php';

#[\AllowDynamicProperties]
class SpotsDashlet extends Dashlet
{
    public $pivotId;
    public $showGui;

    /**
     * Constructor.
     *
     * @global string current language
     *
     * @param guid  $id  id for the current dashlet (assigned from Home module)
     * @param array $def options saved for this dashlet
     */
    public function __construct($id, $def)
    {
        $this->loadLanguage('SpotsDashlet', 'modules/Spots/Dashlets/'); // load the language strings here

        $this->isRefreshable = false;

        if (!empty($def['spotId'])) {
            $this->spotId = $def['spotId'];
        } else {
            $this->spotId = '';
        }

        if (!empty($def['showGui']) && $def['showGui'] === 'on') {
            $this->showGui = 1;
        } else {
            $this->showGui = 0;
        }

        parent::__construct($id); // call parent constructor

        $this->isConfigurable = true; // dashlet is configurable
        $this->hasScript = true;  // dashlet has javascript attached to it

        // if no custom title, use default
        if (empty($def['title'])) {
            $this->title = $this->dashletStrings['LBL_TITLE'];
        } else {
            $this->title = $def['title'];
        }
    }

    public function checkIfSpotHasBeenDeleted($spotId)
    {
        $spotBean = BeanFactory::getBean('Spots', $spotId);

        return $spotBean === false;
    }

    /**
     * Displays the dashlet.
     *
     * @return string html to display dashlet
     */
    public function display()
    {

        //As the dashlet may point to a pivot that has been marked as deleted, check this here

        if (is_null($this->spotId) || $this->spotId === '') {
            return parent::display('').'<span style="margin-left:10px;" class="dashletAnalyticMessage">'.$this->dashletStrings['LBL_NO_SPOTS_SELECTED'].'</span><br />'; // return parent::display for title and such
        } else {
            if ($this->checkIfSpotHasBeenDeleted($this->spotId)) {
                return parent::display('').'<span style="margin-left:10px;" class="dashletAnalyticMessage">'.$this->dashletStrings['LBL_SPOTS_POINTED_DELETED'].'</span><br />'; // return parent::display for title and such
            }

            $ss = new Sugar_Smarty();
            $ss->assign('id', $this->id);
            $ss->assign('showUI', $this->showGui);
            $ss->assign('spotToLoad', $this->spotId);

            $spot = BeanFactory::getBean('Spots', $this->spotId);

            $ss->assign('config', $spot->config);
            $ss->assign('type', $spot->type);

            $str = $ss->fetch('modules/Spots/Dashlets/SpotsDashlet/SpotsDashlet.tpl');

            return parent::display().$str.'<br />'; // return parent::display for title and such
        }
    }

    /**
     * Displays the javascript for the dashlet.
     *
     * @return string javascript to use with this dashlet
     */
    public function displayScript()
    {
    }

    /**
     * Displays the configuration form for the dashlet.
     *
     * @return string html to display form
     */
    public function displayOptions()
    {
        global $app_strings;

        $ss = new Sugar_Smarty();
        $ss->assign('titleLbl', $this->dashletStrings['LBL_CONFIGURE_TITLE']);
        $ss->assign('saveLbl', $app_strings['LBL_SAVE_BUTTON_LABEL']);
        $ss->assign('spotToLoadTitleLbl', $this->dashletStrings['LBL_SPOTS_TO_LOAD']);
        $ss->assign('showUILbl', $this->dashletStrings['LBL_SHOW_UI']);
        $ss->assign('title', $this->title);
        $ss->assign('id', $this->id);
        $ss->assign('showUI', $this->showGui);
        $ss->assign('spotToLoad', $this->spotId);

        $ss->assign('spots', $this->getSpotsList());

        return parent::displayOptions().$ss->fetch('modules/Spots/Dashlets/SpotsDashlet/SpotsDashletOptions.tpl');
    }

    /**
     * Returns a json_encoded string of the available spots names.
     *
     *
     * @return json_encoded string of the list of available spots names
     */
    public function getSpotsList()
    {
        $spotBean = BeanFactory::getBean('Spots');
        $beanList = $spotBean->get_full_list('name');
        $returnArray = [];
        if (!is_null($beanList)) {
            foreach ($beanList as $b) {
                $bean = new stdClass();
                $bean->type = $b->type;
                $bean->config = htmlspecialchars_decode((string) $b->config);
                $bean->name = $b->name;
                $bean->id = $b->id;
                $returnArray[] = $bean;
            }
        }

        return json_encode($returnArray);
    }

    /**
     * called to filter out $_REQUEST object when the user submits the configure dropdown.
     *
     * @param array $req $_REQUEST
     *
     * @return array filtered options to save
     */
    public function saveOptions($req)
    {
        $options = array();
        $options['title'] = $_REQUEST['title'];
        if (isset($_REQUEST['showGui'])) {
            $options['showGui'] = $_REQUEST['showGui'];
        } else {
            $options['showGui'] = '';
        }
        if (isset($_REQUEST['spots'])) {
            $options['spotId'] = $_REQUEST['spots'];
        } else {
            $options['spotId'] = '';
        }

        return $options;
    }
}
