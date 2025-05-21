<?php
/**
 * This file is part of SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation.
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
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 */

require_once 'include/MVC/View/SugarView.php';
require_once 'SticInclude/Views.php';

#[\AllowDynamicProperties]
class stic_Custom_ViewsViewselectmodule extends SugarView
{
    private $buttons;
    private $title;

    public function __construct()
    {
        if (isset($_REQUEST['view'])) {
            $this->view = $_REQUEST['view'];
        }

        $this->editModule = (!empty($_REQUEST['view_module'])) ? $_REQUEST['view_module'] : null;
        $this->buttons = array(); // initialize so that modules without subpanels for example don't result in this being unset and causing problems in the smarty->assign
    }

    public function preDisplay()
    {
        parent::preDisplay();

        SticViews::preDisplay($this);

        // Write here the SinergiaCRM code that must be executed for this module and view
    }

    public function display()
    {
        parent::display();

        SticViews::display($this);

        $this->title = translate('LBL_MODULE_TITLE');
        $this->generateStudioModuleButtons();

        $this->ss->assign("title", $this->title);
        $this->ss->assign('buttons', $this->buttons);

        echo $this->ss->fetch('modules/stic_Custom_Views/tpls/selectmodule.tpl');
        echo getVersionedScript("modules/stic_Custom_Views/Utils.js");
    }

    public function generateStudioModuleButtons()
    {
        require_once 'modules/ModuleBuilder/Module/StudioBrowser.php';
        $sb = new StudioBrowser();
        $nodes = $sb->getNodes();
        $this->buttons = array();

        foreach ($nodes as $module) {
            $this->buttons[$module['name']] = [
                'linkId' => 'customViewlink_' . $module['module'],
                'module' => $module['module'],
                'icon' => $module['icon'],
            ];
        }
    }
}
