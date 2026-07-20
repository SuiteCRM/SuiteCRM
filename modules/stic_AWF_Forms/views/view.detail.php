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
require_once 'include/MVC/View/views/view.detail.php';
require_once 'SticInclude/Views.php';

class stic_AWF_FormsViewDetail extends ViewDetail
{

    public function __construct()
    {
        parent::__construct();

    }

    public function preDisplay()
    {
        parent::preDisplay();

        SticViews::preDisplay($this);

        // Bootstrap (modified with scoped classes: do not crash current layout!)
        echo "<link rel='stylesheet' href='". getVersionedPath("SticInclude/vendor/bootstrap/css/bootstrap.scoped.min.css"). "'>";
        echo getVersionedScript("SticInclude/vendor/bootstrap/js/bootstrap.bundle.min.js");

        // Alpinejs
        // echo '<script src="//unpkg.com/alpinejs" defer></script>';
        echo '<script src="' . getVersionedPath('SticInclude/vendor/alpine/alpine.min.js') . '" defer></script>';

        // AWF 
        echo getVersionedScript("modules/stic_AWF_Forms/js/stic_AwfClasses.js");
        echo getVersionedScript("modules/stic_AWF_Forms/js/utils.js");
        echo "<link rel='stylesheet' href='". getVersionedPath("modules/stic_AWF_Forms/ui/Common/css/sticControls.css"). "'>";

        // Details
        echo getVersionedScript("modules/stic_AWF_Forms/ui/DetailView/js/details.js");
    }

    public function display()
    {
        parent::display();

        SticViews::display($this);

        $beanArray = $this->bean->toArray();
        foreach ($beanArray as $key => $value) {
            if (is_string($value)) {
                $beanArray[$key] = html_entity_decode($value, ENT_QUOTES, 'UTF-8');
            }
        }
        $this->ss->assign('beanJson', json_encode($beanArray));
        
        // DetailView: Custom Details 
        echo $this->ss->fetch('modules/stic_AWF_Forms/ui/DetailView/tpl/details.tpl');
    }

}
