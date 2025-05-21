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
class stic_PaymentsViewm182wizard extends SugarView {

    public function __construct() {
        parent::__construct();
    }

    public function preDisplay() {
        parent::preDisplay();

    }

    public function display() {
        parent::display();
        $this->ss->assign("LAB", $this->view_object_map, $list_label ?? null);
        $this->ss->assign("INT", $this->view_object_map, $list_intern ?? null);
        $this->ss->assign("ERR", $this->view_object_map, $error ?? null);
        $this->ss->assign("VAL", $this->view_object_map, $missingSettings ?? null);
        $this->ss->display('modules/stic_Payments/tpls/M182Wizard.tpl');
    }

}
