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

if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

class stic_MessagesViewConversation extends SugarView {

    public $messages = [];
    public $parentName = '';
    public $parentId = '';
    public $parentType = '';
    public $contactPhone = '';
    public $windowOpen = false;
    public $windowMessage = '';
    public $newMessageUrl = '';
    public $modStrings = [];
    public $pollDelay = 5000;

    public function display() {
        global $sugar_config;
        $messages = $this->messages;
        $parentName = $this->parentName;
        $parentId = $this->parentId;
        $parentType = $this->parentType;
        $windowOpen = $this->windowOpen;
        $windowMessage = $this->windowMessage;
        $newMessageUrl = $this->newMessageUrl;
        $mod_strings = $this->modStrings;
        $pollDelay = $sugar_config['stic_conversation_poll_delay'] ?? 5000;

        include 'modules/stic_Messages/include/ConversationView/ConversationView.tpl';
    }
}
