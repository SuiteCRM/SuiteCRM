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
// Prevents directly accessing this file from a web browser
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

/**
 * Abstract class for deferred actions that operate on ONE data block.
 * Automates the definition, obtaining and validation of the DataBlock parameter.
 */
abstract class DeferredDataBlockActionDefinition extends ServerDataBlockActionDefinition implements IDeferredAction {
    /**
     * Returns the type of the action, which is ActionType::DEFERRED for all classes extending DeferredActionDefinition.
     * This method is final to ensure that all deferred actions consistently return the correct type which is ActionType::DEFERRED.
     * @return ActionType The type of the action, which is ActionType::DEFERRED
     */
    final public function getType(): ActionType {
        return ActionType::DEFERRED;
    }

    /**
     * Returns the Subflow success label
     */
    public function getFlowSuccessLabel(): string { return $this->translate('FLOW_SUCCESS'); }

    /**
     * Returns the Subflow error label
     */
    public function getFlowErrorLabel(): string { return $this->translate('FLOW_ERROR'); }
}