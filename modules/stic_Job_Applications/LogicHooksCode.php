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

class stic_Job_ApplicationsLogicHooks
{
    /**
     * Update counts when an application is created
     *
     * @param SugarBean $bean The stic_Job_Applications bean
     * @param string $event The hook event
     * @param array $arguments Additional arguments
     */
    public function after_save(&$bean, $event, $arguments)
    {
        if (empty($bean->id)) {
            return;
        }

        require_once 'modules/stic_Job_Applications/Utils.php';
        stic_Job_ApplicationsUtils::updateRelatedOffersApplicationsCounts($bean, true);
        stic_Job_ApplicationsUtils::notifyOnStatusChange($bean);
    }

    /**
     * Update counts when an application is deleted
     *
     * @param SugarBean $bean
     * @param string $event
     * @param array $arguments
     */
    public function after_delete(&$bean, $event, $arguments)
    {
        require_once 'modules/stic_Job_Applications/Utils.php';
        stic_Job_ApplicationsUtils::updateRelatedOffersApplicationsCounts($bean, false);
    }

    /**
     * Update counts when a relationship is added
     *
     * @param SugarBean $bean
     * @param string $event
     * @param array $arguments
     */
    public function after_relationship_add(&$bean, $event, $arguments)
    {
        require_once 'modules/stic_Job_Applications/Utils.php';
        if (!stic_Job_ApplicationsUtils::isOfferRelationship($arguments)) {
            return;
        }

        stic_Job_ApplicationsUtils::updateOfferCountsByIds(array($arguments['related_id'] ?? null));
    }

    /**
     * Update counts when a relationship is removed
     *
     * @param SugarBean $bean
     * @param string $event
     * @param array $arguments
     */
    public function after_relationship_delete(&$bean, $event, $arguments)
    {
        require_once 'modules/stic_Job_Applications/Utils.php';
        if (!stic_Job_ApplicationsUtils::isOfferRelationship($arguments)) {
            return;
        }

        stic_Job_ApplicationsUtils::updateOfferCountsByIds(array($arguments['related_id'] ?? null));
    }
}
