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

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

class stic_Assessments_ScoresLogicHooks
{
    /**
     * Build a descriptive name for the score when it is not provided.
     *
     * @param stic_Assessments_Scores $bean
     * @param string $event
     * @param array $arguments
     * @return void
     */
    public function before_save(&$bean, $event, $arguments)
    {
        if (!empty($bean->name)) {
            return;
        }

        global $timedate;

        $prefix = translate('LBL_SCORE_NAME_PREFIX', 'stic_Assessments_Scores');
        $score = ($bean->score_value !== null && $bean->score_value !== '') ? $bean->score_value : '0';
        $label = !empty($bean->classification_label) ? ' (' . $bean->classification_label . ')' : '';
        $date = !empty($timedate) ? $timedate->asUser($timedate->now()) : date('Y-m-d H:i');

        $bean->name = $prefix . ': ' . $score . $label . ' - ' . $date;
    }
}
