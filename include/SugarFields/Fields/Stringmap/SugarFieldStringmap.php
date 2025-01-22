<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2022 SalesAgility Ltd.
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

require_once 'include/SugarFields/Fields/Text/SugarFieldText.php';

class SugarFieldStringmap extends SugarFieldText
{
    /**
     * @inheritDoc
     */
    function getEditViewSmarty($parentFieldArray, $vardef, $displayParams, $tabindex) {

        $this->calculateShowKeys($vardef);
        $this->setLabels($vardef);


        return parent::getEditViewSmarty($parentFieldArray, $vardef, $displayParams, $tabindex);
    }

    /**
     * @inheritDoc
     */
    public function getDetailViewSmarty($parentFieldArray, $vardef, $displayParams, $tabindex)
    {
        $this->calculateShowKeys($vardef);
        $this->setLabels($vardef);

        return parent::getDetailViewSmarty($parentFieldArray, $vardef, $displayParams, $tabindex);
    }

    /**
     * @inheritDoc
     */
    public function save(&$bean, $params, $field, $properties, $prefix = '')
    {
        parent::save($bean, $params, $field, $properties, $prefix);
        $map = $this->getStringMapFromRequest($field);

        $bean->$field = '';
        if(!empty($map)) {
            $bean->$field = json_encode($map, JSON_THROW_ON_ERROR);
        }
    }

    /**
     * @param array $vardef
     * @return void
     */
    protected function calculateShowKeys(array &$vardef): void
    {
        $showKeys = false;
        if (isTrue($vardef['show_keys'] ?? false)) {
            $showKeys = $vardef['show_keys'];
        }

        $vardef['show_keys'] = $showKeys;
    }

    /**
     * @param $field
     * @return array
     */
    protected function getStringMapFromRequest($field): array
    {
        $requestKeyEntry = $field . '-key';
        $requestValueEntry = $field . '-value';

        $keys = [];
        if (!empty($_REQUEST[$requestKeyEntry]) && is_array($_REQUEST[$requestKeyEntry])) {
            $keys = $_REQUEST[$requestKeyEntry];
        }

        $values = [];
        if (!empty($_REQUEST[$requestValueEntry]) && is_array($_REQUEST[$requestValueEntry])) {
            $values = $_REQUEST[$requestValueEntry];
        }

        $map = [];
        $i = 0;
        foreach ($values as $value) {

            $key = $i;
            if (!empty($keys[$i])) {
                $key = $keys[$i];
            }

            if (empty($keys[$i]) && empty($value)){
                $i++;
                continue;
            }

            $map[$key] = $value;

            $i++;
        }

        return $map;
    }

    /**
     * @param array $vardef
     * @return void
     */
    protected function setLabels(array $vardef): void
    {
        global $app_strings;

        $keyLabelKey = $vardef['key_label'] ?? 'LBL_KEY';
        $valueLabelKey = $vardef['value_label'] ?? 'LBL_VALUE';
        $this->ss->assign('entry_key_label', $app_strings[$keyLabelKey] ?? '');
        $this->ss->assign('entry_value_label', $app_strings[$valueLabelKey] ?? '');
    }


}
