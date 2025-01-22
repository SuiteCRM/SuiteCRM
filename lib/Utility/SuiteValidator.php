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

namespace SuiteCRM\Utility;

#[\AllowDynamicProperties]
class SuiteValidator
{
    /**
     * @param string|null $id
     * @return bool
     */
    public function isValidId(?string $id): bool
    {
        if (empty($id)) {
            return false;
        }

        $pattern = $this->getIdValidationPattern();

        return is_numeric($id) || (is_string($id) && preg_match($pattern, $id));
    }

    /**
     * @param string|null $key
     * @return bool
     */
    public function isValidKey(?string $key): bool
    {
        if (empty($key)) {
            return false;
        }

        $pattern = $this->getKeyValidationPattern();

        return is_numeric($key) || preg_match($pattern, $key);
    }

    /**
     * @param string $fieldname
     * @return bool
     */
    public function isPercentageField(string $fieldname): bool
    {
        if ($fieldname === 'aos_products_quotes_vat' ||
            strpos(strtolower($fieldname), 'pct') !== false ||
            strpos(strtolower($fieldname), 'percent') !== false ||
            strpos(strtolower($fieldname), 'percentage') !== false) {
            return true;
        }

        return false;
    }

    /**
     * Get id validation pattern
     * @return string
     */
    public function getIdValidationPattern(): string
    {
        global $sugar_config;

        if (isset($sugar_config['strict_id_validation']) && $sugar_config['strict_id_validation']) {
            $pattern = '/^\{?[A-Z0-9]{8}-[A-Z0-9]{4}-[A-Z0-9]{4}-[A-Z0-9]{4}-[A-Z0-9]{12}\}?$/i';
        } else {
            $pattern = get_id_validation_pattern();
        }

        return $pattern;
    }

    /**
     * @return string
     */
    protected function getKeyValidationPattern(): string
    {
        global $sugar_config;

        if (!empty($sugar_config['key_validation_pattern'])) {
            $pattern = $sugar_config['key_validation_pattern'];
        } else {
            $pattern = '/^[A-Z0-9\-\_\.]*$/i';
        }

        return $pattern;
    }
}
