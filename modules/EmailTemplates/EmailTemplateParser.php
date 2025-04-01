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

#[\AllowDynamicProperties]
class EmailTemplateParser
{
    /**
     * Official expression for variables, extended with underscore
     * @see http://php.net/manual/en/language.variables.basics.php
     */
    public const PATTERN = '/\$([a-zA-Z_\x7f-\xff]+_[a-zA-Z0-9_\x7f-\xff]*)/';

    /**
     * Allowed keys as result
     * @var array
     */
    private static $allowedAttributes = ['subject', 'body_html', 'body'];

    /**
     * Allowed Non-DB fields
     * @var array
     */
    private static $allowedVariables = ['survey_url_display'];

    /**
     * @var EmailTemplate
     */
    private $template;

    /**
     * @var Campaign
     */
    private $campaign;

    /**
     * @var EmailInterface
     */
    private $module;

    /**
     * @var string
     */
    private $siteUrl;

    /**
     * @var string
     */
    private $trackerId;

    /**
     * @var null|Surveys
     */
    private $survey = null;

    /**
     * @param EmailTemplate $template
     * @param Campaign $campaign
     * @param EmailInterface $module
     * @param string $siteUrl
     * @param string $trackerId
     */
    public function __construct(
        EmailTemplate $template,
        Campaign $campaign,
        EmailInterface $module,
        $siteUrl,
        $trackerId
    ) {
        $this->template = $template;
        $this->campaign = $campaign;
        $this->module = $module;
        $this->siteUrl = $siteUrl;
        $this->trackerId = $trackerId;
    }

    /**
     * @return array
     */
    public function parseVariables()
    {
        $templateData = [];

        foreach (static::$allowedAttributes as $key) {
            if (property_exists($this->template, $key)) {
                $templateData[$key] = $this->getParsedValue($this->template->$key);
            }
        }

        return $templateData;
    }

    /**
     * @param string $attributeValue
     *
     * @return string
     */
    private function getParsedValue($attributeValue)
    {
        $matches = preg_match_all(static::PATTERN, $attributeValue, $variables);

        if ($matches !== 0) {
            foreach ($variables[0] as $variable) {
                $attributeValue = str_replace($variable, $this->getValueFromBean($variable), $attributeValue);
            }
        }

        return $attributeValue;
    }

    /**
     * This is need to be extended properly and replace the method below in the future
     * @see EmailTemplate::parse_email_template
     *
     * @param string $variable
     *
     * @return string
     */
    private function getValueFromBean($variable)
    {
        global $app_list_strings;

        $charVariable = chr(36);
        $charUnderscore = chr(95);

        if (strpos($variable, $charVariable) === false || strpos($variable, $charUnderscore) === false) {
            $GLOBALS['log']->warn(sprintf(
                'Variable %s parsed to an empty string, because attribute has no %s or %s character',
                $variable,
                $charVariable,
                $charUnderscore
            ));
            return '';
        }

        $parts = explode($charUnderscore, ltrim($variable, $charVariable));
        list($moduleName, $attribute) = [array_shift($parts), implode($charUnderscore, $parts)];

        /* Leads/Prospects/Users have a special variable naming scheme.
        $contact_xxx for leads/prospects and $contact_user_xxx for users */
        if (strtolower($moduleName) === 'contact') {
            if (in_array($this->module->object_name, ['Lead', 'Prospect'], true)) {
                $moduleName = strtolower($this->module->object_name);
            } else if ($this->module->object_name == 'User' && str_begin(strtolower($attribute), 'user_')) {
                $attribute = explode('_', $attribute, 2)[1];
                $moduleName = 'user';
            }
        }

        if (in_array($attribute, static::$allowedVariables, true)) {
            return $this->getNonDBVariableValue($attribute);
        }

        if ($this->module instanceof $moduleName && property_exists($this->module, $attribute)) {
            if (isset($this->module->field_name_map[$attribute]['type']) && ($this->module->field_name_map[$attribute]['type']) === 'enum') {
                $enum = $this->module->field_name_map[$attribute]['options'];
                if (isset($app_list_strings[$enum][$this->module->$attribute])) {
                    $this->module->$attribute = $app_list_strings[$enum][$this->module->$attribute];
                }
            } else if (($this->module->field_name_map[$attribute]['type']) && (($this->module->field_name_map[$attribute]['type']) === 'decimal' || ($this->module->field_name_map[$attribute]['type']) === 'float')) {
                $value = formatDecimalInConfigSettings($this->module->$attribute, false);
                return $value;
            }
            return $this->module->$attribute;
        }

        $GLOBALS['log']->warn(sprintf(
            'Variable %s parsed to an empty string, because attribute %s is not set in %s bean',
            $variable,
            $attribute,
            get_class($this->module)
        ));
        return '';
    }

    /**
     * @return Surveys
     */
    public function getSurvey()
    {
        if ($this->survey === null) {
            $this->survey = \BeanFactory::getBean('Surveys', $this->campaign->survey_id);
        }

        return $this->survey;
    }

    /**
     * This one will be removed once dynamic fields will be fixed
     * @param string $attribute
     *
     * @return string
     */
    private function getNonDBVariableValue($attribute)
    {
        $value = '';

        if ($attribute === 'survey_url_display' && $this->module instanceof Person) {
            /** @var Contact $contact */
            $contact = $this->module;
            $value = sprintf(
                '%s/index.php?entryPoint=survey&id=%s&contact=%s&tracker=%s',
                $this->siteUrl,
                $this->getSurvey()->id,
                $contact->id,
                $this->trackerId !== null ? $this->trackerId : ''
            );
        }

        return $value;
    }
}
