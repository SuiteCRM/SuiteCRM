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
 *
 * This file was contributed by diligent technology & business consulting GmbH <info@dtbc.eu>
 *
 */

use SuiteCRM\Utility\SuiteValidator;

require_once('modules/AOW_Actions/actions/actionBase.php');

/**
 * Class actionComputeField
 */
class actionComputeField extends actionBase
{
    public const RAW_VALUE = "raw";
    public const FORMATTED_VALUE = "formatted";

    /**
     * @return array
     */
    public function loadJS()
    {
        return array('modules/AOW_Actions/actions/actionComputeField.js');
    }

    /**
     * @param SugarBean $bean
     * @param array $params
     * @param bool $in_save
     *
     * @return bool|void
     */
    public function run_action(SugarBean $bean, $params = array(), $in_save = false)
    {
        try {
            require_once('modules/AOW_Actions/FormulaCalculator.php');

            $bean->fill_in_additional_detail_fields();

            $resolvedParameters = $this->resolveParameters(
                $bean,
                $this->getArrayFromParams($params, 'parameter'),
                $this->getArrayFromParams($params, 'parameterType')
            );
            $resolvedRelationParameters = $this->resolveRelationParameters(
                $bean,
                $this->getArrayFromParams($params, 'relationParameter'),
                $this->getArrayFromParams($params, 'relationParameterField'),
                $this->getArrayFromParams($params, 'relationParameterType')
            );

            $formulas = $this->getArrayFromParams($params, 'formula');
            $formulaContents = $this->getArrayFromParams($params, 'formulaContent');

            if (count($formulas) == 0) {
                return;
            }

            $calculator = new FormulaCalculator(
                $resolvedParameters,
                $resolvedRelationParameters,
                $bean->module_name,
                $bean->created_by
            );

            $relateFields = $this->getAllRelatedFields($bean);
            $formulasCount = is_countable($formulas) ? count($formulas) : 0;

             for ($i = 0; $i < $formulasCount; $i++) {
                if (array_key_exists($formulas[$i], $relateFields) && isset($relateFields[$formulas[$i]]['id_name'])) {
                    $calcValue = $calculator->calculateFormula($formulaContents[$i]);
                    $bean->{$relateFields[$formulas[$i]]['id_name']} = ( is_numeric($calcValue) ? (float)$calcValue : $calcValue );
                } else {
                    $calcValue = $calculator->calculateFormula($formulaContents[$i]);
                    $bean->{$formulas[$i]} = ( is_numeric($calcValue) ? (float)$calcValue : $calcValue );
                }
            }

            if ($in_save) {
                global $current_user;
                $bean->processed = true;
                $check_notify =
                    $bean->assigned_user_id != $current_user->id &&
                    $bean->assigned_user_id != $bean->fetched_row['assigned_user_id'];
            } else {
                $check_notify = $bean->assigned_user_id != $bean->fetched_row['assigned_user_id'];
            }

            $bean->process_save_dates = false;
            $bean->new_with_id = false;

            $bean->save($check_notify);

            return true;
        } catch (Exception $e) {
            if (isset($GLOBALS['log'])) {
                $GLOBALS['log']->fatal("Calculated Field Exception: " . $e->getMessage());
            }

            return false;
        }
    }

    /**
     * @param $bean
     * @param $parameters
     * @param $parameterTypes
     *
     * @return array
     */
    private function resolveParameters($bean, $parameters, $parameterTypes)
    {
        $resolvedParameters = array();
        $parametersCount = is_countable($parameters) ? count($parameters) : 0;

        for ($i = 0; $i < $parametersCount; $i++) {
            if ($parameterTypes[$i] == actionComputeField::FORMATTED_VALUE) {
                $dataType = $bean->field_name_map[$parameters[$i]]['type'];

                if ($dataType == 'enum' || $dataType == 'dynamicenum') {
                    $resolvedParameters[$i] =
                        $GLOBALS['app_list_strings'][$bean->field_defs[$parameters[$i]]['options']][$bean->{$parameters[$i]}];
                } else {
                    if ($dataType == 'multienum') {
                        $resolvedParameters[$i] = $this->getMultiEnumTranslated($bean, $parameters[$i]);
                    }
                }
            } else {
                $resolvedParameters[$i] = ($bean->{$parameters[$i]} == null) ? "" : $bean->{$parameters[$i]};
            }
        }

        return $resolvedParameters;
    }

    /**
     * @param $bean
     * @param $fieldName
     *
     * @return string
     */
    public function getMultiEnumTranslated($bean, $fieldName)
    {
        $displayFieldValues = unencodeMultienum($bean->$fieldName);

        array_walk(
            $displayFieldValues,
            function (&$val) use ($bean, $fieldName) {
                $val = $GLOBALS['app_list_strings'][$bean->field_defs[$fieldName]['options']][$val];
            }
        );

        return implode(", ", $displayFieldValues);
    }

    /**
     * @param $params
     * @param $key
     *
     * @return array
     */
    private function getArrayFromParams($params, $key)
    {
        $elements = array();

        if (!isset($params[$key])) {
            return $elements;
        }

        foreach ($params[$key] as $field) {
            $elements [] = $field;
        }

        return $elements;
    }

    /**
     * @param SugarBean $bean
     * @param $relationParameters
     * @param $relationParameterFields
     * @param $relationParameterTypes
     *
     * @return array
     */
    private function resolveRelationParameters(
        $bean,
        $relationParameters,
        $relationParameterFields,
        $relationParameterTypes
    ) {
        $isValidator = new SuiteValidator();
        $resolvedRelationParameters = array();

        $relateFields = $this->getAllRelatedFields($bean);
        $relationParametersCount = is_countable($relateFields) ? count($relationParameters) : 0;

        for ($i = 0; $i < $relationParametersCount; $i++) {
            $entity = null;

            if (isset($relateFields[$relationParameters[$i]]) &&
                $relateFields[$relationParameters[$i]]['type'] == 'relate'
            ) {
                $relatedEntityId = $bean->{$relateFields[$relationParameters[$i]]['id_name']};

                if (!$relatedEntityId) {
                    $resolvedRelationParameters[$i] = "";
                    continue;
                }

                if (is_object($relatedEntityId)) {
                    // If this is a Link2 object then need to use the relationship
                    // - because it's a one to many relationship's 'one' side
                    $relationship = $relateFields[$relationParameters[$i]]['link'];
                    if ($bean->load_relationship($relationship)) {
                        foreach ($bean->$relationship->getBeans() as $relatedEntity) {
                            $entity = $relatedEntity;
                            break;
                        }
                    }
                } elseif ($isValidator->isValidId($relatedEntityId)) {
                    // If this is a string, it's probably an id of an object - really a relate field
                    $entity = BeanFactory::getBean(
                        $relateFields[$relationParameters[$i]]['module'],
                        $relatedEntityId
                    );
                } else {
                    // Skip if not recognized
                    $resolvedRelationParameters[$i] = '';
                    continue;
                }
            } else {
                if ($bean->load_relationship($relationParameters[$i])) {
                    foreach ($bean->{$relationParameters[$i]}->getBeans() as $relatedEntity) {
                        $entity = $relatedEntity;
                    }
                }
            }

            if ($entity == null) {
                $resolvedRelationParameters[$i] = "";
                continue;
            }

            if ($relationParameterTypes[$i] == actionComputeField::FORMATTED_VALUE) {
                $dataType = $entity->field_name_map[$relationParameterFields[$i]]['type'];

                if ($dataType == 'enum' || $dataType == 'dynamicenum') {
                    $resolvedRelationParameters[$i] =
                        $GLOBALS['app_list_strings'][$entity->field_defs[$relationParameterFields[$i]]['options']][$entity->{$relationParameterFields[$i]}];
                } else {
                    if ($dataType == 'multienum') {
                        $resolvedRelationParameters[$i] =
                            $this->getMultiEnumTranslated($entity, $relationParameterFields[$i]);
                    }
                }
            } else {
                if ($entity->field_name_map[$relationParameterFields[$i]]['type'] == 'relate' &&
                    isset($entity->field_name_map[$relationParameterFields[$i]]['id_name'])
                ) {
                    $id_name = $entity->field_name_map[$relationParameterFields[$i]]['id_name'];
                    $resolvedRelationParameters[$i] = ($entity->{$id_name} == null) ? "" : $entity->{$id_name};
                } else {
                    $resolvedRelationParameters[$i] =
                        ($entity->{$relationParameterFields[$i]} == null) ? "" :
                            $entity->{$relationParameterFields[$i]};
                }
            }
        }

        return $resolvedRelationParameters;
    }

    /**
     * @param $bean
     *
     * @return array
     */
    private function getAllRelatedFields($bean)
    {
        $related_fields = array();

        foreach ($bean->field_defs as $field => $defs) {
            if (isset($defs['type']) && $defs['type'] == 'relate') {
                $related_fields[$field] = $defs;
            }
        }

        return $related_fields;
    }

    /**
     * @param $line
     * @param SugarBean|null $bean
     * @param array $params
     *
     * @return string
     */
    public function edit_display($line, SugarBean $bean = null, $params = array())
    {
        require_once("modules/AOW_WorkFlow/aow_utils.php");

        $containerName = "computeFieldParameterContainerLine" . $line;

        $moduleFieldsDropDown = $this->getModuleFieldsDropdown($bean);
        $relationOptions = $this->getOneRelationOptions($bean, $line);

        $html = "
			<link rel='stylesheet' type='text/css' href='modules/AOW_Actions/actions/actionComputeField.css' />
			<div class='computeFieldContainer' id='$containerName'>
				<fieldset>
					<legend>" .
            translate("LBL_COMPUTEFIELD_PARAMETERS", "AOW_Actions") .
            "</legend>
					<div class='computeFieldParametersContainer'>
						<table style='display: none;'>
							<thead>
								<tr>
									<th></th>
									<th>" .
            translate("LBL_COMPUTEFIELD_FIELD_NAME", "AOW_Actions") .
            "</th>
									<th>" .
            translate("LBL_COMPUTEFIELD_VALUE_TYPE", "AOW_Actions") .
            "</th>
									<th>" .
            translate("LBL_COMPUTEFIELD_IDENTIFIER", "AOW_Actions") .
            "</th>
								</tr>
							</thead>
							<tbody>
							</tbody>
						</table>						
						<select class='parameterSelect' id='parameterSelect$line'>
							$moduleFieldsDropDown
						</select>
						<select class='parameterTypeSelect' id='parameterTypeSelect$line'>
							<option value='raw'>" .
            translate("LBL_COMPUTEFIELD_RAW_VALUE", "AOW_Actions") .
            "</option>
							<option value='formatted'>" .
            translate("LBL_COMPUTEFIELD_FORMATTED_VALUE", "AOW_Actions") .
            "</option>
						</select>
						<input type='button' class='button' onclick='addParameter($line, \"$containerName\");' value='" .
            translate("LBL_COMPUTEFIELD_ADD_PARAMETER", "AOW_Actions") .
            "' />
					</div>
				</fieldset>
				<fieldset>
					<legend>" .
            translate("LBL_COMPUTEFIELD_RELATION_PARAMETERS", "AOW_Actions") .
            "</legend>
					<div class='computeFieldRelationParametersContainer'>
						<table style='display: none;'>
							<thead>
								<tr>
									<th></th>
									<th>" .
            translate("LBL_COMPUTEFIELD_RELATION", "AOW_Actions") .
            "</th>
									<th>" .
            translate("LBL_COMPUTEFIELD_FIELD_NAME", "AOW_Actions") .
            "</th>
									<th>" .
            translate("LBL_COMPUTEFIELD_VALUE_TYPE", "AOW_Actions") .
            "</th>
									<th>" .
            translate("LBL_COMPUTEFIELD_IDENTIFIER", "AOW_Actions") .
            "</th>
								</tr>
							</thead>
							<tbody>
							</tbody>
						</table>						
						$relationOptions
						<select class='relationParameterTypeSelect' id='relationParameterTypeSelect$line'>
							<option value='raw'>" .
            translate("LBL_COMPUTEFIELD_RAW_VALUE", "AOW_Actions") .
            "</option>
							<option value='formatted'>" .
            translate("LBL_COMPUTEFIELD_FORMATTED_VALUE", "AOW_Actions") .
            "</option>
						</select>
						<input type='button' class='button' onclick='addRelationParameter($line, \"$containerName\");' value='" .
            translate("LBL_COMPUTEFIELD_ADD_RELATION_PARAMETER", "AOW_Actions") .
            "' />
					</div>
				</fieldset>
				<fieldset>
					<legend>" .
            translate("LBL_COMPUTEFIELD_FORMULAS", "AOW_Actions") .
            "</legend>
					<div class='computeFieldFormulaContainer'>
						<table style='display: none;'>
							<thead>
								<tr>
									<th></th>
									<th>" .
            translate("LBL_COMPUTEFIELD_FIELD_NAME", "AOW_Actions") .
            "</th>
									<th>" .
            translate("LBL_COMPUTEFIELD_FORMULA", "AOW_Actions") .
            "</th>
								</tr>
							</thead>
							<tbody>
							</tbody>
						</table>						
						<select>
							$moduleFieldsDropDown
						</select>
						<input type='button' class='button' onclick='addFormula($line, \"$containerName\");' value='" .
            translate("LBL_COMPUTEFIELD_ADD_FORMULA", "AOW_Actions") .
            "' />
					</div>
				</fieldset>";

            $parameters = $this->createJavascriptArrayFromParams($params, 'parameter');
            $parameterTypes = $this->createJavascriptArrayFromParams($params, 'parameterType');
            $formulas = $this->createJavascriptArrayFromParams($params, 'formula');
            $formulaContents = $this->createJavascriptArrayFromParams($params, 'formulaContent');
            $relationParameters = $this->createJavascriptArrayFromParams($params, 'relationParameter');
            $relationParameterFields = $this->createJavascriptArrayFromParams($params, 'relationParameterField');
            $relationParameterTypes = $this->createJavascriptArrayFromParams($params, 'relationParameterType');


            $html .= "
				<script id ='aow_script$line' type='text/javascript'>
					computers[$line] = new FieldComputer($line, '$containerName', $parameters, $parameterTypes, $formulas, $formulaContents, $relationParameters, $relationParameterFields, $relationParameterTypes);
					
					function onRelationParameterSelectChange$line() {
						$('#$containerName').find('.relationParameterFieldSelect').hide();
						$('#$containerName').find('select.relationParameterFieldSelect[relation=\"' + $(this).val() + '\"]').show();
					}
					
					$('#relationParameterSelect$line').change(onRelationParameterSelectChange$line);					
					$('#relationParameterSelect$line').change();
					
					function onFieldChange$line(dropdown, valueDropdown) {
                        var value = $(dropdown).find('option:selected').attr('dataType');						
						if (value == 'enum' || value == 'multienum' || value == 'dynamicenum') {
							$(valueDropdown).show();
						} else {
							$(valueDropdown).hide();
						}
					}
					
					$('#$containerName .computeFieldParametersContainer').find('.parameterSelect').change(function () {
						onFieldChange$line(this, $('#$containerName .computeFieldParametersContainer').find('.parameterTypeSelect'));
					});
					
					$('#$containerName .computeFieldRelationParametersContainer').find('.relationParameterFieldSelect').change(function () {
						onFieldChange$line(this, $('#$containerName .computeFieldRelationParametersContainer').find('.relationParameterTypeSelect'));
					});
					
					$('#$containerName .computeFieldParametersContainer').find('.parameterSelect').change();
					$('#$containerName .computeFieldRelationParametersContainer').find('.relationParameterFieldSelect:visible').change();
				</script>
			</div>";

        return $html;
    }

    /**
     * @param $bean
     *
     * @return string
     */
    public function getModuleFieldsDropdown($bean)
    {
        $moduleFields = json_decode((string) getModuleFields($bean->module_name, "JSON"), true);
        $optionsString = "";

        foreach ($moduleFields as $key => $value) {
            if ($key == "") {
                continue;
            }

            foreach ($bean->field_name_map as $mapKey => $mapValue) {
                if ($key != $mapKey) {
                    continue;
                }

                $dataType = $mapValue['type'];
                $optionsString .= "<option value='$key' dataType='$dataType'>$value</option>";

                break;
            }
        }

        return $optionsString;
    }

    /**
     * @param $bean
     * @param $line
     *
     * @return string
     */
    private function getOneRelationOptions($bean, $line)
    {
        $oneRelations = $this->getOneRelations($bean);
        $relateFields = $this->getAllRelatedFields($bean);
        $fieldSelects = "";
        $alreadyAddedLinks = array();
        $optionsArray = array();

        foreach ($oneRelations as $oneRelation) {
            if (isset($oneRelation['name']) && in_array($oneRelation['name'], $alreadyAddedLinks)) {
                continue;
            }

            if (!$bean->load_relationship($oneRelation['name'])) {
                continue;
            }

            $def = $this->getRelationDefinitions($bean, $oneRelation['name']);
            $oppositeModule = $this->getOppositeModule($bean, $def);

            $optionsArray [] = array(
                'value' => $oneRelation['name'],
                'module' => translate($oppositeModule),
                'relation' => $this->translate($oneRelation['vname'], $bean->module_name, $oppositeModule)
            );

            $fieldSelects .= $this->getRelationParameterFieldSelect($oneRelation['name'], $oppositeModule, $line);
            $alreadyAddedLinks [] = $oneRelation['name'];
        }

        foreach ($relateFields as $name => $relateField) {
            if (isset($relateField['link']) && in_array($relateField['link'], $alreadyAddedLinks)) {
                continue;
            }

            if (isset($relateField['group']) &&
                in_array($relateFields[$relateField['group']]['link'], $alreadyAddedLinks)
            ) {
                continue;
            }

            if (isset($relateField['link_type']) && $relateField['link_type'] == "relationship_info") {
                continue;
            }

            $optionsArray [] = array(
                'value' => $name,
                'module' => $this->translate($relateField['module']),
                'relation' => $this->translate($relateField['vname'], $bean->module_name, $relateField['module'])
            );

            $fieldSelects .= $this->getRelationParameterFieldSelect($name, $relateField['module'], $line);

            if (isset($relateField['link'])) {
                $alreadyAddedLinks [] = $relateField['link'];
            }
        }

        $options = $this->createOptions($optionsArray);

        return "<select id='relationParameterSelect$line' class='relationParameterSelect'>$options</select>" .
            $fieldSelects;
    }

    /**
     * @param $bean
     *
     * @return array
     */
    private function getOneRelations($bean)
    {
        $linkedFields = $bean->get_linked_fields();
        $oneRelations = array();

        foreach ($linkedFields as $key => $value) {
            if (!isset($value['link_type']) || $value['link_type'] != "one") {
                continue;
            }

            $oneRelations [] = $value;
        }

        return $oneRelations;
    }

    /**
     * @param $bean
     * @param $relationName
     *
     * @return mixed
     */
    private function getRelationDefinitions($bean, $relationName)
    {
        return $bean->{$relationName}->relationship->def;
    }

    /**
     * @param $bean
     * @param $def
     *
     * @return mixed
     */
    private function getOppositeModule($bean, $def)
    {
        return $def['lhs_module'] == $bean->module_name ? $def['rhs_module'] : $def['lhs_module'];
    }

    /**
     * @param $label
     * @param string $module
     * @param string $otherModule
     *
     * @return mixed|string
     */
    private function translate($label, $module = "", $otherModule = "")
    {
        $translated = translate($label);
        if ($translated != $label) {
            return $translated;
        }

        $translated = translate($label, $module);
        if ($translated != $label) {
            return $translated;
        }

        return translate($label, $otherModule);
    }

    /**
     * @param $relationName
     * @param $moduleName
     * @param $line
     *
     * @return string
     */
    private function getRelationParameterFieldSelect($relationName, $moduleName, $line)
    {
        global $beanList;

        $bean = new $beanList[$moduleName]();
        $fields = $this->getModuleFieldsDropdown($bean);

        return "<select class='relationParameterFieldSelect' relation='$relationName' style='display:none;'>$fields</select>";
    }

    /**
     * @param $optionsArray
     *
     * @return string
     */
    private function createOptions($optionsArray)
    {
        $options = "";

        usort(
            $optionsArray,
            function ($item1, $item2) {
                $compareString1 = $item1['module'] . ' : ' . $item1['relation'];
                $compareString2 = $item2['module'] . ' : ' . $item2['relation'];

                if ($compareString1 === $compareString2) {
                    return 0;
                }

                return $compareString1 < $compareString2 ? -1 : 1;
            }
        );

        foreach ($optionsArray as $option) {
            $options .= "<option value='" .
                $option['value'] .
                "'>" .
                $option['module'] .
                ' : ' .
                $option['relation'] .
                "</option>";
        }

        return $options;
    }

    /**
     * @param $params
     * @param $key
     *
     * @return string
     */
    private function createJavascriptArrayFromParams($params, $key)
    {
        $paramArray = $this->getArrayFromParams($params, $key);

        return count($paramArray) == 0 ? "[]" : "['" . implode("', '", $paramArray) . "']";
    }

    /**
     * @param $relationName
     * @param $oppositeModule
     *
     * @return string
     */
    private function getOption($relationName, $oppositeModule)
    {
        $oneRelation = [];

        return "<option value='" .
            $oneRelation['name'] .
            "'>" .
            translate($oppositeModule) .
            ' : ' .
            translate($oneRelation['vname']) .
            "</option>";
    }

    /**
     * @param $relationship_name
     * @param $module
     *
     * @return string
     */
    private function getOtherModuleForRelationship($relationship_name, $module)
    {
        $db = DBManagerFactory::getInstance();

        $query =
            "SELECT relationship_name, rhs_module, lhs_module FROM relationships WHERE deleted=0 AND relationship_name = '" .
            $relationship_name .
            "'";
        $result = $db->query($query, true);
        $row = $db->fetchByAssoc($result);

        if ($row != null) {
            if (strtolower($row['rhs_module']) === strtolower($module)) {
                return $row['lhs_module'];
            }

            if (strtolower($row['lhs_module']) === strtolower($module)) {
                return $row['rhs_module'];
            }
        }

        return "";
    }
}
