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

require_once('include/MVC/Controller/SugarController.php');
#[\AllowDynamicProperties]
class stic_AWF_FormsController extends SugarController
{
    private function _saveBeanFromData($data)
    {
        if (empty($data['bean']['id'])) {
            $bean = BeanFactory::newBean('stic_AWF_Forms');
        } else {
            $bean = BeanFactory::getBean('stic_AWF_Forms', $data['bean']['id']);
        }

        if (!$bean) {
            throw new Exception("Record not found or could not be created");
        }

        // Update all fields in vardef
        foreach ($bean->field_defs as $fieldName => $def) {
            if (isset($data['bean'][$fieldName])) {
                // Skip protected fields
                if (in_array($fieldName, ['id','date_entered','date_modified','deleted'])) {
                    continue;
                }
                $value = $data['bean'][$fieldName];
                if (($fieldName === 'start_date' || $fieldName === 'end_date') && !empty($value)) {
                    global $current_user;
                    // Date is in user's local timezone and frontend format, convert to UTC for DB storage
                    $dateObj = DateTime::createFromFormat('Y-m-d\TH:i', $value, new DateTimeZone($current_user->getPreference('timezone')));
                    if ($dateObj !== false) {
                        $dateObj->setTimezone(new DateTimeZone('UTC'));
                        $value = $dateObj->format('Y-m-d H:i:s');
                    }
                }
                $bean->$fieldName = $value;
            }
        }

        // Update config_json with new config
        if (isset($data['config'])) {
             $bean->configuration = $data['config'];
        }
        
        $bean->save();
        return $bean;
    }

    /**
     * Handles the 'saveDraft' action to Save current configuration in bean
     */
    public function action_saveDraft()
    {
        header('Content-Type: application/json');

        $data = json_decode(file_get_contents('php://input'), true);
        try {
            $bean = $this->_saveBeanFromData($data);
            echo json_encode(['success' => true, 'id' => $bean->id]);
        } catch (Exception $e) {
            echo json_encode(['success' => false, 'message' => $e->getMessage()]);
        }

        sugar_cleanup(true);
    }

    /**
     * Handles the 'finalizeConfig' action to Finish bean edition
     */
    public function action_finalizeConfig()
    {
        header('Content-Type: application/json');

        $data = json_decode(file_get_contents('php://input'), true);
        try {
            $bean = $this->_saveBeanFromData($data);
            $redirectUrl = 'index.php?module=stic_AWF_Forms&action=DetailView&record=' . $bean->id;
            echo json_encode(['success' => true, 'id' => $bean->id, 'redirectUrl' => $redirectUrl]);
        } catch (Exception $e) {
            echo json_encode(['success' => false, 'message' => $e->getMessage()]);
        }
        
        sugar_cleanup(true);
    }

    /**
     * Handles the 'getModuleInformation' action to retrieve module relationships and fields.
     */
    public function action_getModuleInformation()
    {
        // Ensure return json 
        header('Content-Type: application/json');

        require_once "modules/stic_AWF_Forms/Utils.php";
        $result = stic_AWF_FormsUtils::getModuleInformation($_REQUEST['getmodule'], json_decode(html_entity_decode($_REQUEST['getavailablemodules']),true));
        $resultStr = json_encode($result, JSON_UNESCAPED_UNICODE);
        echo $resultStr;

        sugar_cleanup(true);
    }

    /**
     * Handles the 'getRecordsTextById' action to retrieve the text to be shown for a list of Ids of given module 
     */
    public function action_getRecordsTextById() {
        // Ensure return json 
        header('Content-Type: application/json');

        require_once "modules/stic_AWF_Forms/Utils.php";
        $result = stic_AWF_FormsUtils::getRecordsTextById($_REQUEST['reqmodule'], json_decode(html_entity_decode($_REQUEST['reqids']),true));
        $resultStr = json_encode($result, JSON_UNESCAPED_UNICODE);
        echo $resultStr;

        sugar_cleanup(true);
    }


    /**
     * Handles the 'getDefinedActions' action to retrieve all actions defined in the Advanced Web Forms system
     */
    public function action_getDefinedActions()
    {
        // Ensure return json 
        header('Content-Type: application/json');

        require_once "modules/stic_AWF_Forms/core/includes.php";
        $serverActions = ActionDiscoveryService::discoverActions();

        $actionDTOs = [];
        foreach ($serverActions as $actionDef) {
            try {
                $actionDTOs[] = new ActionDefinitionDTO($actionDef);
            } catch (\Throwable $t) {
                $GLOBALS['log']->error("Line ".__LINE__.": ".__METHOD__.": Error processing ActionDefinitionDTO for the action {$actionDef->getName()}: " . $t->getMessage());
            }
        }

        $resultStr = json_encode($actionDTOs, JSON_UNESCAPED_UNICODE);
        echo $resultStr;

        sugar_cleanup(true);
    }
    
    /**
     * Handles the 'getAllPopupIds' action to retrieve all record IDs matching
     * the popup query when "Select All" (entire list) is used in a MultiSelect popup.
     */
    public function action_getAllPopupIds()
    {
        header('Content-Type: application/json');

        if (empty($_REQUEST['current_query_by_page'])) {
            echo json_encode(['success' => false, 'message' => 'Missing current_query_by_page']);
            sugar_cleanup(true);
            return;
        }

        $current_query_by_page_array = json_decode(html_entity_decode($_REQUEST['current_query_by_page']), true);
        if (empty($current_query_by_page_array['module'])) {
            echo json_encode(['success' => false, 'message' => 'Missing module in query data']);
            sugar_cleanup(true);
            return;
        }

        $module = $current_query_by_page_array['module'];
        $seed = BeanFactory::getBean($module);
        if (empty($seed)) {
            echo json_encode(['success' => false, 'message' => 'Invalid module: ' . $module]);
            sugar_cleanup(true);
            return;
        }

        if ($seed->bean_implements('ACL')) {
            if (!ACLController::checkAccess($module, 'list', true)) {
                echo json_encode(['success' => false, 'message' => 'ACL access denied']);
                sugar_cleanup(true);
                return;
            }
        }

        $where = $this->_generatePopupSearchWhere($module, $seed, $current_query_by_page_array);

        $accessWhere = '';
        if ($seed->bean_implements('ACL')) {
            $accessWhere = $seed->buildAccessWhere('list');
        }
        if (!empty($accessWhere)) {
            $where = empty($where) ? $accessWhere : $where . ' AND ' . $accessWhere;
        }

        $table_name = $seed->table_name;
        $sql = "SELECT DISTINCT id FROM {$table_name} WHERE deleted=0";
        if (!empty($where)) {
            $sql .= ' AND ' . $where;
        }

        $result = $GLOBALS['db']->query($sql);
        $ids = array();
        while ($row = $GLOBALS['db']->fetchByAssoc($result)) {
            $ids[] = $row['id'];
        }

        echo json_encode(['success' => true, 'ids' => $ids]);
        sugar_cleanup(true);
    }

    private function _generatePopupSearchWhere($module, $seed, $current_query_by_page_array)
    {
        global $popupMeta;

        $popupdefsFile = null;
        if (file_exists('custom/modules/' . $module . '/metadata/popupdefs.php')) {
            $popupdefsFile = 'custom/modules/' . $module . '/metadata/popupdefs.php';
        } elseif (file_exists('modules/' . $module . '/metadata/popupdefs.php')) {
            $popupdefsFile = 'modules/' . $module . '/metadata/popupdefs.php';
        }

        if (empty($popupdefsFile)) {
            $GLOBALS['log']->warn("Line ".__LINE__.": ".__METHOD__.": No popupdefs found for module {$module}, falling back to MassUpdate");
            require_once('include/MassUpdate.php');
            $mass = new MassUpdate();
            $mass->generateSearchWhere($module, htmlentities(json_encode($current_query_by_page_array)));
            return $mass->where_clauses ?? '';
        }

        require $popupdefsFile;

        if (empty($popupMeta) || empty($popupMeta['searchdefs'])) {
            $GLOBALS['log']->warn("Line ".__LINE__.": ".__METHOD__.": popupdefs for {$module} has no searchdefs, falling back to MassUpdate");
            require_once('include/MassUpdate.php');
            $mass = new MassUpdate();
            $mass->generateSearchWhere($module, htmlentities(json_encode($current_query_by_page_array)));
            return $mass->where_clauses ?? '';
        }

        $popupSearchDefs = array();
        if (is_array($popupMeta['searchdefs'])) {
            $popupSearchDefs[$module]['layout']['advanced_search'] = $popupMeta['searchdefs'];
        } else {
            require_once $popupMeta['searchdefs'];
        }

        $searchFields = array();
        if (file_exists('custom/modules/' . $module . '/metadata/SearchFields.php')) {
            require 'custom/modules/' . $module . '/metadata/SearchFields.php';
        } elseif (file_exists('modules/' . $module . '/metadata/SearchFields.php')) {
            require 'modules/' . $module . '/metadata/SearchFields.php';
        }

        if (empty($searchFields) || empty($popupSearchDefs)) {
            return '';
        }

        require_once('include/SearchForm/SearchForm2.php');
        $searchForm = new SearchForm($seed, $module);
        $searchForm->setup($popupSearchDefs, $searchFields, 'SearchFormGenericAdvanced.tpl', 'advanced_search');
        $searchForm->populateFromArray($current_query_by_page_array, 'advanced_search');

        $where_clauses = $searchForm->generateSearchWhere(true, $module);
        $where = '';
        if ((is_countable($where_clauses) ? count($where_clauses) : 0) > 0) {
            $where = '(' . implode(' AND ', $where_clauses) . ')';
        }

        if (!empty($popupMeta['whereStatement'])) {
            if (!empty($where)) {
                $where .= ' AND ';
            }
            $where .= $popupMeta['whereStatement'];
        }

        return $where;
    }

    private function renderOutput(string $recordId, bool $isPreview, ?array $configData = null)
    {
        require_once 'modules/stic_AWF_Forms/core/FormRenderService.php';

        if (ob_get_length()) { 
            ob_clean(); 
        }
        
        try {
            $service = new FormRenderService();
            echo $service->render($recordId, $isPreview, $configData);
        } catch (Exception $e) {
            $GLOBALS['log']->error("Line ".__LINE__.": ".__METHOD__. "Error rendering form: ". $e->getMessage());
            die($e->getMessage());
        }

        sugar_cleanup(true);
    }

    /**
     * Handles the 'renderForm' action to retrieve the form HTML
     */
    public function action_renderForm() 
    {
        $this->renderOutput($_REQUEST['record'] ?? '', false);
    }

    /**
     * Handles the 'renderPreviewForm' action to retrieve the form HTML in preview mode
     */
    public function action_renderPreviewForm()
    {
        $this->renderOutput($_REQUEST['record'] ?? '', true);
    }

    /**
     * Handles the 'renderPreview' action to retrieve the form HTML from JSON (for Live Preview)
     */
    public function action_renderPreview()
    {
        // Live Preview from sent JSON
        $input = file_get_contents('php://input');
        $data = json_decode($input, true);
        
        if (!$data || !isset($data['config'])) {
            die("Error: No config data received");
        }

        $formId = $data['id'] ?? 'preview'; 
        $configData = json_decode($data['config'], true);

        $this->renderOutput($formId, true, $configData);
    }
}