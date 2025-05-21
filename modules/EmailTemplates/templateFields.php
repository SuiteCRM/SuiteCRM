<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}


function generateFieldDefsJS2()
{
    global $app_list_strings, $beanList, $beanFiles;


    $badFields = array(
        'account_description',
        'contact_id',
        'lead_id',
        'opportunity_amount',
        'opportunity_id',
        'opportunity_name',
        'opportunity_role_id',
        'opportunity_role_fields',
        'opportunity_role',
        'campaign_id',
        // User objects
        'id',
        'user_preferences',
        'accept_status',
        'user_hash',
        'authenticate_id',
        'sugar_login',
        'reports_to_id',
        'reports_to_name',
        'is_admin',
        'receive_notifications',
        'modified_user_id',
        'modified_by_name',
        'created_by',
        'created_by_name',
        'accept_status_id',
        'accept_status_name',
    );

    $loopControl = array();
    $prefixes = array();

    foreach ($app_list_strings['moduleList'] as $key => $name) {
        if (isset($beanList[$key]) && isset($beanFiles[$beanList[$key]]) && !str_begin($key, 'AOW_')) {
            require_once($beanFiles[$beanList[$key]]);
            $focus = new $beanList[$key];
            $loopControl[$key][$key] = $focus;
            $prefixes[$key] = strtolower($focus->object_name) . '_';
            if ($focus->object_name == 'Case') {
                $prefixes[$key] = 'a' . strtolower($focus->object_name) . '_';
            }
        }
    }

    $contact = BeanFactory::newBean('Contacts');
    $lead = BeanFactory::newBean('Leads');
    $prospect = BeanFactory::newBean('Prospects');

    $loopControl['Contacts'] = array(
        'Contacts' => $contact,
        'Leads' => $lead,
        'Prospects' => $prospect,
    );

    $prefixes['Users'] = 'contact_user_';


    $collection = array();
    foreach ($loopControl as $collectionKey => $beans) {
        $collection[$collectionKey] = array();
        foreach ($beans as $beankey => $bean) {
            foreach ($bean->field_defs as $key => $field_def) {
                if (    /*($field_def['type'] == 'relate' && empty($field_def['custom_type'])) ||*/
                    ($field_def['type'] == 'assigned_user_name' || $field_def['type'] == 'link') ||
                    ($field_def['type'] == 'bool') ||
                    (in_array($field_def['name'], $badFields))
                ) {
                    continue;
                }
                if (!isset($field_def['vname'])) {
                    //echo $key;
                }
                // valid def found, process
                $optionKey = strtolower("{$prefixes[$collectionKey]}{$key}");
                if (isset($field_def['vname'])) {
                    $optionLabel = preg_replace('/:$/', "", (string) translate($field_def['vname'], $beankey));
                } else {
                    $optionLabel = preg_replace('/:$/', "", (string) $field_def['name']);
                }
                $dup = 1;
                foreach ($collection[$collectionKey] as $value) {
                    if ($value['name'] === $optionKey) {
                        $dup = 0;
                        break;
                    }
                }
                if ($dup) {
                    $collection[$collectionKey][] = array("name" => $optionKey, "value" => $optionLabel);
                }
            }
        }
    }

    $json = getJSONobj();
    $ret = "var field_defs = ";
    $ret .= $json->encode($collection, false);
    $ret .= ";";
    return $ret;
}

function genDropDownJS2()
{
    global $app_list_strings, $beanList, $beanFiles;

    $lblContactAndOthers = implode('/', array(
        isset($app_list_strings['moduleListSingular']['Contacts']) ? $app_list_strings['moduleListSingular']['Contacts'] : 'Contact',
        isset($app_list_strings['moduleListSingular']['Leads']) ? $app_list_strings['moduleListSingular']['Leads'] : 'Lead',
        isset($app_list_strings['moduleListSingular']['Prospects']) ? $app_list_strings['moduleListSingular']['Prospects'] : 'Target',
    ));

    $dropdown = '';

    array_multisort($app_list_strings['moduleList'], SORT_ASC, $app_list_strings['moduleList']);

    foreach ($app_list_strings['moduleList'] as $key => $name) {
        if (isset($beanList[$key]) && isset($beanFiles[$beanList[$key]]) && !str_begin($key, 'AOW_') && !str_begin($key, 'zr2_')) {
            if ($key == 'Contacts') {
                $dropdown .= "<option value='" . $key . "'>
						" . $lblContactAndOthers . "
		  	       </option>";
            } else {
                if (isset($app_list_strings['moduleListSingular'][$key])) {
                    $dropdown .= "<option value='" . $key . "'>
						" . $app_list_strings['moduleListSingular'][$key] . "
		  	       </option>";
                } else {
                    $dropdown .= "<option value='" . $key . "'>
						" . $app_list_strings['moduleList'][$key] . "
		  	       </option>";
                }
            }
        }
    }


    return $dropdown;
}
