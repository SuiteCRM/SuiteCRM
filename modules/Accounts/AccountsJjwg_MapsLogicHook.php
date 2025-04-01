<?php

// custom/modules/Accounts/AccountsJjwg_MapsLogicHook.php

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

#[\AllowDynamicProperties]
class AccountsJjwg_MapsLogicHook
{
    public $jjwg_Maps;
    public function __construct()
    {
        $this->jjwg_Maps = get_module_info('jjwg_Maps');
    }

    public function updateGeocodeInfo(&$bean, $event, $arguments)
    {
        // before_save
        if ($this->jjwg_Maps->settings['logic_hooks_enabled']) {
            $this->jjwg_Maps->updateGeocodeInfo($bean);
        }
    }

    public function updateRelatedProjectGeocodeInfo(&$bean, $event, $arguments)
    {
        // after_save
        if ($this->jjwg_Maps->settings['logic_hooks_enabled']) {
            // Find and Update the Related Projects - save() Triggers Logic Hooks
            require_once('modules/Project/Project.php');
            $projects = $bean->get_linked_beans('project', 'Project');
            foreach ($projects as $project) {
                $project->custom_fields->retrieve();
                $this->jjwg_Maps->updateGeocodeInfo($project, true);
                if ($project->jjwg_maps_address_c != $project->fetched_row['jjwg_maps_address_c']) {
                    $project->save(false);
                }
            }
        }
    }

    public function updateRelatedOpportunitiesGeocodeInfo(&$bean, $event, $arguments)
    {
        // after_save
        if ($this->jjwg_Maps->settings['logic_hooks_enabled']) {
            // Find and Update the Related Opportunities - save() Triggers Logic Hooks
            require_once('modules/Opportunities/Opportunity.php');
            $opportunities = $bean->get_linked_beans('opportunities', 'Opportunity');
            foreach ($opportunities as $opportunity) {
                $opportunity->custom_fields->retrieve();
                $this->jjwg_Maps->updateGeocodeInfo($opportunity, true);
                if ($opportunity->jjwg_maps_address_c != $opportunity->fetched_row['jjwg_maps_address_c']) {
                    $opportunity->save(false);
                }
            }
        }
    }

    public function updateRelatedCasesGeocodeInfo(&$bean, $event, $arguments)
    {
        // after_save
        if ($this->jjwg_Maps->settings['logic_hooks_enabled']) {
            // Find and Update the Related Cases - save() Triggers Logic Hooks
            require_once('modules/Cases/Case.php');
            $cases = $bean->get_linked_beans('cases', 'aCase');
            foreach ($cases as $case) {
                $case->custom_fields->retrieve();
                $this->jjwg_Maps->updateGeocodeInfo($case, true);
                if ($case->jjwg_maps_address_c != $case->fetched_row['jjwg_maps_address_c']) {
                    $case->save(false);
                }
            }
        }
    }

    public function updateRelatedMeetingsGeocodeInfo(&$bean, $event, $arguments)
    {
        // after_save
        if ($this->jjwg_Maps->settings['logic_hooks_enabled']) {
            $this->jjwg_Maps->updateRelatedMeetingsGeocodeInfo($bean);
        }
    }

    public function addRelationship(&$bean, $event, $arguments)
    {
        // after_relationship_add
        // $arguments['module'], $arguments['related_module'], $arguments['id'] and $arguments['related_id']
        if ($this->jjwg_Maps->settings['logic_hooks_enabled']) {
            $focus = get_module_info($arguments['module']);
            if (!empty($arguments['id'])) {
                $focus->retrieve($arguments['id']);
                $focus->custom_fields->retrieve();
                $this->jjwg_Maps->updateGeocodeInfo($focus, true);
                if ($focus->jjwg_maps_address_c != $focus->fetched_row['jjwg_maps_address_c']) {
                    $focus->save(false);
                }
            }
        }
    }

    public function deleteRelationship(&$bean, $event, $arguments)
    {
        // after_relationship_delete
        // $arguments['module'], $arguments['related_module'], $arguments['id'] and $arguments['related_id']
        if ($this->jjwg_Maps->settings['logic_hooks_enabled']) {
            $focus = get_module_info($arguments['module']);
            if (!empty($arguments['id'])) {
                $focus->retrieve($arguments['id']);
                $focus->custom_fields->retrieve();
                $this->jjwg_Maps->updateGeocodeInfo($focus, true);
                if ($focus->jjwg_maps_address_c != $focus->fetched_row['jjwg_maps_address_c']) {
                    $focus->save(false);
                }
            }
        }
    }
}
